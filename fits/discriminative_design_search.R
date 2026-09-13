# Exploratory: can we MUTATE an experimental design (a trial sequence) to be
# more discriminative between theories, holding models' parameters fixed at
# their current best (group) fit?
#
# This is a prototype for the idea raised in the paper's own Discussion
# ("...use models to generate predictions during experiment design, in
# order to choose more informative experiments") and directly extends
# appendix.Rmd's "Model disagreement" index: instead of just measuring which
# EXISTING conditions are most discriminative, search the space of possible
# trial sequences for ones that would be MORE discriminative than anything
# in the current dataset.
#
# Representation -----------------------------------------------------------
# Every condition in xsl_datasets has words/trial == objects/trial: a trial
# is simply a k-subset of word/object indices {1..V} that co-occur (the
# within-trial ambiguity IS the co-occurrence -- a model can't yet tell
# which of the k co-present objects is which of the k spoken words' true
# referent, only that word i's true referent is always object i). So a
# design is just: T trials, each a k-subset of {1..V}, i.e. a "genome" of
# T integer vectors. This is a MUCH smaller space than free-form
# word-versus-object trial construction, and is exactly the space every
# existing xsl_datasets condition already lives in.
#
# Search ---------------------------------------------------------------
# Simulated annealing: pick a random trial, replace one of its members with
# a value not currently in that trial, and recompute the objective; accept
# improving moves always, accept worsening moves with probability
# exp(delta / temperature) (temperature decays geometrically over the run),
# so the search can escape local optima instead of getting stuck at the
# first hill it climbs. Rejects any mutation that would drop an index out
# of the design entirely (every word must appear at least once, or its
# performance is undefined). Track the best genome seen at any point,
# regardless of the annealing schedule's current acceptance.
#
# FIX (2026-09-13, in response to the first run's null result on the
# already-good seed): the search-time objective is now computed on the 6
# DETERMINISTIC models in the panel only (no Monte Carlo noise at all), so
# every accepted improvement during the search is a real one, not a lucky
# draw from a stochastic model's small-n_sim estimate. The 4 stochastic
# models (PbV, GnT, Pursuit, RL) are reintroduced only for the final
# verification of seed vs. best-found design, at full n_sim=200 -- which is
# also where the number actually reported/compared against the appendix's
# 10-model metric comes from. Because deterministic-only evaluation is
# ~25x cheaper (~9ms vs. ~230ms per candidate), the iteration budget is
# also increased substantially (15,000 vs. the original run's 1,200).
#
# Objective -----------------------------------------------------------
# Same "model disagreement" index as appendix.Rmd's "Model disagreement"
# section: mean over words of the SD of ModelPerf across a fixed panel of
# one-per-mechanism models (see mech_reps below), each run ONCE at its
# already-fit group-fit parameters (xsl_model_registry fits are NOT re-run
# here -- the whole point is "more discriminative for the theories as they
# currently stand", not moving-target parameters). The search itself
# optimizes the deterministic-subset version of this (det_reps); reported
# scores are the full 10-model version, matching the appendix.
#
# Two seeds are searched: the current MOST discriminative real condition
# (filt9E_9L) -- can we beat the best natural design? -- and the current
# LEAST discriminative real condition of comparable size (filt0E_9L) -- can
# a bad design be rescued into a good one with the same amount of data
# collection (same V, T, k)?
#
# Output: fits/discriminative_design_search.RData with, for each seed,
# the seed's genome/score and the best-found genome/score, plus a per-word
# breakdown for the winner.

library(XSLmodels)
library(purrr)
library(here)

source(here("fits/model_specs.R"))       # model_specs
source(here("fits/new_model_specs.R"))   # new_model_specs (unused directly, kept for parity)

set.seed(2026)

## ---------------------------------------------------------------------------
## Model panel: one representative per distinct mechanism, at group-fit params
## (same panel as appendix.Rmd's "Model disagreement" section)
## ---------------------------------------------------------------------------

group_fits <- local({
  e1 <- new.env(); load(here("fits/group_fits_all_experiments.Rdata"), envir = e1)
  e1$group_fits_all
})

mech_reps <- c("kachergis", "fazly", "Bayesian_decay", "rescorla-wagner", "trueswell2012",
               "guess-and-test", "pursuit", "softmax_rl", "kalman_filter", "baseline")
# the deterministic subset of mech_reps -- used for the (noise-free) search
# objective; the full panel above is used only for final verification
det_reps <- c("kachergis", "fazly", "Bayesian_decay", "rescorla-wagner", "kalman_filter", "baseline")

specs <- c(model_specs, list(baseline = list(constructor = function(p) baseline(), stochastic = FALSE)))
# softmax_rl/kalman_filter aren't in model_specs.R (that's the original-11 roster) --
# pull their specs + group-fit params from the "new models" fit instead.
new_group_fits <- local({
  e2 <- new.env(); load(here("fits/group_fits_new_models.Rdata"), envir = e2)
  e2$group_fits_new
})
source(here("fits/new_model_specs.R"))
specs <- c(specs, new_model_specs)
group_fits <- c(group_fits, new_group_fits)

stopifnot(all(mech_reps %in% names(specs)), all(mech_reps %in% names(group_fits)))

build_model <- function(name) {
  bestmem <- group_fits[[name]]$optim$bestmem
  specs[[name]]$constructor(if (is.null(bestmem)) numeric(0) else as.numeric(bestmem))
}
is_stochastic <- function(name) isTRUE(specs[[name]]$stochastic)

N_SIM_SEARCH <- 20   # cheap during the search loop
N_SIM_FINAL  <- 200  # precise, for the final reported numbers

word_perf <- function(m) {
  wn <- rownames(m); on <- colnames(m)
  vapply(wn, function(w) if (w %in% on && sum(m[w, ]) > 0) m[w, w] / sum(m[w, ]) else 0, numeric(1))
}

## ---------------------------------------------------------------------------
## Objective: mean-over-words SD of ModelPerf across the mech_reps panel
## ---------------------------------------------------------------------------

genome_to_xsldata <- function(genome) {
  trials <- lapply(genome, as.integer)
  xslData(train = list(words = trials, objects = trials))
}

score_genome <- function(genome, models = mech_reps, n_sim = N_SIM_SEARCH) {
  dat <- genome_to_xsldata(genome)
  perf_mat <- sapply(models, function(m) {
    ctrl <- if (is_stochastic(m)) xslControl(n_sim = n_sim) else xslControl()
    res <- xsl_run(build_model(m), list(dat), control = ctrl)
    mat <- res$fits[[1]]$matrix
    n_div <- if (is_stochastic(m)) n_sim else 1
    word_perf(mat / n_div)
  })
  # perf_mat: n_words x n_models
  mean(apply(perf_mat, 1, sd))
}

# noise-free search-time objective: deterministic models only, so every
# accepted improvement during the search is real, not Monte Carlo noise
score_genome_det <- function(genome) score_genome(genome, models = det_reps)

## ---------------------------------------------------------------------------
## Mutation + hill-climbing search
## ---------------------------------------------------------------------------

# every word 1..V must still appear somewhere after a mutation
covers_all <- function(genome, V) setequal(unique(unlist(genome)), seq_len(V))

mutate_genome <- function(genome, V) {
  repeat {
    g <- genome
    t <- sample.int(length(g), 1)
    trial <- g[[t]]
    slot <- sample.int(length(trial), 1)
    candidates <- setdiff(seq_len(V), trial)
    if (length(candidates) == 0) next
    trial[slot] <- if (length(candidates) == 1) candidates else sample(candidates, 1)
    g[[t]] <- sort(trial)
    if (covers_all(g, V)) return(g)
  }
}

# Simulated annealing over the noise-free deterministic-only objective.
# Temperature decays geometrically from t0 to (approximately) t_end over
# n_iter steps; worsening moves are accepted with probability
# exp(delta / temperature), so early iterations can cross small dips to
# escape local optima while late iterations behave like plain hill-climbing.
anneal <- function(seed_genome, V, n_iter, t0 = 0.02, t_end = 0.0003, verbose_every = 1000) {
  decay <- (t_end / t0)^(1 / n_iter)
  temp <- t0

  best_genome <- seed_genome
  best_score <- score_genome_det(seed_genome)
  cur_genome <- best_genome; cur_score <- best_score
  n_accepted_worse <- 0L
  trace <- numeric(n_iter)

  for (i in seq_len(n_iter)) {
    cand <- mutate_genome(cur_genome, V)
    cand_score <- score_genome_det(cand)
    delta <- cand_score - cur_score
    accept <- delta > 0 || runif(1) < exp(delta / temp)
    if (accept) {
      if (delta <= 0) n_accepted_worse <- n_accepted_worse + 1L
      cur_genome <- cand; cur_score <- cand_score
    }
    if (cur_score > best_score) { best_genome <- cur_genome; best_score <- cur_score }
    trace[i] <- best_score
    temp <- temp * decay
    if (verbose_every > 0 && i %% verbose_every == 0) {
      message(sprintf("  iter %d: temp=%.5f current=%.4f best=%.4f (accepted %d worse moves so far)",
                      i, temp, cur_score, best_score, n_accepted_worse))
    }
  }
  list(genome = best_genome, score = best_score, trace = trace, n_accepted_worse = n_accepted_worse)
}

## ---------------------------------------------------------------------------
## Run: two seeds
## ---------------------------------------------------------------------------

seed_from_condition <- function(label) {
  d <- xsl_datasets[[which(sapply(xsl_datasets, function(x) x$label) == label)]]
  list(genome = lapply(d$train$words, as.integer),
       V = length(unique(unlist(d$train$words))))
}

N_ITER <- 15000  # deterministic-only objective is ~25x cheaper than the mixed panel, so afford far more

results <- list()
for (seed_label in c("filt9E_9L", "filt0E_9L")) {
  message("\n=== Seed: ", seed_label, " ===")
  seed <- seed_from_condition(seed_label)
  seed_score_full <- score_genome(seed$genome, mech_reps, N_SIM_FINAL)
  seed_score_det <- score_genome_det(seed$genome)
  message(sprintf("Seed score: full 10-model panel (n_sim=%d) = %.4f; deterministic-only = %.4f",
                  N_SIM_FINAL, seed_score_full, seed_score_det))

  t0 <- Sys.time()
  an <- anneal(seed$genome, seed$V, n_iter = N_ITER)
  message(sprintf("Search done in %.1f s (%d/%d worsening moves accepted)",
                  as.numeric(Sys.time() - t0, units = "secs"), an$n_accepted_worse, N_ITER))

  final_score_full <- score_genome(an$genome, mech_reps, N_SIM_FINAL)
  message(sprintf("Best-found design: deterministic-only (search objective) = %.4f; full 10-model panel (n_sim=%d) = %.4f",
                  an$score, N_SIM_FINAL, final_score_full))

  n_trials_changed <- sum(!map_lgl(seq_along(seed$genome),
                                   \(i) identical(seed$genome[[i]], an$genome[[i]])))

  results[[seed_label]] <- list(
    seed_genome = seed$genome, seed_score_full = seed_score_full, seed_score_det = seed_score_det,
    best_genome = an$genome, best_score_full = final_score_full, best_score_det = an$score,
    trace = an$trace, V = seed$V, n_trials_changed = n_trials_changed
  )
  message(sprintf("%d / %d trials changed from the original design",
                  n_trials_changed, length(seed$genome)))
}

save(results, mech_reps, det_reps, file = here("fits/discriminative_design_search.RData"))
message("\nSaved fits/discriminative_design_search.RData")

for (seed_label in names(results)) {
  r <- results[[seed_label]]
  cat(sprintf("\n%s: seed=%.4f -> best=%.4f (+%.1f%%), full 10-model panel (n_sim=%d); %d/%d trials changed\n",
             seed_label, r$seed_score_full, r$best_score_full,
             100 * (r$best_score_full - r$seed_score_full) / r$seed_score_full,
             N_SIM_FINAL, r$n_trials_changed, length(r$seed_genome)))
}
