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
# Simple stochastic hill-climbing (a degenerate simulated annealing: only
# accepts strict improvements here, since the objective is cheap enough to
# just run many random restarts' worth of proposals instead of tuning a
# temperature schedule). At each step: pick a random trial, replace one of
# its members with a value not currently in that trial, and recompute the
# objective; keep the mutation iff the objective improves. Rejects any
# mutation that would drop an index out of the design entirely (every word
# must appear at least once, or its performance is undefined).
#
# Objective -----------------------------------------------------------
# Same "model disagreement" index as appendix.Rmd's "Model disagreement"
# section: mean over words of the SD of ModelPerf across a fixed panel of
# one-per-mechanism models (see mech_reps below), each run ONCE at its
# already-fit group-fit parameters (xsl_model_registry fits are NOT re-run
# here -- the whole point is "more discriminative for the theories as they
# currently stand", not moving-target parameters).
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

score_genome <- function(genome, n_sim = N_SIM_SEARCH) {
  dat <- genome_to_xsldata(genome)
  perf_mat <- sapply(mech_reps, function(m) {
    ctrl <- if (is_stochastic(m)) xslControl(n_sim = n_sim) else xslControl()
    res <- xsl_run(build_model(m), list(dat), control = ctrl)
    mat <- res$fits[[1]]$matrix
    n_div <- if (is_stochastic(m)) n_sim else 1
    word_perf(mat / n_div)
  })
  # perf_mat: n_words x n_models
  mean(apply(perf_mat, 1, sd))
}

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

hill_climb <- function(seed_genome, V, n_iter, n_sim = N_SIM_SEARCH, verbose_every = 100) {
  best_genome <- seed_genome
  best_score <- score_genome(seed_genome, n_sim)
  cur_genome <- best_genome; cur_score <- best_score
  trace <- numeric(n_iter)
  for (i in seq_len(n_iter)) {
    cand <- mutate_genome(cur_genome, V)
    cand_score <- score_genome(cand, n_sim)
    if (cand_score > cur_score) { cur_genome <- cand; cur_score <- cand_score }
    if (cur_score > best_score) { best_genome <- cur_genome; best_score <- cur_score }
    trace[i] <- best_score
    if (verbose_every > 0 && i %% verbose_every == 0) {
      message(sprintf("  iter %d: current=%.4f best=%.4f", i, cur_score, best_score))
    }
  }
  list(genome = best_genome, score = best_score, trace = trace)
}

## ---------------------------------------------------------------------------
## Run: two seeds
## ---------------------------------------------------------------------------

seed_from_condition <- function(label) {
  d <- xsl_datasets[[which(sapply(xsl_datasets, function(x) x$label) == label)]]
  list(genome = lapply(d$train$words, as.integer),
       V = length(unique(unlist(d$train$words))))
}

N_ITER <- 1200

results <- list()
for (seed_label in c("filt9E_9L", "filt0E_9L")) {
  message("\n=== Seed: ", seed_label, " ===")
  seed <- seed_from_condition(seed_label)
  seed_score <- score_genome(seed$genome, N_SIM_FINAL)
  message(sprintf("Seed score (n_sim=%d): %.4f", N_SIM_FINAL, seed_score))

  t0 <- Sys.time()
  hc <- hill_climb(seed$genome, seed$V, n_iter = N_ITER)
  message(sprintf("Search done in %.1f s", as.numeric(Sys.time() - t0, units = "secs")))

  final_score <- score_genome(hc$genome, N_SIM_FINAL)
  message(sprintf("Best-found score, re-verified at n_sim=%d: %.4f (search-time estimate was %.4f)",
                  N_SIM_FINAL, final_score, hc$score))

  n_trials_changed <- sum(!map_lgl(seq_along(seed$genome),
                                   \(i) identical(seed$genome[[i]], hc$genome[[i]])))

  results[[seed_label]] <- list(
    seed_genome = seed$genome, seed_score = seed_score,
    best_genome = hc$genome, best_score = final_score,
    trace = hc$trace, V = seed$V, n_trials_changed = n_trials_changed
  )
  message(sprintf("%d / %d trials changed from the original design",
                  n_trials_changed, length(seed$genome)))
}

save(results, mech_reps, file = here("fits/discriminative_design_search.RData"))
message("\nSaved fits/discriminative_design_search.RData")

for (seed_label in names(results)) {
  r <- results[[seed_label]]
  cat(sprintf("\n%s: seed=%.4f -> best=%.4f (+%.1f%%), %d/%d trials changed\n",
             seed_label, r$seed_score, r$best_score,
             100 * (r$best_score - r$seed_score) / r$seed_score,
             r$n_trials_changed, length(r$seed_genome)))
}
