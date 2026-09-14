# v2: generalized experimental-design search, per author feedback on v1.
#
# v1 fixed the design's "shape" (vocabulary size V, trial count T, and a
# single words-per-trial = objects-per-trial k) at the seed condition's own
# values, and searched only over which pairs co-occur where. Per feedback:
# variable item frequency is not a bug to constrain away (word/referent
# frequency is famously Zipf-distributed -- this script lets frequency
# emerge freely and just reports what shape it takes) but the design SHAPE
# itself is worth searching over too. v2 makes V, T, and per-trial
# words/objects counts all mutable, and adds trial-reordering (a pure
# order/recency-vs-primacy manipulation with no compositional change).
#
# Dimensions searched, and their bounds -----------------------------------
#   V   (# word-object pairs, i.e. vocabulary size): [V_MIN, V_MAX]
#   T   (# trials): [T_MIN, T_MAX]
#   k_w (words spoken per trial), k_o (objects visible per trial): each in
#       [K_MIN, K_MAX], and NOT required to be equal -- k_o > k_w means
#       distractor objects with no spoken word this trial; k_o < k_w means
#       a spoken word's own referent can be absent this trial (as in
#       naturalistic corpora, where many utterances aren't about anything
#       in view). This asymmetry is not new: about a third of xsl_datasets
#       conditions (the "MxN" family, e.g. "1x3", "2x4 6/12") already have
#       k_w != k_o; v2 just lets the search reach for it and vary it
#       per-trial rather than fixing one (k_w, k_o) pair for the whole
#       design as those hand-built conditions do.
#   trial order: an explicit reorder mutation, independent of composition,
#       so a winning design's score can be checked against a shuffled
#       version of itself to ask whether these models are sensitive to
#       trial order at all (recency/decay) -- see "Order sensitivity" below.
#
# Bounds are grounded in the real dataset (`xsl_datasets`: V in 8-24, T in
# 16-108, k in 1-4) and the author's own stated practical ceilings (V <=
# ~40 word-object pairs learnable in one session; k <= ~10 pairs/trial,
# "though maybe interesting to push ... a bit"). T's ceiling is a guess
# (a bit above the observed max) since no explicit bound was given.
V_MIN <- 6;  V_MAX <- 40
T_MIN <- 16; T_MAX <- 130
K_MIN <- 1;  K_MAX <- 10
#
# NOT implemented in v2 (deliberately deferred, see the report) ----------
#   - True many-to-many ground truth (a word with >1 correct referent, i.e.
#     synonymy; an object that is the correct referent of >1 word, i.e.
#     homonymy). The package's scoring convention (a word's correct
#     referent is the object sharing its label) is a strict 1-to-1 bijection
#     baked into get_perf()/mafc_test(); genuinely relaxing this needs a
#     custom scoring function, not just a richer trial generator, and is a
#     bigger step than the rest of this generalization.
#   - Category-learning manipulations (multiple exemplar objects per word),
#     per the author's own instruction to set this aside for now.
#   - Explicit "trajectory" structure (e.g. hand-coding k to ramp up or down
#     over training). Nothing stops mutation + reordering from discovering
#     such structure on its own if it's rewarded; see "Order sensitivity"
#     for how we check whether that happened, post hoc.
#
# Genome representation -----------------------------------------------------
# A named list: V (integer), trials (list of length T, each
# list(words = integer vector, objects = integer vector), values in 1..V).
# Validity invariant, checked after every mutation (mutate_genome() just
# retries on failure): every index 1..V appears among the WORDS at least
# once, and every index 1..V appears among the OBJECTS at least once (not
# merely their union) -- several models index their internal word x object
# matrix by numeric position rather than by label, so the word-label-set
# and object-label-set must each independently be a contiguous 1..V, or
# position and label correspondence breaks (hit this as a real bug during
# development: `m[tr_w, tr_o]` subscript-out-of-bounds errors the moment a
# pair appeared only as an object and never as a word). T is within bounds,
# and every trial's words/objects lengths are within [K_MIN, K_MAX] with no
# internal duplicates.
#
# Mutation operators (one drawn per step, see MUT_WEIGHTS) ------------------
#   swap      -- replace one word/object slot in a random trial with a
#                different valid pair index. Cheapest, most frequent.
#   resize    -- grow or shrink one trial's words or objects list by one
#                slot (respecting K_MIN/K_MAX).
#   add_trial / remove_trial -- change T by 1 (respecting T_MIN/T_MAX);
#                adding clones+jitters a random existing trial.
#   add_pair  -- increase V by 1 and place the new pair into a random trial.
#   remove_pair -- decrease V by 1, stripping the removed pair's occurrences
#                first (dropping any trial left with empty words/objects).
#   reorder   -- swap the sequence position of two trials (T and composition
#                both unchanged -- the pure "does order matter" mutation).
#
# Search: same simulated annealing as v1 (decaying-temperature acceptance
# of worsening moves), scored on the noise-free deterministic-only panel
# during search and re-verified on the full 10-model panel at n_sim=200.

library(purrr)
library(here)

source(here("fits/design_search_common.R")) # model panel, build_model, score_design[_det]

set.seed(2026)

## ---------------------------------------------------------------------------
## Genome <-> xslData, validity
## ---------------------------------------------------------------------------

genome_to_train <- function(genome) {
  list(words   = lapply(genome$trials, function(tr) as.integer(tr$words)),
       objects = lapply(genome$trials, function(tr) as.integer(tr$objects)))
}

score_genome <- function(genome, models = mech_reps, n_sim = N_SIM_FINAL) {
  score_design(genome_to_train(genome), models = models, n_sim = n_sim)
}
score_genome_det <- function(genome) score_design_det(genome_to_train(genome))

is_valid <- function(genome) {
  T_cur <- length(genome$trials)
  if (T_cur < T_MIN || T_cur > T_MAX) return(FALSE)
  used_w <- integer(0); used_o <- integer(0)
  for (tr in genome$trials) {
    if (length(tr$words) < K_MIN || length(tr$words) > K_MAX) return(FALSE)
    if (length(tr$objects) < K_MIN || length(tr$objects) > K_MAX) return(FALSE)
    if (anyDuplicated(tr$words) || anyDuplicated(tr$objects)) return(FALSE)
    used_w <- c(used_w, tr$words); used_o <- c(used_o, tr$objects)
  }
  # the word-label-set and object-label-set must EACH independently cover
  # {1..V} (not just their union) -- several models index their internal
  # word x object matrix by numeric POSITION, not by label, so voc_sz and
  # ref_sz must both equal V or those position/label correspondences break
  # (see the v2 fix note in the header comment for the bug this caught)
  setequal(unique(used_w), seq_len(genome$V)) && setequal(unique(used_o), seq_len(genome$V))
}

## ---------------------------------------------------------------------------
## Mutation operators -- each returns NULL on failure (caller retries)
## ---------------------------------------------------------------------------

mut_swap <- function(g) {
  t <- sample.int(length(g$trials), 1)
  field <- sample(c("words", "objects"), 1)
  vec <- g$trials[[t]][[field]]
  slot <- sample.int(length(vec), 1)
  candidates <- setdiff(seq_len(g$V), vec)
  if (length(candidates) == 0) return(NULL)
  vec[slot] <- if (length(candidates) == 1) candidates else sample(candidates, 1)
  g$trials[[t]][[field]] <- vec
  g
}

mut_resize <- function(g) {
  t <- sample.int(length(g$trials), 1)
  field <- sample(c("words", "objects"), 1)
  vec <- g$trials[[t]][[field]]
  if (runif(1) < 0.5 && length(vec) < K_MAX) {
    candidates <- setdiff(seq_len(g$V), vec)
    if (length(candidates) == 0) return(NULL)
    vec <- c(vec, if (length(candidates) == 1) candidates else sample(candidates, 1))
  } else if (length(vec) > K_MIN) {
    vec <- vec[-sample.int(length(vec), 1)]
  } else return(NULL)
  g$trials[[t]][[field]] <- vec
  g
}

mut_add_trial <- function(g) {
  if (length(g$trials) >= T_MAX) return(NULL)
  base <- g$trials[[sample.int(length(g$trials), 1)]] # clone + jitter an existing trial
  g$trials <- append(g$trials, list(base), after = sample.int(length(g$trials) + 1, 1) - 1)
  g
}

mut_remove_trial <- function(g) {
  if (length(g$trials) <= T_MIN) return(NULL)
  g$trials[[sample.int(length(g$trials), 1)]] <- NULL
  g
}

mut_add_pair <- function(g) {
  if (g$V >= V_MAX) return(NULL)
  new_pair <- g$V + 1L
  g$V <- new_pair
  t <- sample.int(length(g$trials), 1)
  field <- sample(c("words", "objects"), 1)
  if (length(g$trials[[t]][[field]]) >= K_MAX) return(NULL)
  g$trials[[t]][[field]] <- c(g$trials[[t]][[field]], new_pair)
  g
}

mut_remove_pair <- function(g) {
  if (g$V <= V_MIN) return(NULL)
  victim <- g$V
  g$trials <- map(g$trials, function(tr) {
    tr$words <- tr$words[tr$words != victim]
    tr$objects <- tr$objects[tr$objects != victim]
    tr
  })
  g$trials <- keep(g$trials, function(tr) length(tr$words) > 0 && length(tr$objects) > 0)
  g$V <- victim - 1L
  g
}

mut_reorder <- function(g) {
  if (length(g$trials) < 2) return(NULL)
  ij <- sample.int(length(g$trials), 2)
  g$trials[ij] <- g$trials[rev(ij)]
  g
}

MUT_FUNS <- list(swap = mut_swap, resize = mut_resize, add_trial = mut_add_trial,
                 remove_trial = mut_remove_trial, add_pair = mut_add_pair,
                 remove_pair = mut_remove_pair, reorder = mut_reorder)
# swap dominates (cheap, local); structural mutations are rarer
MUT_WEIGHTS <- c(swap = 10, resize = 3, add_trial = 1, remove_trial = 1,
                 add_pair = 1, remove_pair = 1, reorder = 2)

mutate_genome <- function(g) {
  repeat {
    op <- sample(names(MUT_FUNS), 1, prob = MUT_WEIGHTS)
    cand <- tryCatch(MUT_FUNS[[op]](g), error = function(e) NULL)
    if (!is.null(cand) && is_valid(cand)) return(cand)
  }
}

## ---------------------------------------------------------------------------
## Simulated annealing (same schedule as v1)
## ---------------------------------------------------------------------------

anneal <- function(seed_genome, n_iter, t0 = 0.02, t_end = 0.0003, verbose_every = 1000) {
  decay <- (t_end / t0)^(1 / n_iter)
  temp <- t0
  best_genome <- seed_genome
  best_score <- score_genome_det(seed_genome)
  cur_genome <- best_genome; cur_score <- best_score
  n_accepted_worse <- 0L
  trace <- numeric(n_iter)
  shape_trace <- matrix(NA_integer_, n_iter, 2, dimnames = list(NULL, c("V", "T")))

  for (i in seq_len(n_iter)) {
    cand <- mutate_genome(cur_genome)
    cand_score <- score_genome_det(cand)
    delta <- cand_score - cur_score
    accept <- delta > 0 || runif(1) < exp(delta / temp)
    if (accept) {
      if (delta <= 0) n_accepted_worse <- n_accepted_worse + 1L
      cur_genome <- cand; cur_score <- cand_score
    }
    if (cur_score > best_score) { best_genome <- cur_genome; best_score <- cur_score }
    trace[i] <- best_score
    shape_trace[i, ] <- c(best_genome$V, length(best_genome$trials))
    temp <- temp * decay
    if (verbose_every > 0 && i %% verbose_every == 0) {
      message(sprintf("  iter %d: temp=%.5f current=%.4f best=%.4f (V=%d, T=%d; %d worse moves accepted)",
                      i, temp, cur_score, best_score, best_genome$V, length(best_genome$trials), n_accepted_worse))
    }
  }
  list(genome = best_genome, score = best_score, trace = trace, shape_trace = shape_trace,
      n_accepted_worse = n_accepted_worse)
}

## ---------------------------------------------------------------------------
## Diagnostics: emergent frequency shape + order sensitivity
## ---------------------------------------------------------------------------

# Zipf check: fit log(frequency) ~ log(rank) across the design's V pairs
# (pooling word- and object-slot occurrences); slope near 0 = uniform,
# more negative = more Zipf-like skew. Purely descriptive -- nothing in
# the search targets or rewards any particular slope.
zipf_slope <- function(genome) {
  freq <- table(factor(unlist(genome$trials), levels = seq_len(genome$V)))
  freq <- sort(as.integer(freq), decreasing = TRUE)
  freq <- freq[freq > 0]
  if (length(freq) < 3) return(NA_real_)
  unname(coef(lm(log(freq) ~ log(seq_along(freq))))[2])
}

# Order sensitivity: re-score the SAME trial multiset in `n_shuffles` random
# orders (full 10-model panel, one n_sim=200 evaluation each) and compare to
# the winning (optimized-order) score. If shuffled orders score
# meaningfully lower on average, trial order -- not just composition -- is
# part of what the search found.
order_sensitivity <- function(genome, n_shuffles = 5) {
  shuffled_scores <- map_dbl(seq_len(n_shuffles), function(.i) {
    g <- genome; g$trials <- sample(g$trials)
    score_genome(g)
  })
  list(optimized_order = score_genome(genome), shuffled = shuffled_scores,
      mean_shuffled = mean(shuffled_scores))
}

## ---------------------------------------------------------------------------
## Run
## ---------------------------------------------------------------------------

seed_from_condition <- function(label) {
  d <- xsl_datasets[[which(sapply(xsl_datasets, function(x) x$label) == label)]]
  V <- length(unique(unlist(d$train$words)))
  trials <- map2(d$train$words, d$train$objects, \(w, o) list(words = as.integer(w), objects = as.integer(o)))
  list(V = V, trials = trials)
}

seed_blank <- function(V, T, k) {
  # a "neutral" starting design at roughly the dataset's median shape:
  # trial t's k words/objects are (t + 0:(k-1)) mod V + 1, i.e. a simple
  # rolling window -- not random, so it starts fully valid (every pair
  # covered) without needing a repair loop
  trials <- map(seq_len(T), function(t) {
    idx <- ((t - 1):(t - 1 + k - 1)) %% V + 1
    list(words = idx, objects = idx)
  })
  list(V = V, trials = trials)
}

N_ITER <- 12000

seeds <- list(
  filt9E_9L = seed_from_condition("filt9E_9L"),
  blank_median = seed_blank(V = 18, T = 45, k = 2)
)

results <- list()
for (seed_label in names(seeds)) {
  message("\n=== Seed: ", seed_label, " ===")
  seed <- seeds[[seed_label]]
  stopifnot(is_valid(seed))
  seed_score_full <- score_genome(seed, mech_reps, N_SIM_FINAL)
  seed_score_det <- score_genome_det(seed)
  message(sprintf("Seed: V=%d, T=%d; full panel (n_sim=%d) = %.4f; det-only = %.4f",
                  seed$V, length(seed$trials), N_SIM_FINAL, seed_score_full, seed_score_det))

  t0 <- Sys.time()
  an <- anneal(seed, n_iter = N_ITER)
  message(sprintf("Search done in %.1f s (%d/%d worsening moves accepted)",
                  as.numeric(Sys.time() - t0, units = "secs"), an$n_accepted_worse, N_ITER))

  final_score_full <- score_genome(an$genome, mech_reps, N_SIM_FINAL)
  slope <- zipf_slope(an$genome)
  message(sprintf("Best-found: V=%d, T=%d; det-only (search obj) = %.4f; full panel (n_sim=%d) = %.4f; Zipf slope = %.2f",
                  an$genome$V, length(an$genome$trials), an$score, N_SIM_FINAL, final_score_full, slope))

  os <- order_sensitivity(an$genome)
  message(sprintf("Order sensitivity: optimized order = %.4f; mean of 5 shuffles = %.4f (range %.4f-%.4f)",
                  os$optimized_order, os$mean_shuffled, min(os$shuffled), max(os$shuffled)))

  results[[seed_label]] <- list(
    seed = seed, seed_score_full = seed_score_full, seed_score_det = seed_score_det,
    best_genome = an$genome, best_score_full = final_score_full, best_score_det = an$score,
    trace = an$trace, shape_trace = an$shape_trace, n_accepted_worse = an$n_accepted_worse,
    zipf_slope = slope, order_sensitivity = os
  )
}

save(results, mech_reps, det_reps, file = here("fits/discriminative_design_search_v2.RData"))
message("\nSaved fits/discriminative_design_search_v2.RData")

for (seed_label in names(results)) {
  r <- results[[seed_label]]
  cat(sprintf("\n%s: seed=%.4f (V=%d,T=%d) -> best=%.4f (V=%d,T=%d) (+%.1f%%); Zipf slope=%.2f; order-sensitive: opt=%.4f vs shuffled-mean=%.4f\n",
             seed_label, r$seed_score_full, r$seed$V, length(r$seed$trials),
             r$best_score_full, r$best_genome$V, length(r$best_genome$trials),
             100 * (r$best_score_full - r$seed_score_full) / r$seed_score_full,
             r$zipf_slope, r$order_sensitivity$optimized_order, r$order_sensitivity$mean_shuffled))
}
