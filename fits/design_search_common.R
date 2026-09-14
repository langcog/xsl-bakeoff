# Shared setup for fits/discriminative_design_search*.R: the one-per-mechanism
# model panel (at group-fit parameters) and the disagreement objective.
# Sourced by both the fixed-shape (v1) and generalized (v2) search scripts.

library(XSLmodels)
library(here)

source(here("fits/model_specs.R"))       # model_specs
source(here("fits/new_model_specs.R"))   # new_model_specs

## ---------------------------------------------------------------------------
## Model panel: one representative per distinct mechanism, at group-fit params
## (same panel as appendix.Rmd's "Model disagreement" section)
## ---------------------------------------------------------------------------

group_fits <- local({
  e1 <- new.env(); load(here("fits/group_fits_all_experiments.Rdata"), envir = e1)
  e1$group_fits_all
})
new_group_fits <- local({
  e2 <- new.env(); load(here("fits/group_fits_new_models.Rdata"), envir = e2)
  e2$group_fits_new
})
group_fits <- c(group_fits, new_group_fits)

specs <- c(model_specs, list(baseline = list(constructor = function(p) baseline(), stochastic = FALSE)),
          new_model_specs)

mech_reps <- c("kachergis", "fazly", "Bayesian_decay", "rescorla-wagner", "trueswell2012",
               "guess-and-test", "pursuit", "softmax_rl", "kalman_filter", "baseline")
# the deterministic subset of mech_reps -- used for the (noise-free) search
# objective; the full panel above is used only for final verification
det_reps <- c("kachergis", "fazly", "Bayesian_decay", "rescorla-wagner", "kalman_filter", "baseline")

stopifnot(all(mech_reps %in% names(specs)), all(mech_reps %in% names(group_fits)))

build_model <- function(name) {
  bestmem <- group_fits[[name]]$optim$bestmem
  specs[[name]]$constructor(if (is.null(bestmem)) numeric(0) else as.numeric(bestmem))
}
is_stochastic <- function(name) isTRUE(specs[[name]]$stochastic)

N_SIM_FINAL <- 200  # precise, for the final reported numbers (verification only)

# proportion-correct (Luce) for word `w`: object with the SAME LABEL as w is
# always its correct referent (the package/dataset convention throughout);
# matched by dimname since a design's word set and object set need not be
# the same size or fully overlapping under the v2 generalization.
word_perf <- function(m) {
  wn <- rownames(m); on <- colnames(m)
  vapply(wn, function(w) if (w %in% on && sum(m[w, ]) > 0) m[w, w] / sum(m[w, ]) else 0, numeric(1))
}

## ---------------------------------------------------------------------------
## Objective: mean-over-words SD of ModelPerf across a model panel
## ---------------------------------------------------------------------------

score_design <- function(train, models = mech_reps, n_sim = N_SIM_FINAL) {
  dat <- xslData(train = train)
  perf_mat <- sapply(models, function(m) {
    ctrl <- if (is_stochastic(m)) xslControl(n_sim = n_sim) else xslControl()
    res <- xsl_run(build_model(m), list(dat), control = ctrl)
    mat <- res$fits[[1]]$matrix
    n_div <- if (is_stochastic(m)) n_sim else 1
    word_perf(mat / n_div)
  })
  mean(apply(perf_mat, 1, sd))
}

# noise-free search-time objective: deterministic models only, so every
# accepted improvement during a search is real, not Monte Carlo noise
score_design_det <- function(train) score_design(train, models = det_reps)
