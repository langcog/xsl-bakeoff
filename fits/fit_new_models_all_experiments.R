# Fits the model variants added to XSLmodels after the original paper --
# uncfam_attention ("BAM+Attention"), uncfam_predictive ("BAM+Prediction"),
# kalman_filter, and softmax_rl -- to ALL conditions currently in
# xsl_datasets. These aren't in model_specs.R's roster (that file is
# specifically the original paper's 11 models, kept separate for the
# pre-package/package comparison) since there's no original bakeoff fit to
# compare them against -- these are new to the package, not a reproduction
# of anything.
#
# Group-level only (all conditions fit jointly, no held-out folds). For
# cross-validated fits on the same models/conditions, see
# cv_new_models.R (a separate script, ~5x the cost).
#
# Bounds/starting values match XSLmodels' own xsl_model_registry() (see
# R/helper.R) exactly, since these are the package's own canonical bounds
# for these models, not something reverse-engineered from old research
# code like model_specs.R's entries are.
#
# Settings: same DEoptim settings as fit_all_models_all_experiments.R
# (NP=100, itermax=100 deterministic; NP=100, itermax=30 stochastic,
# n_sim=200 -- softmax_rl is the only stochastic model here).
#
# Checkpoints after every model (skips any already present in
# fits/group_fits_new_models.Rdata on a re-run), same as cv_new_models.R.

library(XSLmodels)
library(purrr)
library(here)

deoptim_reg <- DEoptim::DEoptim.control(reltol = .001, NP = 100, itermax = 100, trace = FALSE)
deoptim_stoch <- DEoptim::DEoptim.control(reltol = .001, NP = 100, itermax = 30, trace = FALSE)
run_control <- xslControl(n_sim = 200)

new_model_specs <- list(
  uncfam_attention = list(
    constructor = \(p) uncfam_attention(X = p[1], B = p[2], C = p[3]),
    lower = c(0.01, 0.8, 0.8), upper = c(0.5, 1.0, 1.0), stochastic = FALSE
  ),
  uncfam_predictive = list(
    constructor = \(p) uncfam_predictive(X = p[1], B = p[2], C = p[3]),
    lower = c(0.01, 0.8, 0.8), upper = c(0.5, 1.0, 1.0), stochastic = FALSE
  ),
  kalman_filter = list(
    # bounds widened after an earlier fit pinned all three parameters at
    # (or, for sigma2_0, right at) their original bounds -- see XSLmodels
    # PR #7
    constructor = \(p) kalman_filter(tau2 = p[1], sigma2_obs = p[2], sigma2_0 = p[3]),
    lower = c(0.000001, 0.01, 0.001), upper = c(1, 500, 10), stochastic = FALSE
  ),
  softmax_rl = list(
    # beta upper bound widened after an earlier fit landed at beta=18.3,
    # essentially pinned against the old upper bound of 20 -- see
    # XSLmodels PR #7
    constructor = \(p) softmax_rl(alpha = p[1], beta = p[2]),
    lower = c(0.01, 0.1), upper = c(1, 100), stochastic = TRUE
  )
)

data_all <- xsl_datasets
cat("Fitting to all", length(data_all), "conditions.\n")

fit_one <- \(spec, data) {
  dc <- if (spec$stochastic) deoptim_stoch else deoptim_reg
  ctrl <- if (spec$stochastic) run_control else xslControl()
  model <- spec$constructor(spec$lower) # placeholder model; xsl_fit() only needs its shape
  xsl_fit(model, data, lower = spec$lower, upper = spec$upper,
         control = ctrl, deoptim_control = dc)[[1]]
}

run_one <- \(spec, params, data) {
  ctrl <- if (spec$stochastic) run_control else xslControl()
  xsl_run(spec$constructor(params), data, control = ctrl)
}

out_file <- here("fits/group_fits_new_models.Rdata")

group_fits_new <- list()
gfd_new <- tibble::tibble()
if (file.exists(out_file)) {
  load(out_file) # restores group_fits_new (and gfd_new, if a prior run finished)
  cat("Resuming: found", length(group_fits_new), "already-completed model(s):",
      paste(names(group_fits_new), collapse = ", "), "\n")
}
checkpoint <- \() save(group_fits_new, gfd_new, file = out_file)

remaining <- setdiff(names(new_model_specs), names(group_fits_new))
cat("Remaining models to fit:", paste(remaining, collapse = ", "), "\n\n")

for (m in remaining) {
  cat(" ", m, "-", format(Sys.time()), "\n")
  group_fits_new[[m]] <- fit_one(new_model_specs[[m]], data_all)
  checkpoint()
}

build_gfd_rows <- \(m, spec, fit, data) {
  result <- run_one(spec, fit$optim$bestmem, data)
  map(result$fits, \(f) {
    tibble::tibble(
      Model = m, condnum = f$data$label, Condition = f$data$condition,
      ModelPerf = as.vector(f$perf), HumanPerf = f$data$accuracy, Nsubj = f$data$n_subj
    )
  }) |> purrr::list_rbind()
}

# (re)build gfd_new for any model missing from it (covers both newly-fit
# models this run and models fit in the past under the old, non-resumable
# version of this script, which never saved gfd_new incrementally)
models_missing_gfd <- setdiff(names(group_fits_new), if (nrow(gfd_new) > 0) unique(gfd_new$Model) else character(0))
if (length(models_missing_gfd) > 0) {
  cat("\nBuilding gfd_new for:", paste(models_missing_gfd, collapse = ", "), "\n")
  new_rows <- purrr::list_rbind(map(models_missing_gfd, \(m) {
    build_gfd_rows(m, new_model_specs[[m]], group_fits_new[[m]], data_all)
  }))
  gfd_new <- rbind(gfd_new, new_rows)
  checkpoint()
}

cat("Saved", out_file, "with", length(group_fits_new), "models.\n")
