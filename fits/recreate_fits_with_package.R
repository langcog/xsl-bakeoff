# Recreates fits/group_fits.Rdata and fits/cv_group_fits.Rdata -- originally
# built by fit_group_and_cond.R / cv_group_fits.R using this repo's own ad hoc
# fitting_functions.R + models/*.R -- using the XSLmodels package instead.
#
# The original fits are preserved in fits/pre-package/ for comparison; see
# compare_pre_package_vs_package.R for a systematic comparison of this
# script's output against them, and model_specs.R for the model-by-model
# mapping onto the package (including known, expected differences that
# won't be resolved just by refitting).
#
# Model roster and bounds are copied exactly from fit_group_and_cond.R and
# cv_group_fits.R, cross-checked against the models actually present in the
# saved fits/pre-package/*.Rdata (which include a couple of models --
# "baseline" in group_fits, and no "fazlyt" in cv_group_fits at all -- that
# don't quite match what the *current* fit_group_and_cond.R/cv_group_fits.R
# function bodies would produce; the saved .Rdata is treated as ground
# truth throughout, since that's what the paper's tables/figures actually
# used). "fazlyt" (fazly with a threshold parameter) is fit here too as a
# bonus, since it's in group_fits.Rdata's *function definition* even though
# the saved artifact used "baseline" instead -- but it's saved separately
# (fazlyt_fit.Rdata), since it isn't part of the paper's actual tables.
#
# RUNTIME: with quick <- FALSE (the original DEoptim settings), this is a
# faithful but SLOW reproduction -- expect it to take hours, dominated by
# the four stochastic models (500 simulations per evaluation) and by the
# cross-validation loop (5x the group-fit cost). Set quick <- TRUE for a
# fast (~minutes) smoke test that the script runs end to end; that is NOT
# a valid replication of the paper, just a check that the pipeline works.
# See compare_pre_package_vs_package.R's same-parameter check for a fast
# way to validate the model implementations themselves, independent of
# DEoptim convergence.

library(XSLmodels)
library(purrr)
library(caret) # only needed here, to exactly reproduce the original 5-fold split
library(here)

source(here("fits/model_specs.R")) # model_specs, fazlyt_spec

quick <- FALSE # set FALSE for the real, original-settings replication

if (quick) {
  np_reg <- 8; itermax_reg <- 3
  np_stoch <- 8; itermax_stoch <- 2
  n_sim <- 5
} else {
  np_reg <- 100; itermax_reg <- 100 # matches fit_model()'s DEoptim.control
  np_stoch <- 100; itermax_stoch <- 30 # matches fit_stochastic_model()'s
  n_sim <- 500 # matches run_stochastic_model()'s default Nsim
}

deoptim_reg <- DEoptim::DEoptim.control(reltol = .001, NP = np_reg, itermax = itermax_reg, trace = FALSE)
deoptim_stoch <- DEoptim::DEoptim.control(reltol = .001, NP = np_stoch, itermax = itermax_stoch, trace = FALSE)
run_control <- xslControl(n_sim = n_sim)

## ---- Data: the same 44 conditions as the original combined_data.RData ----
# xsl_datasets now ships more than 44 conditions (temporal contiguity /
# Suanda / Koehne were added after this paper); restrict to the original 44.
data44 <- xsl_datasets[1:44]
stopifnot(length(data44) == 44)
labels44 <- map_chr(data44, "label")
if (file.exists(here("data/combined_data.RData"))) {
  load(here("data/combined_data.RData"))
  stopifnot(identical(names(combined_data), labels44))
}

fit_one <- \(spec, data) {
  dc <- if (spec$stochastic) deoptim_stoch else deoptim_reg
  ctrl <- if (spec$stochastic) run_control else xslControl()
  model <- spec$constructor(spec$lower) # placeholder model; xsl_fit() only needs its shape
  xsl_fit(model, data, lower = spec$lower, upper = spec$upper,
         control = ctrl, deoptim_control = dc)[[1]]
}

run_one <- \(spec, params, data) {
  model <- spec$constructor(params)
  ctrl <- if (spec$stochastic) run_control else xslControl()
  xsl_run(model, data, control = ctrl)
}

## ---- Group fits (all 44 conditions fit jointly) ----

cat("Fitting group models (12, including baseline)...\n")
group_fits <- list()
for (m in names(model_specs)) {
  cat(" ", m, "\n")
  group_fits[[m]] <- fit_one(model_specs[[m]], data44)
}
# baseline: zero free parameters, nothing to optimize
group_fits[["baseline"]] <- xsl_fit(baseline(), data44, lower = numeric(0), upper = numeric(0))[[1]]

# gfd: one row per (model, condition, item), matching the original
# get_model_dataframe()'s shape (Model, condnum, Condition, ModelPerf,
# HumanPerf, Nsubj), which paper-using-package.Rmd expects.
build_gfd <- \(fits, specs, data) {
  rows <- map(names(fits), \(m) {
    spec <- if (m == "baseline") list(constructor = \(p) baseline(), stochastic = FALSE) else specs[[m]]
    result <- run_one(spec, fits[[m]]$optim$bestmem, data)
    map(result$fits, \(f) {
      tibble::tibble(
        Model = m, condnum = f$data$label, Condition = f$data$condition,
        ModelPerf = as.vector(f$perf), HumanPerf = f$data$accuracy, Nsubj = f$data$n_subj
      )
    }) |> purrr::list_rbind()
  })
  purrr::list_rbind(rows)
}
gfd <- build_gfd(group_fits, model_specs, data44)

save(group_fits, gfd, file = here("fits/group_fits.Rdata"))
cat("Saved fits/group_fits.Rdata\n")

## ---- fazlyt bonus fit (not part of the paper's saved artifact) ----
cat("Fitting bonus model: fazlyt...\n")
fazlyt_fit <- fit_one(fazlyt_spec, data44)
save(fazlyt_fit, file = here("fits/fazlyt_fit.Rdata"))

## ---- Cross-validated group fits ----
# Reproduces cross_validated_group_fits()'s exact fold-splitting procedure
# (caret::createFolds with set.seed(123)) so fold assignments match the
# original exactly; verified separately (same labels44 vector + seed
# reproduces the original folds bit-for-bit).

cv_models <- names(model_specs) # all 11 -- no fazlyt in CV
cat("\nCross-validating", length(cv_models), "models (5 folds each)...\n")

set.seed(123)
folds <- createFolds(labels44, k = 5, list = TRUE)

cv_group_fits <- list()
for (m in cv_models) {
  cat(" ", m, "\n")
  spec <- model_specs[[m]]
  pars <- NULL; train_acc <- NULL; test_list <- list(); testdf <- tibble::tibble()
  for (i in seq_along(folds)) {
    split <- get_train_test_split(folds[[i]], data44)
    fit <- fit_one(spec, split$train)
    test_run <- run_one(spec, fit$optim$bestmem, split$test)
    pars <- rbind(pars, fit$optim$bestmem)
    train_acc <- c(train_acc, fit$optim$bestval)
    test_list[[i]] <- test_run
    fold_rows <- map(test_run$fits, \(f) {
      tibble::tibble(
        Model = m, condnum = f$data$label, Condition = f$data$condition,
        ModelPerf = as.vector(f$perf), HumanPerf = f$data$accuracy, Nsubj = f$data$n_subj
      )
    }) |> purrr::list_rbind()
    testdf <- rbind(testdf, fold_rows)
    cat("    fold", i, "train SSE:", round(fit$optim$bestval, 3),
        "test SSE:", round(test_run$sse, 3), "\n")
  }
  cv_group_fits[[m]] <- list(pars = pars, train_acc = train_acc, test = test_list, testdf = testdf)
}

save(cv_group_fits, file = here("fits/cv_group_fits.Rdata"))
cat("Saved fits/cv_group_fits.Rdata\n")
