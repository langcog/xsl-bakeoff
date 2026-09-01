# 5-fold cross-validated fits for every model in model_specs.R (the paper's
# 11 models -- no baseline, no fazlyt, matching the original
# cv_group_fits.Rdata's roster), fit to ALL conditions currently in
# xsl_datasets (53 as of this writing: the original 44 plus temporal
# contiguity / Suanda 2014 / Koehne 2013), not just the 44-condition subset
# recreate_fits_with_package.R targets.
#
# This is the expensive twin of fit_all_models_all_experiments.R (~5x the
# cost, since every model is fit 5 times instead of once) -- run it
# separately, expect it to take a long time (the equivalent run on just 44
# conditions with n_sim=500 took multiple days and never finished; this one
# uses n_sim=200 and 53 conditions, so budget accordingly), and note it
# checkpoints after every model (not just at the very end) specifically so
# an interrupted run doesn't lose everything: if fits/cv_all_experiments.Rdata
# already has some models in it, re-running this script skips them and
# picks up where it left off.
#
# Settings: close to the original paper's DEoptim settings (NP=100,
# itermax=100 for deterministic models; NP=100, itermax=30 for stochastic
# models), but with n_sim reduced from the original 500 to 200 (see
# fit_all_models_all_experiments.R for why).

library(XSLmodels)
library(purrr)
library(caret) # for the same fold-splitting approach used throughout
library(here)

source(here("fits/model_specs.R")) # model_specs, fazlyt_spec (unused here)

np_reg <- 100; itermax_reg <- 100
np_stoch <- 100; itermax_stoch <- 30
n_sim <- 200

deoptim_reg <- DEoptim::DEoptim.control(reltol = .001, NP = np_reg, itermax = itermax_reg, trace = FALSE)
deoptim_stoch <- DEoptim::DEoptim.control(reltol = .001, NP = np_stoch, itermax = itermax_stoch, trace = FALSE)
run_control <- xslControl(n_sim = n_sim)

data_all <- xsl_datasets
labels_all <- map_chr(data_all, "label")
cat("Cross-validating on all", length(data_all), "conditions.\n")

fit_one <- \(spec, data) {
  dc <- if (spec$stochastic) deoptim_stoch else deoptim_reg
  ctrl <- if (spec$stochastic) run_control else xslControl()
  model <- spec$constructor(spec$lower)
  xsl_fit(model, data, lower = spec$lower, upper = spec$upper,
         control = ctrl, deoptim_control = dc)[[1]]
}

run_one <- \(spec, params, data) {
  model <- spec$constructor(params)
  ctrl <- if (spec$stochastic) run_control else xslControl()
  xsl_run(model, data, control = ctrl)
}

out_file <- here("fits/cv_all_experiments.Rdata")

cv_all <- list()
if (file.exists(out_file)) {
  load(out_file) # restores cv_all with whatever models finished last time
  cat("Resuming: found", length(cv_all), "already-completed model(s):",
      paste(names(cv_all), collapse = ", "), "\n")
}

set.seed(123)
folds <- createFolds(labels_all, k = 5, list = TRUE)

cv_models <- names(model_specs) # 11 -- no baseline, no fazlyt (matches the original)
remaining <- setdiff(cv_models, names(cv_all))
cat("Remaining models to fit:", paste(remaining, collapse = ", "), "\n\n")

for (m in remaining) {
  cat(m, "-", format(Sys.time()), "\n")
  spec <- model_specs[[m]]
  pars <- NULL; train_acc <- NULL; test_list <- list(); testdf <- tibble::tibble()
  for (i in seq_along(folds)) {
    split <- get_train_test_split(folds[[i]], data_all)
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
    cat("  fold", i, "-", format(Sys.time()), "- train SSE:", round(fit$optim$bestval, 3),
        "test SSE:", round(test_run$sse, 3), "\n")
  }
  cv_all[[m]] <- list(pars = pars, train_acc = train_acc, test = test_list, testdf = testdf)
  save(cv_all, file = out_file) # checkpoint after every model
  cat("  saved (", length(cv_all), "/", length(cv_models), "models done)\n\n")
}

cat("Done. Saved", out_file, "with all", length(cv_all), "models.\n")
