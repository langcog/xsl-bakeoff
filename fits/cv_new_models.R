# 5-fold cross-validated fits for the model variants added to XSLmodels
# after the original paper -- uncfam_attention ("BAM+Attention"),
# uncfam_predictive ("BAM+Prediction"), kalman_filter, and softmax_rl --
# fit to ALL conditions currently in xsl_datasets. Companion to
# fit_new_models_all_experiments.R; see that script for why these aren't
# in model_specs.R's roster.
#
# Checkpoints after every model (not just at the end): if
# fits/cv_new_models.Rdata already has a model in it, re-running this
# script skips it and picks up where it left off.
#
# Uses the same 5-fold split as cv_all_experiments.R (same seed, same
# condition ordering), so results are directly comparable fold-for-fold.
#
# Settings: same as fit_new_models_all_experiments.R (NP=100, itermax=100
# deterministic; NP=100, itermax=30 + n_sim=200 for softmax_rl, the only
# stochastic model here).

library(XSLmodels)
library(purrr)
library(caret)
library(here)

source(here("fits/new_model_specs.R")) # new_model_specs

deoptim_reg <- DEoptim::DEoptim.control(reltol = .001, NP = 100, itermax = 100, trace = FALSE)
deoptim_stoch <- DEoptim::DEoptim.control(reltol = .001, NP = 100, itermax = 30, trace = FALSE)
run_control <- xslControl(n_sim = 200)

data_all <- xsl_datasets
labels_all <- map_chr(data_all, "label")

fit_one <- \(spec, data) {
  dc <- if (spec$stochastic) deoptim_stoch else deoptim_reg
  ctrl <- if (spec$stochastic) run_control else xslControl()
  model <- spec$constructor(spec$lower)
  xsl_fit(model, data, lower = spec$lower, upper = spec$upper,
         control = ctrl, deoptim_control = dc)[[1]]
}
run_one <- \(spec, params, data) {
  ctrl <- if (spec$stochastic) run_control else xslControl()
  xsl_run(spec$constructor(params), data, control = ctrl)
}

out_file <- here("fits/cv_new_models.Rdata")

cv_new <- list()
if (file.exists(out_file)) {
  load(out_file)
  cat("Resuming: found", length(cv_new), "already-completed model(s):",
      paste(names(cv_new), collapse = ", "), "\n")
}

set.seed(123)
folds <- createFolds(labels_all, k = 5, list = TRUE)

remaining <- setdiff(names(new_model_specs), names(cv_new))
cat("Remaining models to fit:", paste(remaining, collapse = ", "), "\n\n")

for (m in remaining) {
  cat(m, "-", format(Sys.time()), "\n")
  spec <- new_model_specs[[m]]
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
  cv_new[[m]] <- list(pars = pars, train_acc = train_acc, test = test_list, testdf = testdf)
  save(cv_new, file = out_file)
  cat("  saved (", length(cv_new), "/", length(new_model_specs), "models done)\n\n")
}

cat("Done. Saved", out_file, "with all", length(cv_new), "models.\n")
