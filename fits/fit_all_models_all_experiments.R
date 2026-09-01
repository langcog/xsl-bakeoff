# Fits every model in model_specs.R (the paper's 11 models, plus baseline
# and the fazlyt bonus) to ALL conditions currently in xsl_datasets -- not
# just the original 44-condition subset recreate_fits_with_package.R
# targets. As of this writing that's 53 conditions (the original 44 plus 9
# added later: temporal contiguity, Suanda 2014, Koehne 2013).
#
# Group-level only (all conditions fit jointly, no held-out folds). For
# cross-validated fits on the same 53 conditions, see cv_all_experiments.R
# (a separate script, since CV is ~5x the cost and worth running
# independently/overnight).
#
# Settings: close to the original paper's DEoptim settings (NP=100,
# itermax=100 for deterministic models; NP=100, itermax=30 for stochastic
# models), but with n_sim reduced from the original 500 to 200 -- the
# stochastic models' cost scales directly with n_sim, and 500 is what made
# the equivalent CV run (see recreate_full_run.log) take multiple days
# without finishing.
#
# Does NOT include decay/multi_sampling/tilles (XSLmodels registers 12
# models via show_models(), but the paper only fit 11 + baseline; "decay"
# was explicitly dropped by the original author as "barely better than
# baseline", "multi_sampling" wasn't part of the paper at all, and
# "tilles" is known to be numerically unstable -- see model_specs.R and
# XSLmodels' own model registry for details). Add specs for these to
# model_specs.R first if you want them included here too.

library(XSLmodels)
library(purrr)
library(here)

source(here("fits/model_specs.R")) # model_specs, fazlyt_spec

np_reg <- 100; itermax_reg <- 100 # matches fit_model()'s DEoptim.control
np_stoch <- 100; itermax_stoch <- 30 # matches fit_stochastic_model()'s
n_sim <- 200 # original was 500; reduced for a practical runtime

deoptim_reg <- DEoptim::DEoptim.control(reltol = .001, NP = np_reg, itermax = itermax_reg, trace = FALSE)
deoptim_stoch <- DEoptim::DEoptim.control(reltol = .001, NP = np_stoch, itermax = itermax_stoch, trace = FALSE)
run_control <- xslControl(n_sim = n_sim)

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
  model <- spec$constructor(params)
  ctrl <- if (spec$stochastic) run_control else xslControl()
  xsl_run(model, data, control = ctrl)
}

## ---- Group fits (all conditions fit jointly), with incremental saves ----
# saved after each model, not just at the end, so an interruption doesn't
# lose everything (the earlier 44-condition CV run had no such
# checkpointing and lost several days of progress when its session died)

group_fits_all <- list()
checkpoint <- \() save(group_fits_all, file = here("fits/group_fits_all_experiments.Rdata"))

cat("Fitting group models (11, from model_specs.R)...\n")
for (m in names(model_specs)) {
  cat(" ", m, "-", format(Sys.time()), "\n")
  group_fits_all[[m]] <- fit_one(model_specs[[m]], data_all)
  checkpoint()
}

cat("  baseline -", format(Sys.time()), "\n")
group_fits_all[["baseline"]] <- xsl_fit(baseline(), data_all, lower = numeric(0), upper = numeric(0))[[1]]
checkpoint()

cat("  fazlyt (bonus) -", format(Sys.time()), "\n")
group_fits_all[["fazlyt"]] <- fit_one(fazlyt_spec, data_all)
checkpoint()

# gfd_all: one row per (model, condition, item), same shape
# get_model_dataframe() produced originally (Model, condnum, Condition,
# ModelPerf, HumanPerf, Nsubj)
build_gfd <- \(fits, specs, data) {
  rows <- map(names(fits), \(m) {
    spec <- if (m == "baseline") list(constructor = \(p) baseline(), stochastic = FALSE) else
      if (m == "fazlyt") fazlyt_spec else specs[[m]]
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
cat("Building gfd_all...\n")
gfd_all <- build_gfd(group_fits_all, model_specs, data_all)

save(group_fits_all, gfd_all, file = here("fits/group_fits_all_experiments.Rdata"))
cat("Saved fits/group_fits_all_experiments.Rdata\n")
