# Round-2 refit for the journal paper: every model in fits/v04/specs.R, fit
# to all of xsl_datasets (56 conditions) with XSLmodels pinned at
# fits/v04/XSLMODELS_COMMIT (0.4.0).
#
# Usage (from xsl-bakeoff/; install the pinned package first with
# fits/v04/install_pinned_xslmodels.sh):
#
#   R_LIBS=fits/v04/Rlib Rscript fits/v04/refit.R group  [model ...]
#   R_LIBS=fits/v04/Rlib Rscript fits/v04/refit.R cv     [model ...]
#
# R_LIBS (not .libPaths()) matters: DEoptim's parallel workers are fresh R
# processes and only see the pinned package if they inherit it from the
# environment. The script refuses to run against any other version.
#
# Env vars: N_WORKERS (DEoptim parallel workers, default 10), N_SIM
# (simulated learners per evaluation of a stochastic model, default 200),
# QUICK=1 (tiny DEoptim budget + n_sim, for smoke tests only; writes to
# fits/v04/quick/ instead).
#
# Settings otherwise match round 1 (fits/fit_all_models_all_experiments.R,
# fits/cv_all_experiments.R): DEoptim reltol = .001, NP = 100, itermax = 100
# (deterministic) or 30 (stochastic); 5-fold CV over conditions via
# caret::createFolds(labels, k = 5) after set.seed(123). With 56 conditions
# the folds necessarily differ from round 1's 53-condition folds.
#
# Checkpointing / resume: the group phase saves after every model and the CV
# phase after every FOLD; re-running skips whatever is already saved.
# Stochastic models are not bit-reproducible across runs (parallel workers
# draw their own RNG streams), as is inherent to parallel simulation.
#
# Outputs (fits/v04/):
#   group_fits.Rdata -- group_fits (named list of DEoptim results),
#                       gfd (Model, condnum, Condition, ModelPerf, HumanPerf, Nsubj),
#                       meta
#   cv_fits.Rdata    -- cv_fits (per model: $folds[[i]] = list(par, train_sse,
#                       test_sse, testdf), and once all 5 folds exist,
#                       $pars, $testdf), meta

suppressMessages({
  library(XSLmodels)
  library(purrr)
  library(dplyr)
  library(here)
})

args <- commandArgs(trailingOnly = TRUE)
phase <- args[1]
stopifnot(phase %in% c("group", "cv"))

pinned <- trimws(readLines(here("fits/v04/XSLMODELS_COMMIT")))
if (as.character(packageVersion("XSLmodels")) != "0.4.0" ||
    !startsWith(find.package("XSLmodels"), here("fits/v04/Rlib"))) {
  stop("XSLmodels 0.4.0 from fits/v04/Rlib is not first on the library path ",
       "(found ", packageVersion("XSLmodels"), " at ", find.package("XSLmodels"),
       "). Run with R_LIBS=fits/v04/Rlib.")
}

source(here("fits/v04/specs.R"))  # specs_v04, GROUP_ONLY, RUN_ORDER

QUICK     <- identical(Sys.getenv("QUICK"), "1")
N_WORKERS <- as.integer(Sys.getenv("N_WORKERS", "10"))
N_SIM     <- if (QUICK) 5L else as.integer(Sys.getenv("N_SIM", "200"))
OUT_DIR   <- if (QUICK) here("fits/v04/quick") else here("fits/v04")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# DEoptim's managed parallel backend sizes its cluster from
# parallelly::availableCores(), which honours options(mc.cores)
options(mc.cores = N_WORKERS)

models <- if (length(args) > 1) args[-1] else RUN_ORDER
stopifnot(all(models %in% names(specs_v04)))
if (phase == "cv") models <- setdiff(models, GROUP_ONLY)

data_all <- xsl_datasets
labels   <- map_chr(data_all, "label")
stopifnot(length(data_all) == 56)

meta <- list(xslmodels_commit = pinned, xslmodels_version = "0.4.0",
             n_conditions = length(data_all), n_sim = N_SIM, quick = QUICK,
             deoptim = if (QUICK) "NP=8, itermax=2" else "reltol=.001, NP=100, itermax=100 (det) / 30 (stoch)",
             started = format(Sys.time()))

deoptim_ctrl <- function(stochastic) {
  DEoptim::DEoptim.control(
    reltol = .001, trace = FALSE,
    NP = if (QUICK) 8 else 100,
    itermax = if (QUICK) 2 else if (stochastic) 30 else 100,
    parallelType = if (QUICK) "none" else "parallel",
    packages = list("XSLmodels"))
}
run_ctrl <- function(spec) if (spec$stochastic) xslControl(n_sim = N_SIM) else xslControl()

fit_one <- function(spec, data) {
  model <- spec$constructor(spec$lower)  # placeholder; xsl_fit only needs its shape
  xsl_fit(model, data, lower = spec$lower, upper = spec$upper,
          control = run_ctrl(spec), deoptim_control = deoptim_ctrl(spec$stochastic))[[1]]
}

perf_df <- function(name, spec, params, data) {
  res <- xsl_run(spec$constructor(params), data, control = run_ctrl(spec))
  list(sse = res$sse,
       df = map(res$fits, \(f) tibble(
         Model = name, condnum = f$data$label, Condition = f$data$condition,
         ModelPerf = as.vector(f$perf), HumanPerf = f$data$accuracy, Nsubj = f$data$n_subj
       )) |> list_rbind())
}

stamp <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)

## ---------------------------------------------------------------------------
## group phase
## ---------------------------------------------------------------------------
if (phase == "group") {
  out <- file.path(OUT_DIR, "group_fits.Rdata")
  group_fits <- list(); gfd <- tibble()
  if (file.exists(out)) load(out)
  for (m in setdiff(models, names(group_fits))) {
    spec <- specs_v04[[m]]
    stamp("group  ", m, " (", length(spec$lower), " params, ",
          if (spec$stochastic) "stochastic" else "deterministic", ")")
    t0 <- Sys.time()
    set.seed(2026)
    group_fits[[m]] <- fit_one(spec, data_all)
    pd <- perf_df(m, spec, group_fits[[m]]$optim$bestmem, data_all)
    gfd <- bind_rows(if (nrow(gfd)) filter(gfd, Model != m), pd$df)
    meta$updated <- format(Sys.time())
    save(group_fits, gfd, meta, file = out)
    stamp("  done in ", format(round(difftime(Sys.time(), t0, units = "mins"), 1)),
          "  bestval = ", signif(group_fits[[m]]$optim$bestval, 4),
          "  bestmem = ", paste(signif(group_fits[[m]]$optim$bestmem, 3), collapse = ", "))
  }
  stamp("group phase complete: ", length(group_fits), " models in ", out)
}

## ---------------------------------------------------------------------------
## cv phase
## ---------------------------------------------------------------------------
if (phase == "cv") {
  out <- file.path(OUT_DIR, "cv_fits.Rdata")
  set.seed(123)
  folds <- caret::createFolds(labels, k = 5, list = TRUE)
  cv_fits <- list()
  if (file.exists(out)) load(out)
  for (m in models) {
    spec <- specs_v04[[m]]
    if (is.null(cv_fits[[m]])) cv_fits[[m]] <- list(folds = list())
    for (i in seq_along(folds)) {
      if (length(cv_fits[[m]]$folds) >= i && !is.null(cv_fits[[m]]$folds[[i]])) next
      stamp("cv     ", m, "  fold ", i, "/5")
      t0 <- Sys.time()
      split <- get_train_test_split(folds[[i]], data_all)
      set.seed(2026 + i)
      fit <- fit_one(spec, split$train)
      pd <- perf_df(m, spec, fit$optim$bestmem, split$test)
      cv_fits[[m]]$folds[[i]] <- list(par = fit$optim$bestmem, train_sse = fit$optim$bestval,
                                      test_sse = pd$sse, testdf = pd$df,
                                      test_conditions = labels[folds[[i]]])
      if (length(compact(cv_fits[[m]]$folds)) == 5) {
        cv_fits[[m]]$pars   <- do.call(rbind, map(cv_fits[[m]]$folds, "par"))
        cv_fits[[m]]$testdf <- list_rbind(map(cv_fits[[m]]$folds, "testdf"))
      }
      meta$updated <- format(Sys.time())
      save(cv_fits, folds, meta, file = out)
      stamp("  done in ", format(round(difftime(Sys.time(), t0, units = "mins"), 1)),
            "  train = ", signif(fit$optim$bestval, 4), "  test = ", signif(pd$sse, 4))
    }
  }
  stamp("cv phase complete: ", sum(map_lgl(cv_fits, \(x) !is.null(x$testdf))),
        " models with all 5 folds in ", out)
}
