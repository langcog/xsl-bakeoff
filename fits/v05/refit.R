# fits/v05 (2026-10-14): a copy of fits/v04/refit.R and specs.R (paths
# changed from fits/v04 to fits/v05, nothing else), run against XSLmodels
# master after PR #29, which corrected the 12 filtering conditions'
# (filt*E_*L) test scoring and added their AtoB tests (24 test items each;
# n_subj 27 -> 23 in the *_6L conditions). Every other condition, model, and
# setting is unchanged from v04, so v05 differs from v04 only through the
# data: the filtering fix (PR #29), and PR #30, which rebuilt the frequency /
# contextual-diversity conditions from source, dropped duplicates orig_3x3
# and orig_4x4, and moved Suanda/Koehne to the held-out sets (47 conditions).
# The package version is still 0.4.0; the pinned commit
# is in fits/v05/XSLMODELS_COMMIT.
#
# Round-2 refit for the journal paper: every model in fits/v05/specs.R, fit
# to all of xsl_datasets (56 conditions) with XSLmodels pinned at
# fits/v05/XSLMODELS_COMMIT (0.4.0).
#
# Usage (from xsl-bakeoff/; install the pinned package first with
# fits/v05/install_pinned_xslmodels.sh):
#
#   R_LIBS=fits/v05/Rlib Rscript fits/v05/refit.R group  [model ...]
#   R_LIBS=fits/v05/Rlib Rscript fits/v05/refit.R cv     [model ...]
#
# R_LIBS (not .libPaths()) matters: DEoptim's parallel workers are fresh R
# processes and only see the pinned package if they inherit it from the
# environment. The script refuses to run against any other version.
#
# Env vars: N_WORKERS (DEoptim parallel workers, default 10), N_SIM
# (simulated learners per evaluation of a stochastic model, default 200),
# QUICK=1 (tiny DEoptim budget + n_sim, for smoke tests only; writes to
# fits/v05/quick/ instead).
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
# Outputs (fits/v05/):
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

pinned <- trimws(readLines(here("fits/v05/XSLMODELS_COMMIT")))
if (as.character(packageVersion("XSLmodels")) != "0.4.0" ||
    !startsWith(find.package("XSLmodels"), here("fits/v05/Rlib"))) {
  stop("XSLmodels 0.4.0 from fits/v05/Rlib is not first on the library path ",
       "(found ", packageVersion("XSLmodels"), " at ", find.package("XSLmodels"),
       "). Run with R_LIBS=fits/v05/Rlib.")
}

source(here("fits/v05/specs.R"))  # specs_v04, GROUP_ONLY, RUN_ORDER

QUICK     <- identical(Sys.getenv("QUICK"), "1")
N_WORKERS <- as.integer(Sys.getenv("N_WORKERS", "10"))
N_SIM     <- if (QUICK) 5L else as.integer(Sys.getenv("N_SIM", "200"))
# FIT_SET: v05 fits one set, "adults" = all 47 xsl_datasets conditions (output
# fits/v05/adults/). After XSLmodels PR #30, xsl_datasets holds only adult
# conditions with item-level data: Suanda et al. 2014 (the only child
# conditions) and Koehne et al. 2013 have condition-level means only and are
# now held-out generalization tests (suanda2014, koehne2013), as in the
# original CogSci bake-off. The v04 "children" fit set (Suanda + benitez2020
# kids) is dropped (2026-10-14); children are scored only by generalization.
FIT_SET   <- Sys.getenv("FIT_SET", "adults")
stopifnot(FIT_SET == "adults")
BASE_DIR  <- here("fits/v05", FIT_SET)
OUT_DIR   <- if (QUICK) file.path(BASE_DIR, "quick") else BASE_DIR
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# DEoptim's managed parallel backend sizes its cluster from
# parallelly::availableCores(), which honours options(mc.cores)
options(mc.cores = N_WORKERS)

models <- if (length(args) > 1) args[-1] else RUN_ORDER
stopifnot(all(models %in% names(specs_v04)))
if (phase == "cv") models <- setdiff(models, GROUP_ONLY)

data_all <- xsl_datasets
labels   <- map_chr(data_all, "label")
stopifnot(length(data_all) == 47, !anyDuplicated(labels),
          !any(grepl("^Suanda2014|^Koehne2013|^orig_[34]x[34]$", labels)))

meta <- list(xslmodels_commit = pinned, xslmodels_version = "0.4.0",
             fit_set = FIT_SET, n_conditions = length(data_all), conditions = labels,
             n_sim = N_SIM, quick = QUICK,
             deoptim = if (QUICK) "NP=8, itermax=2" else "reltol=.001, NP=100, itermax=100 (det) / 30 (stoch)",
             started = format(Sys.time()))

# QUICK shrinks the budget but keeps the SAME parallel backend as a real run,
# so a smoke test exercises the worker path. (A 2026-10-03 run had every
# worker-side evaluation fail while a serial QUICK test had passed; see
# fits/v05/failed_2026-10-03/.)
deoptim_ctrl <- function(stochastic) {
  DEoptim::DEoptim.control(
    reltol = .001, trace = FALSE,
    NP = if (QUICK) 8 else 100,
    itermax = if (QUICK) 2 else if (stochastic) 30 else 100,
    parallelType = "parallel",
    packages = list("XSLmodels"))
}
run_ctrl <- function(spec) if (spec$stochastic) xslControl(n_sim = N_SIM) else xslControl()

# Worker-side objective errors are appended here (one line per failed
# evaluation) instead of vanishing: xsl_fit() turns any error into Inf, so a
# run whose every evaluation fails otherwise "succeeds" with bestval = Inf.
ERR_FILE <- file.path(OUT_DIR, "refit_objective_errors.log")

# Same objective as XSLmodels::xsl_fit() -- SSE from xsl_run() at the
# proposed parameters, Inf if that errors or is NA -- but logging errors, and
# refusing to return a fit whose best objective isn't finite.
fit_one <- function(spec, data, label = "") {
  # DEoptim ships `objective`'s closure to the PSOCK workers; an unforced
  # argument is a promise that the worker would evaluate in its own empty
  # global env ("object 'data_all' not found") -- the cause of the
  # 2026-10-03 all-Inf run
  force(data); force(label)
  model <- spec$constructor(spec$lower)  # placeholder; only its shape is used
  ctrl <- run_ctrl(spec)
  err_file <- ERR_FILE
  objective <- function(p) {
    sse <- tryCatch(
      XSLmodels::xsl_run(XSLmodels::update_params(model, p), data, control = ctrl)$sse,
      error = function(e) {
        cat(format(Sys.time(), "%F %T"), " pid ", Sys.getpid(), " [", label, "] p = ",
            paste(signif(p, 4), collapse = ","), " :: ", conditionMessage(e), "\n",
            sep = "", file = err_file, append = TRUE)
        NA_real_
      })
    if (is.na(sse)) Inf else sse
  }
  fit <- if (length(spec$lower) == 0) {
    list(optim = list(bestmem = numeric(0), bestval = objective(numeric(0))))
  } else {
    DEoptim::DEoptim(objective, lower = spec$lower, upper = spec$upper,
                     control = deoptim_ctrl(spec$stochastic))
  }
  if (!is.finite(fit$optim$bestval)) {
    stop("fit [", label, "] has a non-finite best objective -- every evaluation failed. ",
         "First errors:\n", paste(utils::head(readLines(err_file, warn = FALSE), 3), collapse = "\n"),
         "\n(full log: ", err_file, ")")
  }
  fit
}

# Pre-flight: before any real fitting, confirm DEoptim's parallel workers load
# the pinned XSLmodels and can evaluate a real objective. Fails fast instead of
# spending hours producing Inf fits.
preflight <- function() {
  pinned_path <- find.package("XSLmodels")
  probe <- DEoptim::DEoptim(function(p) {
    if (find.package("XSLmodels") != pinned_path)
      stop("worker loaded XSLmodels from ", find.package("XSLmodels"), ", not ", pinned_path)
    sum(p^2)
  }, lower = c(-1, -1), upper = c(1, 1),
  control = DEoptim::DEoptim.control(NP = 2 * N_WORKERS, itermax = 1, trace = FALSE,
                                     parallelType = "parallel", packages = list("XSLmodels")))
  spec <- specs_v04$fazly
  tiny <- spec; tiny$stochastic <- FALSE
  old_quick <- QUICK; QUICK <<- TRUE; on.exit(QUICK <<- old_quick)
  f <- fit_one(tiny, data_all[1:3], label = "preflight fazly")
  stamp("preflight OK: ", N_WORKERS, " workers load ", pinned_path,
        "; tiny fazly fit bestval = ", signif(f$optim$bestval, 3))
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

preflight()

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
    group_fits[[m]] <- fit_one(spec, data_all, label = m)
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
  folds <- if (FIT_SET == "children") as.list(seq_along(data_all)) else
    caret::createFolds(labels, k = 5, list = TRUE)
  cv_fits <- list()
  if (file.exists(out)) load(out)
  for (m in models) {
    spec <- specs_v04[[m]]
    if (is.null(cv_fits[[m]])) cv_fits[[m]] <- list(folds = list())
    for (i in seq_along(folds)) {
      if (length(cv_fits[[m]]$folds) >= i && !is.null(cv_fits[[m]]$folds[[i]])) next
      stamp("cv     ", m, "  fold ", i, "/", length(folds))
      t0 <- Sys.time()
      split <- get_train_test_split(folds[[i]], data_all)
      set.seed(2026 + i)
      fit <- fit_one(spec, split$train, label = paste0(m, " fold ", i))
      pd <- perf_df(m, spec, fit$optim$bestmem, split$test)
      cv_fits[[m]]$folds[[i]] <- list(par = fit$optim$bestmem, train_sse = fit$optim$bestval,
                                      test_sse = pd$sse, testdf = pd$df,
                                      test_conditions = labels[folds[[i]]])
      if (length(compact(cv_fits[[m]]$folds)) == length(folds)) {
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
        " models with all ", length(folds), " folds in ", out)
}
