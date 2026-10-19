# Refit uncfam_predictive (FU-P) for v05 with restarts (2026-10-18).
#
# The v05 group fit ended in a poor basin (X = 3.55, B = 15, C = .69; SSE 2.492),
# while CV fold 2's parameters (X = .070, B = .31, C = .999) score .667 on all 47
# conditions. CV folds 1 and 3 ended in the same poor basin. FU-P's objective is
# bimodal, and one DEoptim run (NP = 100, itermax = 100) doesn't reliably find
# the better basin.
#
# This script refits the group fit and CV folds 1 and 3 with N_RESTARTS
# independent DEoptim runs each (random initial populations, refit.R's
# settings and bounds), keeping the best training objective. The group fit
# also gets one run whose initial population is seeded with the 5 CV folds'
# parameters (allowed for the group fit, which sees every condition anyway; NOT
# done for the CV folds, where other folds' parameters were fit to the held-out
# conditions). A refit result replaces the saved one only if its training
# objective is lower, so this can only improve the saved fits. Folds 2, 4 and
# 5, already in the good basin, are left as they are.
#
# Backs up group_fits.Rdata and cv_fits.Rdata (as *.pre_fup_refit.Rdata)
# before writing.
#
# Usage (from xsl-bakeoff/):
#   R_LIBS=fits/v05/Rlib N_WORKERS=8 Rscript fits/v05/refit_fup.R

suppressMessages({
  library(XSLmodels)
  library(purrr)
  library(dplyr)
  library(here)
})
if (!startsWith(find.package("XSLmodels"), here("fits/v05/Rlib"))) stop("run with R_LIBS=fits/v05/Rlib")
source(here("fits/v05/specs.R"))

M          <- "uncfam_predictive"
N_WORKERS  <- as.integer(Sys.getenv("N_WORKERS", "8"))
N_RESTARTS <- as.integer(Sys.getenv("N_RESTARTS", "5"))
FOLDS      <- c(1L, 3L)
DIR        <- here("fits/v05/adults")
options(mc.cores = N_WORKERS)
stamp <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)

spec <- specs_v04[[M]]
data_all <- xsl_datasets
labels <- map_chr(data_all, "label")
stopifnot(length(data_all) == 47)

# Same objective as refit.R's fit_one(): xsl_run()$sse, Inf on error
de_run <- function(data, seed, initialpop = NULL) {
  force(data)
  model <- spec$constructor(spec$lower)
  objective <- function(p) {
    v <- tryCatch(XSLmodels::xsl_run(XSLmodels::update_params(model, p), data)$sse, error = \(e) NA_real_)
    if (is.na(v)) Inf else v
  }
  set.seed(seed)
  DEoptim::DEoptim(objective, lower = spec$lower, upper = spec$upper,
                   control = DEoptim::DEoptim.control(
                     reltol = .001, trace = FALSE, NP = 100, itermax = 100,
                     initialpop = initialpop, parallelType = "parallel",
                     packages = list("XSLmodels")))
}

# Best of N_RESTARTS random-start runs (plus optional seeded runs)
best_of <- function(data, label, base_seed, seeded_pops = list()) {
  runs <- list()
  for (r in seq_len(N_RESTARTS)) {
    t0 <- Sys.time()
    runs[[length(runs) + 1]] <- de_run(data, base_seed + r)
    stamp(sprintf("  %s restart %d/%d: SSE %.4f at %s (%.1f min)", label, r, N_RESTARTS,
                  runs[[length(runs)]]$optim$bestval, paste(signif(runs[[length(runs)]]$optim$bestmem, 3), collapse = ", "),
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  for (s in seq_along(seeded_pops)) {
    runs[[length(runs) + 1]] <- de_run(data, base_seed + 100 + s, initialpop = seeded_pops[[s]])
    stamp(sprintf("  %s seeded run %d: SSE %.4f at %s", label, s, runs[[length(runs)]]$optim$bestval,
                  paste(signif(runs[[length(runs)]]$optim$bestmem, 3), collapse = ", ")))
  }
  vals <- map_dbl(runs, \(x) x$optim$bestval)
  list(best = runs[[which.min(vals)]], all_vals = vals,
       all_pars = map(runs, \(x) unname(x$optim$bestmem)))
}

perf_df <- function(params, data) {
  res <- xsl_run(spec$constructor(params), data)
  list(sse = res$sse,
       df = map(res$fits, \(f) tibble(
         Model = M, condnum = f$data$label, Condition = f$data$condition,
         ModelPerf = as.vector(f$perf), HumanPerf = f$data$accuracy, Nsubj = f$data$n_subj)) |> list_rbind())
}

## ---- back up ---------------------------------------------------------------------
gpath <- file.path(DIR, "group_fits.Rdata"); cpath <- file.path(DIR, "cv_fits.Rdata")
for (p in c(gpath, cpath)) {
  b <- sub("\\.Rdata$", ".pre_fup_refit.Rdata", p)
  if (!file.exists(b)) file.copy(p, b)
}
log_rows <- list()

## ---- group fit ----------------------------------------------------------------------
load(gpath)  # group_fits, gfd, meta
cv_env <- new.env(); load(cpath, envir = cv_env)   # kept apart: it has its own `meta`
old_group <- group_fits[[M]]$optim$bestval
stamp("group: old SSE ", signif(old_group, 4), " at ", paste(signif(group_fits[[M]]$optim$bestmem, 3), collapse = ", "))
# seeded population: the 5 CV fold optima, plus random points for the rest
fold_pars <- do.call(rbind, map(cv_env$cv_fits[[M]]$folds, \(f) unname(f$par)))
set.seed(7)
rand <- sapply(seq_along(spec$lower), \(j) runif(100 - nrow(fold_pars), spec$lower[j], spec$upper[j]))
g <- best_of(data_all, "group", 2026, seeded_pops = list(rbind(fold_pars, rand)))
log_rows$group <- tibble(fit = "group", old = old_group, new = g$best$optim$bestval,
                         runs = paste(sprintf("%.4f", g$all_vals), collapse = " "))
if (g$best$optim$bestval < old_group) {
  group_fits[[M]] <- g$best
  pd <- perf_df(g$best$optim$bestmem, data_all)
  gfd <- bind_rows(filter(gfd, Model != M), pd$df)
  meta$updated <- format(Sys.time())
  meta$fup_refit <- "2026-10-18: uncfam_predictive refit with restarts (fits/v05/refit_fup.R)"
  save(group_fits, gfd, meta, file = gpath)
  stamp("group: replaced (", signif(old_group, 4), " -> ", signif(g$best$optim$bestval, 4), ")")
} else stamp("group: kept the old fit")

## ---- CV folds 1 and 3 -------------------------------------------------------------
load(cpath)  # cv_fits, folds, meta (the CV file's own)
for (i in FOLDS) {
  old <- cv_fits[[M]]$folds[[i]]
  stamp("cv fold ", i, ": old train SSE ", signif(old$train_sse, 4), " at ", paste(signif(old$par, 3), collapse = ", "))
  split <- get_train_test_split(folds[[i]], data_all)
  stopifnot(setequal(labels[folds[[i]]], old$test_conditions))
  b <- best_of(split$train, paste("fold", i), 2026 + 10 * i)
  log_rows[[paste0("fold", i)]] <- tibble(fit = paste("cv fold", i), old = old$train_sse, new = b$best$optim$bestval,
                                          runs = paste(sprintf("%.4f", b$all_vals), collapse = " "))
  if (b$best$optim$bestval < old$train_sse) {
    pd <- perf_df(b$best$optim$bestmem, split$test)
    cv_fits[[M]]$folds[[i]] <- list(par = b$best$optim$bestmem, train_sse = b$best$optim$bestval,
                                    test_sse = pd$sse, testdf = pd$df, test_conditions = old$test_conditions)
    stamp(sprintf("cv fold %d: replaced (train %.4f -> %.4f; test %.4f -> %.4f)", i, old$train_sse,
                  b$best$optim$bestval, old$test_sse, pd$sse))
  } else stamp("cv fold ", i, ": kept the old fit")
}
cv_fits[[M]]$pars   <- do.call(rbind, map(cv_fits[[M]]$folds, "par"))
cv_fits[[M]]$testdf <- list_rbind(map(cv_fits[[M]]$folds, "testdf"))
meta$updated <- format(Sys.time())
meta$fup_refit <- "2026-10-18: uncfam_predictive folds 1 and 3 refit with restarts (fits/v05/refit_fup.R)"
save(cv_fits, folds, meta, file = cpath)

log_tab <- list_rbind(log_rows)
saveRDS(log_tab, file.path(DIR, "fup_refit_log.rds"))
print(log_tab)
stamp("FU-P refit done")
