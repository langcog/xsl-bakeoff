# Group fit and 5-fold CV of the trial-level Yurovsky & Frank (2015) model,
# yf2015, yf2015_me and their fixed-gamma versions (fits/v06/yf2015/specs_yf2015.R), against XSLmodels
# at fits/v06/yf2015/XSLMODELS_COMMIT (PR #46, branch add-yurovsky-frank2015).
#
# Same procedure as fits/v05/rwkf/refit_rwkf.R: the 47 xsl_datasets
# conditions, the same 5 CV folds as fits/v05/adults/cv_fits.Rdata, refit.R's
# objective (n_subj-weighted SSE) and DEoptim settings (NP = 100, itermax =
# 100, reltol = .001), best of N_RESTARTS runs. Progress is saved after every
# fit (fits/v06/yf2015/yf2015_fits.Rdata), so the script is resumable.
#
# Unlike refit_rwkf.R it does NOT merge into fits/v05/adults/*.Rdata (which
# the paper loads); it writes fits/v06/yf2015/yf2015_refit_summary.rds,
# comparing against the v05 models' group and CV SSE.
#
# Usage (from xsl-bakeoff/):
#   R_LIBS=$(pwd)/fits/v06/yf2015/Rlib N_WORKERS=10 Rscript fits/v06/yf2015/refit_yf2015.R [model ...]

suppressMessages({
  library(XSLmodels)
  library(purrr)
  library(dplyr)
  library(here)
})
DIR  <- here("fits/v06/yf2015")
ADIR <- here("fits/v05/adults")
COMMIT <- trimws(readLines(file.path(DIR, "XSLMODELS_COMMIT")))
if (!startsWith(find.package("XSLmodels"), file.path(DIR, "Rlib"))) {
  stop("run with R_LIBS=$(pwd)/fits/v06/yf2015/Rlib (found ", find.package("XSLmodels"), ")")
}
source(file.path(DIR, "specs_yf2015.R"))

args       <- commandArgs(trailingOnly = TRUE)
MODELS     <- if (length(args)) args else YF_MODELS
stopifnot(all(MODELS %in% YF_MODELS))
N_WORKERS  <- as.integer(Sys.getenv("N_WORKERS", "10"))
N_RESTARTS <- as.integer(Sys.getenv("N_RESTARTS", "3"))
options(mc.cores = N_WORKERS)
stamp <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)

data_all <- xsl_datasets
labels <- map_chr(data_all, "label")
stopifnot(length(data_all) == 47)
cv_env <- new.env(); load(file.path(ADIR, "cv_fits.Rdata"), envir = cv_env)
folds <- cv_env$folds
stopifnot(length(folds) == 5)

ERR_FILE <- file.path(DIR, "refit_objective_errors.log")
de_run <- function(spec, data, seed, label) {
  force(data); force(label)
  model <- spec$constructor(spec$lower)
  err_file <- ERR_FILE
  objective <- function(p) {
    v <- tryCatch(XSLmodels::xsl_run(spec$constructor(p), data)$sse,
                  error = function(e) {
                    cat(format(Sys.time(), "%F %T"), " [", label, "] p = ", paste(signif(p, 4), collapse = ","),
                        " :: ", conditionMessage(e), "\n", sep = "", file = err_file, append = TRUE)
                    NA_real_
                  })
    if (is.na(v)) Inf else v
  }
  set.seed(seed)
  fit <- DEoptim::DEoptim(objective, lower = spec$lower, upper = spec$upper,
                          control = DEoptim::DEoptim.control(
                            reltol = .001, trace = FALSE, NP = 100, itermax = 100,
                            parallelType = "parallel", packages = list("XSLmodels")))
  if (!is.finite(fit$optim$bestval)) stop("[", label, "] every evaluation failed; see ", err_file)
  fit
}
best_of <- function(spec, data, label, base_seed) {
  runs <- map(seq_len(N_RESTARTS), function(r) {
    t0 <- Sys.time()
    f <- de_run(spec, data, base_seed + r, paste(label, "run", r))
    stamp(sprintf("  %s run %d/%d: SSE %.4f at %s (%.1f min)", label, r, N_RESTARTS, f$optim$bestval,
                  paste(signif(f$optim$bestmem, 3), collapse = ", "), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    f
  })
  vals <- map_dbl(runs, \(x) x$optim$bestval)
  list(best = runs[[which.min(vals)]], all_vals = vals, all_pars = map(runs, \(x) unname(x$optim$bestmem)))
}
perf_df <- function(m, params, data) {
  res <- xsl_run(specs_yf2015[[m]]$constructor(params), data)
  list(sse = res$sse,
       df = map(res$fits, \(f) tibble(
         Model = m, condnum = f$data$label, Condition = f$data$condition,
         ModelPerf = as.vector(f$perf), HumanPerf = f$data$accuracy, Nsubj = f$data$n_subj)) |> list_rbind())
}

## ---- fits (resumable) ---------------------------------------------------------------
prog_file <- file.path(DIR, "yf2015_fits.Rdata")
prog <- list()
if (file.exists(prog_file)) load(prog_file)  # prog
save_prog <- function() save(prog, file = prog_file)

for (m in MODELS) {
  spec <- specs_yf2015[[m]]
  if (is.null(prog[[m]])) prog[[m]] <- list(group = NULL, folds = list())
  if (is.null(prog[[m]]$group)) {
    stamp(m, ": group (", N_RESTARTS, " restarts, ", N_WORKERS, " workers)")
    b <- best_of(spec, data_all, paste(m, "group"), 2026)
    prog[[m]]$group <- c(b, list(perf = perf_df(m, b$best$optim$bestmem, data_all)))
    save_prog()
    stamp(m, ": group done, SSE ", signif(b$best$optim$bestval, 4))
  }
  for (i in seq_along(folds)) {
    if (length(prog[[m]]$folds) >= i && !is.null(prog[[m]]$folds[[i]])) next
    stamp(m, ": cv fold ", i, "/5")
    split <- get_train_test_split(folds[[i]], data_all)
    b <- best_of(spec, split$train, paste(m, "fold", i), 2026 + 10 * i)
    pd <- perf_df(m, b$best$optim$bestmem, split$test)
    prog[[m]]$folds[[i]] <- list(par = b$best$optim$bestmem, train_sse = b$best$optim$bestval,
                                 test_sse = pd$sse, testdf = pd$df, test_conditions = labels[folds[[i]]],
                                 all_vals = b$all_vals, all_pars = b$all_pars)
    save_prog()
    stamp(sprintf("%s: cv fold %d done, train %.4f, test %.4f", m, i, b$best$optim$bestval, pd$sse))
  }
}

## ---- summary, against the v05 models (read-only) ---------------------------------
done <- names(Filter(\(x) !is.null(x$group) && length(Filter(Negate(is.null), x$folds)) == 5, prog))
wsse <- \(df) df |> group_by(condnum) |> summarise(s = sum((ModelPerf - HumanPerf)^2), n = first(Nsubj)) |>
  summarise(v = sum(s * n) / sum(n)) |> pull(v)
g_env <- new.env(); load(file.path(ADIR, "group_fits.Rdata"), envir = g_env)
v05 <- tibble(model = names(cv_env$cv_fits),
              group_sse = map_dbl(model, \(m) if (is.null(g_env$group_fits[[m]])) NA_real_ else g_env$group_fits[[m]]$optim$bestval),
              cv_sse = map_dbl(model, \(m) if (is.null(cv_env$cv_fits[[m]]$testdf)) NA_real_ else wsse(cv_env$cv_fits[[m]]$testdf)),
              source = "v05")
yf <- tibble(model = done,
             group_sse = map_dbl(done, \(m) prog[[m]]$group$best$optim$bestval),
             cv_sse = map_dbl(done, \(m) wsse(list_rbind(map(prog[[m]]$folds, "testdf")))),
             group_par = map_chr(done, \(m) paste(signif(prog[[m]]$group$best$optim$bestmem, 3), collapse = ", ")),
             group_runs = map_chr(done, \(m) paste(sprintf("%.4f", prog[[m]]$group$all_vals), collapse = " ")),
             source = paste("yf2015 @", substr(COMMIT, 1, 7)))
summary_tab <- bind_rows(yf, v05) |> arrange(cv_sse)
saveRDS(summary_tab, file.path(DIR, "yf2015_refit_summary.rds"))
print(as.data.frame(summary_tab), digits = 4)
stamp("done: ", paste(done, collapse = ", "))
