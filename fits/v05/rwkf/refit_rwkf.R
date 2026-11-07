# Refit the fixed rescorla-wagner and fit the error-driven redesigns for v05,
# against XSLmodels at fits/v05/rwkf/XSLMODELS_COMMIT (master after PRs
# #41-#42, 2026-11-05).
#
# XSLmodels PR #41 fixed rescorla_wagner()'s update: each trial's per-object
# prediction errors were added to the word x object submatrix as a vector,
# which R recycled down the columns, so word i got the i-th object's error
# for every object on the trial. Luce choice then reproduced co-occurrence
# proportions, which is why the v05 rescorla-wagner fit (e70c10c) matched
# baseline. PR #42 added the canonical options (absent objects as outcomes,
# a full-covariance Kalman filter) and a softmax choice rule (phi). For the
# models in specs.R, the only other change since the v05 pin is tilles' (its
# own refit, fits/v05/tilles/), so every other v05 fit stands.
#
# Models (specs.R's RWKF_MODELS): rescorla-wagner (fixed, v05 spec),
# rescorla-wagner_softmax, rescorla-wagner_discriminative, kalman_filter_full,
# and the control baseline_softmax (co-occurrence counts with the same
# softmax rule). Each gets a group fit plus all 5 CV folds (the same folds as
# fits/v05/adults/cv_fits.Rdata), with refit.R's objective (n_subj-weighted
# SSE) and DEoptim settings (NP = 100, itermax = 100, reltol = .001), keeping
# the best of N_RESTARTS runs, as for tilles. Progress is saved after every
# fit (fits/v05/rwkf/rwkf_fits.Rdata), so the script is resumable.
#
# Once a model's six fits exist, its entries in fits/v05/adults/group_fits.Rdata
# and cv_fits.Rdata are added or replaced (backups: *.pre_rwkf_refit.Rdata),
# and each file's meta records which models come from this commit.
#
# Usage (from xsl-bakeoff/):
#   R_LIBS=$(pwd)/fits/v05/rwkf/Rlib N_WORKERS=10 Rscript fits/v05/rwkf/refit_rwkf.R [model ...]

suppressMessages({
  library(XSLmodels)
  library(purrr)
  library(dplyr)
  library(here)
})
DIR  <- here("fits/v05/rwkf")
ADIR <- here("fits/v05/adults")
COMMIT <- trimws(readLines(file.path(DIR, "XSLMODELS_COMMIT")))
if (!startsWith(find.package("XSLmodels"), file.path(DIR, "Rlib"))) {
  stop("run with R_LIBS=$(pwd)/fits/v05/rwkf/Rlib (found ", find.package("XSLmodels"), ")")
}
source(here("fits/v05/specs.R"))

args       <- commandArgs(trailingOnly = TRUE)
MODELS     <- if (length(args)) args else RWKF_MODELS
stopifnot(all(MODELS %in% RWKF_MODELS))
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
    v <- tryCatch(XSLmodels::xsl_run(XSLmodels::update_params(model, p), data)$sse,
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
  res <- xsl_run(specs_v04[[m]]$constructor(params), data)
  list(sse = res$sse,
       df = map(res$fits, \(f) tibble(
         Model = m, condnum = f$data$label, Condition = f$data$condition,
         ModelPerf = as.vector(f$perf), HumanPerf = f$data$accuracy, Nsubj = f$data$n_subj)) |> list_rbind())
}

## ---- fits (resumable) ---------------------------------------------------------------
prog_file <- file.path(DIR, "rwkf_fits.Rdata")
prog <- list()
if (file.exists(prog_file)) load(prog_file)  # prog
save_prog <- function() save(prog, file = prog_file)

for (m in MODELS) {
  spec <- specs_v04[[m]]
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

## ---- merge into the v05 fit files --------------------------------------------------
done <- names(Filter(\(x) !is.null(x$group) && length(Filter(Negate(is.null), x$folds)) == 5, prog))
for (p in file.path(ADIR, c("group_fits.Rdata", "cv_fits.Rdata"))) {
  b <- sub("\\.Rdata$", ".pre_rwkf_refit.Rdata", p)
  if (!file.exists(b)) file.copy(p, b)
}
note <- paste0(paste(done, collapse = ", "), " fit 2026-11-05 against XSLmodels ", COMMIT,
               " (PRs #41-#42; fits/v05/rwkf/refit_rwkf.R)")

load(file.path(ADIR, "group_fits.Rdata"))  # group_fits, gfd, meta
old_group <- map_dbl(set_names(done), \(m) if (is.null(group_fits[[m]])) NA_real_ else group_fits[[m]]$optim$bestval)
for (m in done) {
  group_fits[[m]] <- prog[[m]]$group$best
  gfd <- bind_rows(filter(gfd, Model != m), prog[[m]]$group$perf$df)
}
meta$updated <- format(Sys.time()); meta$rwkf_refit <- note
save(group_fits, gfd, meta, file = file.path(ADIR, "group_fits.Rdata"))

load(file.path(ADIR, "cv_fits.Rdata"))  # cv_fits, folds, meta
wsse <- \(df) df |> group_by(condnum) |> summarise(s = sum((ModelPerf - HumanPerf)^2), n = first(Nsubj)) |>
  summarise(v = sum(s * n) / sum(n)) |> pull(v)
old_cv <- map_dbl(set_names(done), \(m) if (is.null(cv_fits[[m]]$testdf)) NA_real_ else wsse(cv_fits[[m]]$testdf))
for (m in done) {
  cv_fits[[m]] <- list(folds = map(prog[[m]]$folds, \(f) f[c("par", "train_sse", "test_sse", "testdf", "test_conditions")]))
  cv_fits[[m]]$pars   <- do.call(rbind, map(cv_fits[[m]]$folds, "par"))
  cv_fits[[m]]$testdf <- list_rbind(map(cv_fits[[m]]$folds, "testdf"))
}
meta$updated <- format(Sys.time()); meta$rwkf_refit <- note
save(cv_fits, folds, meta, file = file.path(ADIR, "cv_fits.Rdata"))

summary_tab <- tibble(
  model = done,
  group_old = old_group[done], group_new = map_dbl(done, \(m) prog[[m]]$group$best$optim$bestval),
  cv_old = old_cv[done], cv_new = map_dbl(done, \(m) wsse(cv_fits[[m]]$testdf)),
  group_par = map_chr(done, \(m) paste(signif(prog[[m]]$group$best$optim$bestmem, 3), collapse = ", ")),
  group_runs = map_chr(done, \(m) paste(sprintf("%.4f", prog[[m]]$group$all_vals), collapse = " ")))
saveRDS(summary_tab, file.path(DIR, "rwkf_refit_summary.rds"))
print(as.data.frame(summary_tab), digits = 4)
stamp("merged ", paste(done, collapse = ", "), " into ", ADIR)
