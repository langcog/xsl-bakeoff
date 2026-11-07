# Refit tilles (TF) for v05 against XSLmodels b44b9fa (2026-10-19).
#
# The v05 fits (fits/v05/adults/) were made with XSLmodels e70c10c. XSLmodels
# PRs #32-#34 (merged after that pin) correct tilles() to match Tilles &
# Fontanari (2013): eq 7's entropy normalized by log2(n_o) rather than
# log(n_o), alpha recomputed for absent words, eqs 1-2 counting objects, and
# eq 5's flux spread evenly when there is nothing to be proportional to. Those
# are the only changes between e70c10c and b44b9fa (R/model-tilles.R; no data
# changes), so every other model's v05 fit stands, and only tilles is refit.
#
# Group fit plus all 5 CV folds (the same folds as fits/v05/adults/cv_fits.Rdata),
# with refit.R's objective (n_subj-weighted SSE), bounds and DEoptim settings
# (NP = 100, itermax = 100, reltol = .001), and the best of N_RESTARTS
# independent runs per fit (refit.R used one; FU-P showed a single run can end
# in a poor basin). Progress is saved after every fit to
# fits/v05/tilles/tilles_fits.Rdata, so the script is resumable.
#
# Once all six fits exist, the tilles entries in fits/v05/adults/group_fits.Rdata
# and cv_fits.Rdata are replaced (backups: *.pre_tilles_refit.Rdata), and each
# file's meta records that tilles comes from b44b9fa.
#
# Usage (from xsl-bakeoff/):
#   R_LIBS=fits/v05/tilles/Rlib N_WORKERS=8 Rscript fits/v05/tilles/refit_tilles.R

suppressMessages({
  library(XSLmodels)
  library(purrr)
  library(dplyr)
  library(here)
})
DIR  <- here("fits/v05/tilles")
ADIR <- here("fits/v05/adults")
COMMIT <- trimws(readLines(file.path(DIR, "XSLMODELS_COMMIT")))
if (!startsWith(find.package("XSLmodels"), file.path(DIR, "Rlib"))) {
  stop("run with R_LIBS=fits/v05/tilles/Rlib (found ", find.package("XSLmodels"), ")")
}
source(here("fits/v05/specs.R"))

M          <- "tilles"
N_WORKERS  <- as.integer(Sys.getenv("N_WORKERS", "8"))
N_RESTARTS <- as.integer(Sys.getenv("N_RESTARTS", "3"))
options(mc.cores = N_WORKERS)
stamp <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)

spec <- specs_v04[[M]]
data_all <- xsl_datasets
labels <- map_chr(data_all, "label")
stopifnot(length(data_all) == 47)
cv_env <- new.env(); load(file.path(ADIR, "cv_fits.Rdata"), envir = cv_env)
folds <- cv_env$folds
stopifnot(length(folds) == 5)

ERR_FILE <- file.path(DIR, "refit_objective_errors.log")
de_run <- function(data, seed, label) {
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
best_of <- function(data, label, base_seed) {
  runs <- map(seq_len(N_RESTARTS), function(r) {
    t0 <- Sys.time()
    f <- de_run(data, base_seed + r, paste(label, "run", r))
    stamp(sprintf("  %s run %d/%d: SSE %.4f at %s (%.1f min)", label, r, N_RESTARTS, f$optim$bestval,
                  paste(signif(f$optim$bestmem, 3), collapse = ", "), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    f
  })
  vals <- map_dbl(runs, \(x) x$optim$bestval)
  list(best = runs[[which.min(vals)]], all_vals = vals, all_pars = map(runs, \(x) unname(x$optim$bestmem)))
}
perf_df <- function(params, data) {
  res <- xsl_run(spec$constructor(params), data)
  list(sse = res$sse,
       df = map(res$fits, \(f) tibble(
         Model = M, condnum = f$data$label, Condition = f$data$condition,
         ModelPerf = as.vector(f$perf), HumanPerf = f$data$accuracy, Nsubj = f$data$n_subj)) |> list_rbind())
}

## ---- fits (resumable) ---------------------------------------------------------------
prog_file <- file.path(DIR, "tilles_fits.Rdata")
prog <- list(group = NULL, folds = list())
if (file.exists(prog_file)) load(prog_file)  # prog
save_prog <- function() save(prog, file = prog_file)

if (is.null(prog$group)) {
  stamp("group (", N_RESTARTS, " restarts, ", N_WORKERS, " workers)")
  b <- best_of(data_all, "group", 2026)
  prog$group <- c(b, list(perf = perf_df(b$best$optim$bestmem, data_all)))
  save_prog()
  stamp("group done: SSE ", signif(b$best$optim$bestval, 4))
}
for (i in seq_along(folds)) {
  if (length(prog$folds) >= i && !is.null(prog$folds[[i]])) next
  stamp("cv fold ", i, "/5")
  split <- get_train_test_split(folds[[i]], data_all)
  b <- best_of(split$train, paste("fold", i), 2026 + 10 * i)
  pd <- perf_df(b$best$optim$bestmem, split$test)
  prog$folds[[i]] <- list(par = b$best$optim$bestmem, train_sse = b$best$optim$bestval,
                          test_sse = pd$sse, testdf = pd$df, test_conditions = labels[folds[[i]]],
                          all_vals = b$all_vals, all_pars = b$all_pars)
  save_prog()
  stamp(sprintf("cv fold %d done: train %.4f, test %.4f", i, b$best$optim$bestval, pd$sse))
}

## ---- merge into the v05 fit files --------------------------------------------------
for (p in file.path(ADIR, c("group_fits.Rdata", "cv_fits.Rdata"))) {
  b <- sub("\\.Rdata$", ".pre_tilles_refit.Rdata", p)
  if (!file.exists(b)) file.copy(p, b)
}
note <- paste0("tilles refit 2026-10-19 against XSLmodels ", COMMIT,
               " (PRs #32-#34; fits/v05/tilles/refit_tilles.R); every other model is from ",
               "the pinned commit in fits/v05/XSLMODELS_COMMIT")

load(file.path(ADIR, "group_fits.Rdata"))  # group_fits, gfd, meta
old_group <- group_fits[[M]]$optim$bestval
group_fits[[M]] <- prog$group$best
gfd <- bind_rows(filter(gfd, Model != M), prog$group$perf$df)
meta$updated <- format(Sys.time()); meta$tilles_refit <- note
save(group_fits, gfd, meta, file = file.path(ADIR, "group_fits.Rdata"))

load(file.path(ADIR, "cv_fits.Rdata"))  # cv_fits, folds, meta
old_cv <- map_dbl(cv_fits[[M]]$folds, "test_sse")
cv_fits[[M]] <- list(folds = map(prog$folds, \(f) f[c("par", "train_sse", "test_sse", "testdf", "test_conditions")]))
cv_fits[[M]]$pars   <- do.call(rbind, map(cv_fits[[M]]$folds, "par"))
cv_fits[[M]]$testdf <- list_rbind(map(cv_fits[[M]]$folds, "testdf"))
meta$updated <- format(Sys.time()); meta$tilles_refit <- note
save(cv_fits, folds, meta, file = file.path(ADIR, "cv_fits.Rdata"))

wsse <- \(df) df |> group_by(condnum) |> summarise(s = sum((ModelPerf - HumanPerf)^2), n = first(Nsubj)) |>
  summarise(v = sum(s * n) / sum(n)) |> pull(v)
summary_tab <- tibble(
  fit = c("group", paste("cv fold", 1:5), "CV (pooled out-of-fold)"),
  old_model = c(old_group, old_cv, NA),
  corrected = c(prog$group$best$optim$bestval, map_dbl(prog$folds, "test_sse"), wsse(cv_fits[[M]]$testdf)),
  runs = c(paste(sprintf("%.4f", prog$group$all_vals), collapse = " "),
           map_chr(prog$folds, \(f) paste(sprintf("%.4f", f$all_vals), collapse = " ")), ""))
saveRDS(summary_tab, file.path(DIR, "tilles_refit_summary.rds"))
print(summary_tab, width = 200)
stamp("tilles refit merged into ", ADIR)
