# Which model best accounts for learning trajectories? The kachergis_priming
# data (XSLmodels PR #38): three conditions (164 participants) in which
# people studied one training sequence four times, with an 18AFC test on all
# 18 words after each repetition.
#
#   4x4 6x       n = 54  the original 4x4 sequence (= the dropped orig_4x4)
#   4x4 3/6/9x   n = 44  the sequence of xsl_datasets' 3_x8_369_4x4
#   3x3 3/6/9x   n = 66  the sequence of xsl_datasets' freq369-3x3loCD
#
# The participants are not in xsl_datasets, so the v05 fits never saw them.
# Every model in fits/v05/specs.R is scored in two regimes:
#
#  - generalize: at its v05 group-fit parameters (fits/v05/adults/
#    group_fits.Rdata), with no refitting -- a held-out test, as in
#    fits/v05/generalize.R, but of a whole learning curve.
#  - fit: fit directly to the three trajectories (all 12 condition x block
#    accuracy vectors at once), with v05's objective and settings: n_subj-
#    weighted SSE on item accuracy, DEoptim reltol = .001, NP = 100,
#    itermax = 100 (deterministic) or 30 (stochastic), set.seed(2026).
#
# Block b is predicted by a model trained on b repetitions of the sequence
# (XSLmodels' multi-block scoring; for deterministic models this is exactly
# what continuous training records after each block). Both regimes are
# scored identically, one single-block condition per block:
#  - item level: r and RMSE over the 216 condition x block x word accuracies,
#    and the error in each condition's 4-point learning curve (block means);
#  - choice level: multinomial log-likelihood of every response (which of
#    the 18 objects was chosen), with a lapse rate eps (uniform guessing)
#    fit per model and regime, as in fits/v05/extras/choice_level.R; and,
#    for the fit regime, BIC from it (k = the model's parameters + eps).
# Parameters were fit to accuracy only, so the foil choices are a test.
#
# Libraries: the generalize phase uses the v05 pins (fits/v05/Rlib, and
# fits/v05/tilles/Rlib for tilles' corrected model), like generalize.R. The
# fit phase needs multi-block xslData, which the v05 pin predates, so it uses
# XSLmodels master at fits/v05/priming/XSLMODELS_COMMIT, installed in
# fits/v05/priming/Rlib. Every model in specs.R other than tilles (whose
# corrected model is the master one) is unchanged between the two.
#
# The fixed rescorla-wagner and the error-driven redesigns (specs.R's
# RWKF_MODELS) are scored in both phases with fits/v05/rwkf/Rlib (XSLmodels
# after PRs #41-#42); see fits/v05/rwkf/queue_rwkf.sh. Each cached result
# records its library and commit.
#
# Usage (from xsl-bakeoff/): bash fits/v05/priming/run_priming.sh [N_WORKERS]
# or by hand:
#   R_LIBS=$(pwd)/fits/v05/Rlib          Rscript fits/v05/priming/priming.R generalize
#   R_LIBS=$(pwd)/fits/v05/tilles/Rlib   Rscript fits/v05/priming/priming.R generalize tilles
#   R_LIBS=$(pwd)/fits/v05/priming/Rlib  Rscript fits/v05/priming/priming.R fit [model ...]
#   R_LIBS=$(pwd)/fits/v05/priming/Rlib  Rscript fits/v05/priming/priming.R summary
# Every phase caches per model (generalize/<model>.rds, fits/<model>.rds) and
# skips what's done; summary combines whatever exists.
#
# Output: fits/v05/priming/priming.Rdata --
#   items    one row per regime x model x condition x block x word
#   curves   one row per regime x model x condition x block (block means)
#   summary  one row per regime x model
#   meta

suppressMessages({
  library(XSLmodels)
  library(purrr)
  library(dplyr)
  library(here)
})
source(here("fits/v05/specs.R"))  # specs_v04, RUN_ORDER

args  <- commandArgs(trailingOnly = TRUE)
phase <- args[1]
stopifnot(phase %in% c("generalize", "fit", "summary"))
DIR       <- here("fits/v05/priming")
N_SIM     <- as.integer(Sys.getenv("N_SIM", "200"))
N_WORKERS <- as.integer(Sys.getenv("N_WORKERS", "10"))
stamp <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)

lib <- find.package("XSLmodels")
LIBS <- c(v05 = here("fits/v05/Rlib"), tilles = here("fits/v05/tilles/Rlib"), priming = file.path(DIR, "Rlib"),
          rwkf = here("fits/v05/rwkf/Rlib"))
COMMIT_FILES <- c(v05 = here("fits/v05/XSLMODELS_COMMIT"), tilles = here("fits/v05/tilles/XSLMODELS_COMMIT"),
                  priming = file.path(DIR, "XSLMODELS_COMMIT"), rwkf = here("fits/v05/rwkf/XSLMODELS_COMMIT"))
LIB_USED <- names(LIBS)[startsWith(lib, LIBS)]
need <- switch(phase, generalize = c("v05", "tilles", "rwkf"), fit = c("priming", "rwkf"), summary = names(LIBS))
if (!length(LIB_USED) || !LIB_USED %in% need) {
  stop("the ", phase, " phase needs R_LIBS=", paste(LIBS[need], collapse = " or "), " (found ", lib, ")")
}

# ---- data -------------------------------------------------------------------
# A copy of XSLmodels' kachergis_priming (the v05 pin predates it), and each
# condition x block as a single-block condition: block b's accuracy and
# responses, trained on b repetitions of the sequence.
PRIMING_RDS <- file.path(DIR, "kachergis_priming.rds")
if (!file.exists(PRIMING_RDS)) {
  stopifnot(LIB_USED == "priming")
  saveRDS(kachergis_priming, PRIMING_RDS)
}
priming <- readRDS(PRIMING_RDS)
stopifnot(length(priming) == 3, all(map_dbl(priming, \(x) nrow(x$accuracy)) == 4))

block_conditions <- imap(priming, \(x, cn) map(1:4, \(b) xslData(
  train = list(words = rep(x$train$words, b), objects = rep(x$train$objects, b)),
  accuracy = unname(x$accuracy[b, ]), n_subj = x$n_subj,
  response_matrix = x$response_matrix[[b]],
  label = paste0(cn, " block", b), condition = cn))) |> list_flatten() |> unname()

ctrl_for <- function(spec) if (spec$stochastic) xslControl(n_sim = N_SIM) else xslControl()
# the fixed rescorla-wagner and the error-driven redesigns (specs.R's
# RWKF_MODELS) are scored with fits/v05/rwkf/Rlib, tilles with its own, and
# the rest with the v05 pin (generalize) or this directory's pin (fit)
lib_of <- function(m, phase) ifelse(m %in% RWKF_MODELS, "rwkf",
                                    ifelse(phase == "fit", "priming", ifelse(m == "tilles", "tilles", "v05")))
ch_dec_of <- function(name, par) if (name == "Bayesian_decay") par[3] else NULL

# One row per test item: the model's accuracy and choice distribution over
# the 18 objects (exactly as xsl_run() scores it; asserted), and the human
# accuracy and response counts.
score <- function(name, par) {
  spec <- specs_v04[[name]]
  mod <- spec$constructor(par)
  res <- xsl_run(mod, block_conditions, control = ctrl_for(spec))
  d <- ch_dec_of(name, par); d <- if (is.null(d)) 1 else d
  phi <- mod$params[["phi"]]
  map(res$fits, \(f) {
    dat <- f$data; mat <- f$matrix; rm <- dat$response_matrix
    stopifnot(length(dat$test) == 0, identical(colnames(mat), colnames(rm)),
              identical(rownames(mat), colnames(mat)))
    # softmax (phi) on the mean matrix, then the power rule, as xsl_run()
    if (!is.null(phi)) mat <- softmax_weights(mat / nrow(f$responses), phi)
    M <- pmax(mat, 0)^d
    probs <- map(seq_len(nrow(M)), \(i) { tot <- sum(M[i, ]); if (tot > 0) unname(M[i, ]) / tot else rep(1 / ncol(M), ncol(M)) })
    p_correct <- map2_dbl(probs, seq_along(probs), \(p, i) p[i])
    stopifnot(max(abs(p_correct - as.vector(f$perf))) < 1e-9)
    tibble(model = name, condition = dat$condition, block = as.integer(sub(".*block", "", dat$label)),
           n_subj = dat$n_subj, item = seq_along(probs), model_perf = p_correct, human = dat$accuracy,
           p = probs, counts = map(seq_along(probs), \(i) unname(rm[i, ])))
  }) |> list_rbind()
}

# ---- generalize -------------------------------------------------------------
if (phase == "generalize") {
  e <- new.env(); load(here("fits/v05/adults/group_fits.Rdata"), envir = e)
  pars <- map(e$group_fits, \(g) unname(g$optim$bestmem))
  models <- if (length(args) > 1) args[-1] else intersect(RUN_ORDER, names(pars))
  models <- models[lib_of(models, "generalize") == LIB_USED]
  out_dir <- file.path(DIR, "generalize"); dir.create(out_dir, showWarnings = FALSE)
  for (m in models) {
    out <- file.path(out_dir, paste0(m, ".rds"))
    if (file.exists(out) && identical(readRDS(out)$par, pars[[m]])) next
    stamp("generalize ", m)
    set.seed(2026)
    saveRDS(list(par = pars[[m]], items = score(m, pars[[m]]), lib = LIB_USED,
                 commit = readLines(COMMIT_FILES[[LIB_USED]])), out)
  }
  stamp("generalize phase done")
}

# ---- fit --------------------------------------------------------------------
if (phase == "fit") {
  options(mc.cores = N_WORKERS)
  models <- if (length(args) > 1) args[-1] else RUN_ORDER
  stopifnot(all(models %in% names(specs_v04)))
  models <- models[lib_of(models, "fit") == LIB_USED]
  out_dir <- file.path(DIR, "fits"); dir.create(out_dir, showWarnings = FALSE)
  err_file <- file.path(DIR, "fit_objective_errors.log")
  # as fits/v05/refit.R's fit_one(), on the multi-block conditions
  fit_one <- function(spec, data) {
    force(data)
    model <- spec$constructor(spec$lower)
    ctrl <- ctrl_for(spec)
    objective <- function(p) {
      sse <- tryCatch(XSLmodels::xsl_run(XSLmodels::update_params(model, p), data, control = ctrl)$sse,
                      error = function(e) {
                        cat(format(Sys.time(), "%F %T"), " p = ", paste(signif(p, 4), collapse = ","), " :: ",
                            conditionMessage(e), "\n", sep = "", file = err_file, append = TRUE)
                        NA_real_
                      })
      if (is.na(sse)) Inf else sse
    }
    if (length(spec$lower) == 0) return(list(optim = list(bestmem = numeric(0), bestval = objective(numeric(0)))))
    fit <- DEoptim::DEoptim(objective, lower = spec$lower, upper = spec$upper,
                            control = DEoptim::DEoptim.control(
                              reltol = .001, trace = FALSE, NP = 100,
                              itermax = if (spec$stochastic) 30 else 100,
                              parallelType = "parallel", packages = list("XSLmodels")))
    if (!is.finite(fit$optim$bestval)) stop("every evaluation failed; see ", err_file)
    fit
  }
  for (m in models) {
    out <- file.path(out_dir, paste0(m, ".rds"))
    if (file.exists(out)) next
    spec <- specs_v04[[m]]
    stamp("fit ", m, " (", length(spec$lower), " params, ", if (spec$stochastic) "stochastic" else "deterministic", ")")
    t0 <- Sys.time()
    set.seed(2026)
    fit <- fit_one(spec, priming)
    par <- unname(fit$optim$bestmem)
    set.seed(2026)
    saveRDS(list(par = par, sse = fit$optim$bestval, k = length(par), items = score(m, par),
                 minutes = as.numeric(difftime(Sys.time(), t0, units = "mins")),
                 lib = LIB_USED, commit = readLines(COMMIT_FILES[[LIB_USED]])), out)
    stamp("  done in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min; sse = ", signif(fit$optim$bestval, 4),
          "; par = ", paste(signif(par, 3), collapse = ", "))
  }
  stamp("fit phase done")
}

# ---- summary ----------------------------------------------------------------
if (phase == "summary") {
  lapse <- function(p, eps) (1 - eps) * p + eps / length(p)
  read_regime <- function(regime) {
    files <- list.files(file.path(DIR, regime), "\\.rds$", full.names = TRUE)
    set_names(map(files, readRDS), sub("\\.rds$", "", basename(files)))
  }
  res <- list(generalize = read_regime("generalize"), fit = read_regime("fits"))
  items <- imap(res, \(r, regime) map(r, "items") |> list_rbind() |> mutate(regime = regime, .before = 1)) |> list_rbind()

  n_resp <- sum(map_dbl(block_conditions, \(x) sum(x$response_matrix)))
  chance_ll <- log(1 / 18)
  choice <- items |> group_by(regime, model) |> group_modify(\(it, key) {
    ll <- \(eps) sum(map2_dbl(it$p, it$counts, \(p, h) sum(h * log(lapse(p, eps)))))
    b <- optimize(\(e) -ll(e), c(1e-6, 1 - 1e-6))
    tibble(ll = -b$objective, eps = b$minimum)
  }) |> ungroup()

  curves <- items |> group_by(regime, model, condition, block) |>
    summarise(model_mean = mean(model_perf), human_mean = mean(human), .groups = "drop")

  summary <- items |> group_by(regime, model) |>
    summarise(sse = sum(tapply((model_perf - human)^2 * n_subj, paste(condition, block), sum)) /
                sum(tapply(n_subj, paste(condition, block), first)),
              item_r = if (sd(model_perf) > 0) cor(model_perf, human) else NA_real_,
              item_rmse = sqrt(mean((model_perf - human)^2)), .groups = "drop") |>
    left_join(curves |> group_by(regime, model) |>
                summarise(curve_mae = mean(abs(model_mean - human_mean)),
                          curve_r = cor(model_mean, human_mean), .groups = "drop"), by = c("regime", "model")) |>
    left_join(choice, by = c("regime", "model")) |>
    mutate(ll_per_resp = ll / n_resp, ll_gain_vs_chance = ll_per_resp - chance_ll,
           k = map2_dbl(regime, model, \(rg, m) if (rg == "fit") res$fit[[m]]$k + 1 else NA_real_),
           bic = -2 * ll + k * log(n_resp)) |>
    arrange(regime, desc(ll))

  meta <- list(n_responses = n_resp, n_sim = N_SIM, chance_ll_per_resp = chance_ll,
               generalize_commits = unique(map_chr(res$generalize, "commit")),
               fit_commits = unique(map_chr(res$fit, "commit")), updated = format(Sys.time()))
  save(items, curves, summary, meta, file = file.path(DIR, "priming.Rdata"))
  print(as.data.frame(select(summary, regime, model, item_r, item_rmse, curve_mae, ll_per_resp, eps, bic)),
        digits = 3, row.names = FALSE)
}
