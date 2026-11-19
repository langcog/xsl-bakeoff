# Generalization of every v05 model to Yurovsky & Frank (2015)
# (yurovsky_frank2015): each model's group-fit parameters (fit to the 47
# xsl_datasets conditions) predict the three experiments, with NO refitting.
#
# The paradigm is response-contingent: a word's test shows the object the
# participant chose on its exposure trial (Same), one they didn't (Switch), or
# one they didn't with a new word (New Label). For a model, we take its
# exposure-trial response to be its own choice among that trial's objects,
# from its state right after the trial, by its own choice rule (Luce, ch_dec,
# softmax phi, or remember-or-guess), and condition on it, per simulated
# learner:
#   Same      = sum_o c(o) P(o | word, {o} + novel)
#   Switch    = sum_o c(o) mean_{o' != o} P(o' | word, {o'} + novel)
#   New Label = the same as Switch, with a novel word
# where c(o) is the exposure response distribution and P(. | word, alts) the
# choice at test, from the state just before the test trial. A model whose
# state after exposure is symmetric over the objects (most associative
# models, on a novel word with novel objects) therefore predicts Same =
# Switch.
#
# Training sequences follow the experiments' fixed designs (one per
# condition; the 4 practice trials are dropped):
#   Experiments 1-2: 8 words, each an exposure trial (N objects) and a test
#     trial `interval` trials later, at the positions in the data (identical
#     for every participant in an interval condition). Each test trial is
#     also a training trial, showing exposure object 1 and N - 1 novel ones
#     (which object a participant saw depended on their choice; this only
#     affects later words). Exp. 2's New Label test trials are trained as
#     their word's own trial, too.
#   Experiment 3: 4 blocks of Ex-A (word A, N objects), Ex-B (word B, N
#     objects), T-A (A with one of A's objects, one of B's, and N - 2 novel);
#     the paper puts one Ex-B between each Ex-A and T-A but doesn't give the
#     block order, so blocks are sequential.
#
# Models: every model in fits/v05/adults/group_fits.Rdata (specs.R), plus
# yf2015 and yf2015_me (specs_yf2015.R) once refit_yf2015.R has their group
# fits. All are scored with XSLmodels at fits/v06/yf2015/XSLMODELS_COMMIT,
# which for these models matches the libraries they were fit with (checked:
# `sse_check` re-runs each group fit on xsl_datasets). yf2015 models are fit
# in mean-field mode, whose state after an exposure is symmetric, so here
# they run in sampling mode (their own account of the paradigm, in which the
# response is the sampled hypothesis, is yf2015_paradigm(); see
# generalize_yf.R).
#
# Results are cached per model in fits/v06/yf2015/yf_scores/<model>.rds
# (skipped next time unless its parameters change), and the summary is
# rebuilt from every cached model: fits/v06/yf2015/yf_scores_summary.rds.
#
# Usage (from xsl-bakeoff/):
#   R_LIBS=$(pwd)/fits/v06/yf2015/Rlib N_CORES=4 Rscript fits/v06/yf2015/score_yf_all.R [model ...]
#   ... score_yf_all.R --validate   # check the scorer against yf2015_paradigm()

suppressMessages({
  library(XSLmodels)
  library(purrr)
  library(dplyr)
  library(here)
})
DIR <- here("fits/v06/yf2015")
if (!startsWith(find.package("XSLmodels"), file.path(DIR, "Rlib"))) {
  stop("run with R_LIBS=$(pwd)/fits/v06/yf2015/Rlib (found ", find.package("XSLmodels"), ")")
}
source(here("fits/v05/specs.R"))
source(file.path(DIR, "specs_yf2015.R"))
N_SIM   <- as.integer(Sys.getenv("N_SIM", "200"))
N_CORES <- as.integer(Sys.getenv("N_CORES", "4"))
OUT <- file.path(DIR, "yf_scores")
dir.create(OUT, showWarnings = FALSE)
stamp <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)

source(file.path(DIR, "yf_scoring.R"))

## ---- validation ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if ("--validate" %in% args) {
  # sigma = 1, gamma >= 1: the trial-level model's state after an exposure puts
  # all its strength on its hypothesis, so its response is its hypothesis and
  # this scorer must equal yf2015_paradigm() exactly, learner by learner
  for (mu in c(0, 2)) {
    mod <- yf2015(sigma = 1, lambda = .3, gamma = 1.2, mu = mu, sampling = TRUE)
    got <- score_model(mod, n_sim = 3)
    want12 <- yf2015_paradigm(mod, as.data.frame(got$e12))
    want3 <- yf2015_paradigm(mod, as.data.frame(got$e3))
    stamp(sprintf("validate mu = %g: Exp 1-2 max diff %.2g, Exp 3 max diff %.2g / %.2g", mu,
                  max(abs(got$e12$p_target - want12$p_target)),
                  max(abs(got$e3$p_target - want3$p_target)), max(abs(got$e3$p_competitor - want3$p_competitor))))
  }
  # a symmetric associative model predicts Same = Switch, and chance for New Label
  got <- score_model(baseline())
  stamp(sprintf("validate baseline: max |Same - Switch| %.2g, max |New Label - 1/N| %.2g",
                max(abs(filter(got$e12, trial_type == "Same")$p_target - filter(got$e12, trial_type == "Switch")$p_target)),
                max(abs(filter(got$e12, trial_type == "New Label")$p_target -
                          1 / filter(got$e12, trial_type == "New Label")$n_referents))))
  quit(save = "no")
}

## ---- all models -----------------------------------------------------------------------
g <- new.env(); load(here("fits/v05/adults/group_fits.Rdata"), envir = g)
jobs <- imap(g$group_fits, \(f, m) list(model = m, par = unname(f$optim$bestmem), spec = specs_v04[[m]],
                                         in_sample = f$optim$bestval))
yf_file <- file.path(DIR, "yf2015_fits.Rdata")
if (file.exists(yf_file)) {
  load(yf_file)  # prog
  for (m in names(Filter(\(x) !is.null(x$group), prog))) {
    jobs[[m]] <- list(model = m, par = unname(prog[[m]]$group$best$optim$bestmem), spec = specs_yf2015[[m]],
                      in_sample = prog[[m]]$group$best$optim$bestval)
  }
}
models <- if (length(setdiff(args, "--validate"))) setdiff(args, "--validate") else names(jobs)
stopifnot(all(models %in% names(jobs)))

run_job <- function(j) {
  path <- file.path(OUT, paste0(j$model, ".rds"))
  if (file.exists(path) && identical(readRDS(path)$par, j$par)) return(invisible(NULL))
  t0 <- Sys.time()
  mod <- j$spec$constructor(j$par)
  # yf2015 models: sampling mode (see the header)
  if (identical(mod$name, "yf2015")) { mod$params$sampling <- TRUE; mod$stochastic <- TRUE }
  # check that this library reproduces the model's group fit on xsl_datasets
  set.seed(1)
  sse <- xsl_run(j$spec$constructor(j$par), xsl_datasets,
                 control = if (j$spec$stochastic) xslControl(n_sim = 200) else xslControl())$sse
  pred <- score_model(mod, N_SIM)
  saveRDS(list(model = j$model, par = j$par, pred = pred, sse_check = sse, in_sample = j$in_sample,
               stochastic = mod$stochastic, n_sim = if (mod$stochastic) N_SIM else 1,
               minutes = as.numeric(difftime(Sys.time(), t0, units = "mins"))), path)
  stamp(sprintf("%s: done (%.1f min; SSE %.4f vs fit %.4f)", j$model,
                as.numeric(difftime(Sys.time(), t0, units = "mins")), sse, j$in_sample))
}
res <- parallel::mclapply(jobs[models], \(j) tryCatch(run_job(j), error = \(e) conditionMessage(e)),
                          mc.cores = N_CORES)
walk2(models, res, \(m, r) if (is.character(r)) stamp("FAILED ", m, ": ", r))

## ---- summary --------------------------------------------------------------------------
# Two rows per model: its raw predictions (lapse 0), and with a lapse fit to
# Experiment 1. Ranked by the held-out likelihood (Experiments 2 and 3) with
# the lapse.
done <- map(list.files(OUT, "\\.rds$", full.names = TRUE), readRDS)
summary_tab <- map(done, \(x) {
  eps <- fit_lapse(x$pred)
  bind_cols(tibble(model = x$model, sse_fit = x$in_sample, sse_check = x$sse_check),
            bind_rows(metrics(x$pred, 0), metrics(x$pred, eps)))
}) |> list_rbind() |>
  mutate(heldout_loglik = e2_loglik + e3_loglik) |>
  group_by(model) |> mutate(rank_key = heldout_loglik[lapse > 0 | n() == 1][1]) |> ungroup() |>
  arrange(desc(rank_key), lapse) |> select(-rank_key)
saveRDS(summary_tab, file.path(DIR, "yf_scores_summary.rds"))
print(as.data.frame(filter(summary_tab, lapse > 0)), digits = 3)
