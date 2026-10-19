# Shared helpers for the v05 "extras" analyses (noise ceiling, choice-level
# evaluation, model/parameter recovery). Sourced by the scripts in this
# directory; run them from xsl-bakeoff/ with the pinned package:
#
#   R_LIBS=fits/v05/Rlib Rscript fits/v05/extras/<script>.R
#
# Defines: specs_v04 (fits/v05/specs.R), load_group_pars(), run_model(),
# choice_items(), EXCLUDE_CHOICE, lapse(), stamp().

suppressMessages({
  library(XSLmodels)
  library(purrr)
  library(dplyr)
  library(here)
})

if (!startsWith(find.package("XSLmodels"), here("fits/v05/Rlib"))) {
  stop("XSLmodels must come from fits/v05/Rlib (found ", find.package("XSLmodels"),
       "). Run with R_LIBS=fits/v05/Rlib.")
}
source(here("fits/v05/specs.R"))  # specs_v04, GROUP_ONLY, RUN_ORDER

EXTRAS_DIR <- here("fits/v05/extras")
stamp <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)

# Best group-fit parameters per model from the v05 refit (whatever models it
# has finished so far).
load_group_pars <- function(path = here("fits/v05/adults/group_fits.Rdata")) {
  e <- new.env(); load(path, envir = e)
  map(e$group_fits, \(g) unname(g$optim$bestmem))
}

run_model <- function(name, par, data = xsl_datasets, n_sim = 200) {
  spec <- specs_v04[[name]]
  ctrl <- if (spec$stochastic) xslControl(n_sim = n_sim) else xslControl()
  xsl_run(spec$constructor(par), data, control = ctrl)
}

# Test items whose human responses can't be scored as a choice among the
# listed alternatives. Empty since XSLmodels PR #31 (e70c10c), which fixed the
# two cases found 2026-10-14: 214's listed 4AFC foils were never shown (its
# participants saw 213's test displays), and 211 item 17 listed object 17
# twice in place of foil 2. Every human response is now among its item's
# listed alternatives (asserted by n_out == 0 in choice_items()).
EXCLUDE_CHOICE <- list()

# One row per test item: the alternatives, the model's choice distribution
# over them, and the human response counts over the same alternatives.
#
# The model distribution reproduces xsl_run()'s scoring exactly (asserted
# against f$perf): Luce choice over the listed alternatives on the summed
# (over simulations) matrix when the condition has an explicit test (via
# predict_referent()), and choice among all objects otherwise (get_perf());
# both with Bayesian_decay's exponent ch_dec, as xsl_run() applies it on both
# paths since XSLmodels PR #31.
choice_items <- function(f, ch_dec = NULL) {
  dat <- f$data; mat <- f$matrix; rm <- dat$response_matrix
  n_items <- length(dat$accuracy)
  if (length(dat$test)) {
    t <- XSLmodels:::labelled_test(dat$test)
    alts <- t$objects
    correct <- if (is.null(t$correct)) unlist(t$words) else unlist(t$correct)
    probs <- map(seq_len(n_items), \(i) predict_referent(mat, t$words[[i]], alts[[i]], d = ch_dec))
  } else {
    stopifnot(identical(rownames(mat), colnames(mat)[seq_len(nrow(mat))]))
    d <- if (is.null(ch_dec)) 1 else ch_dec
    M <- pmax(mat, 0)^d
    alts <- rep(list(colnames(mat)), n_items)
    correct <- colnames(mat)[seq_len(n_items)]
    probs <- map(seq_len(n_items), \(i) {
      tot <- sum(M[i, ]); if (tot > 0) M[i, ] / tot else rep(1 / ncol(M), ncol(M))
    })
  }
  p_correct <- map2_dbl(probs, seq_len(n_items), \(p, i) p[match(correct[i], alts[[i]])])
  stopifnot(max(abs(p_correct - as.vector(f$perf))) < 1e-9)
  counts <- map(seq_len(n_items), \(i) {
    out <- rm[i, setdiff(colnames(rm), alts[[i]])]
    c(n_in = sum(rm[i, match(alts[[i]], colnames(rm))]), n_out = sum(out))
  })
  stopifnot(all(map_dbl(counts, "n_out") == 0))
  ex <- EXCLUDE_CHOICE[[dat$label]]
  tibble(label = dat$label, condition = dat$condition, n_subj = dat$n_subj, item = seq_len(n_items),
         alts = alts, correct = correct, p = probs,
         human = map(seq_len(n_items), \(i) as.vector(rm[i, match(alts[[i]], colnames(rm))])),
         n_out = map_dbl(counts, "n_out"),
         excluded = if (is.null(ex)) FALSE else if (identical(ex, "all")) TRUE else seq_len(n_items) %in% ex)
}

# Lapse mixture: with probability eps the learner picks uniformly among the k
# alternatives. Keeps the multinomial likelihood finite for models that give
# a chosen object probability 0.
lapse <- function(p, eps) (1 - eps) * p + eps / length(p)
