# Package-drift check: are the saved fits still what the CURRENT XSLmodels
# source produces?
#
# Every fit in fits/ was produced via library(XSLmodels) against the installed
# package at the time (0.3.0 or earlier). XSLmodels 0.4.0 changed some model
# behavior -- notably get_perf() now treats negative associations as 0 and
# answers all-zero rows at chance (affects error-driven models), and
# uncfam_predictive() now caps associations at beta. This script takes each
# model's SAVED group-fit parameters, reruns it with the current package
# SOURCE (devtools::load_all) on the same 53 conditions the fit used, and
# compares item-level ModelPerf against the saved gfd. No fitting involved,
# so any difference is a package-behavior change, not optimizer noise.
#
# Stochastic models are also run twice under 0.4.0 with different seeds to
# give a Monte Carlo noise floor: a saved-vs-0.4 difference within that floor
# is just simulation noise.
#
# Run from xsl-bakeoff/:  Rscript fits/check_package_drift.R
# Output: fits/package_drift.rds (one row per model) + printed table.

suppressMessages({
  devtools::load_all("../XSLmodels", quiet = TRUE)
  library(dplyr); library(purrr); library(here)
})
source(here("fits/model_specs.R"))      # model_specs, fazlyt_spec
source(here("fits/new_model_specs.R"))  # new_model_specs

N_SIM <- 200

saved <- local({
  e1 <- new.env(); load(here("fits/group_fits_all_experiments.Rdata"), envir = e1)
  e2 <- new.env(); load(here("fits/group_fits_new_models.Rdata"),       envir = e2)
  list(fits = c(e1$group_fits_all, e2$group_fits_new),
       gfd  = bind_rows(e1$gfd_all, e2$gfd_new))
})

specs <- c(model_specs,
           list(baseline = list(constructor = function(p) baseline(), stochastic = FALSE),
                fazlyt = fazlyt_spec),
           new_model_specs)

data53 <- xsl_datasets[1:53]
stopifnot(identical(map_chr(data53, "label"), unique(saved$gfd$condnum)))

run_perf <- function(m, seed) {
  set.seed(seed)
  bm <- saved$fits[[m]]$optim$bestmem
  mod <- specs[[m]]$constructor(if (is.null(bm)) numeric(0) else as.numeric(bm))
  ctrl <- if (isTRUE(specs[[m]]$stochastic)) xslControl(n_sim = N_SIM) else xslControl()
  res <- xsl_run(mod, data53, control = ctrl)
  unlist(map(res$fits, \(f) as.vector(f$perf)))
}

models <- intersect(names(specs), unique(saved$gfd$Model))

drift <- map_dfr(models, function(m) {
  message("  ", m)
  old <- saved$gfd %>% filter(Model == m)
  stopifnot(identical(old$condnum, rep(old$condnum, 1)))  # keep saved order
  new1 <- run_perf(m, 1)
  stopifnot(length(new1) == nrow(old))
  stoch <- isTRUE(specs[[m]]$stochastic)
  floor_diff <- if (stoch) mean(abs(new1 - run_perf(m, 2))) else 0
  d <- abs(new1 - old$ModelPerf)
  tibble(
    Model = m, stochastic = stoch,
    SSE_saved = sum((old$ModelPerf - old$HumanPerf)^2),
    SSE_0.4   = sum((new1 - old$HumanPerf)^2),
    r_saved   = cor(old$ModelPerf, old$HumanPerf),
    r_0.4     = cor(new1, old$HumanPerf),
    mean_abs_diff = mean(d), max_abs_diff = max(d),
    noise_floor = floor_diff
  )
}) %>%
  mutate(dSSE = SSE_0.4 - SSE_saved,
         changed = if_else(stochastic, mean_abs_diff > 2 * noise_floor, max_abs_diff > 1e-8)) %>%
  arrange(desc(changed), desc(abs(dSSE)))

saveRDS(drift, here("fits/package_drift.rds"))
print(as.data.frame(drift %>% mutate(across(where(is.numeric), \(x) signif(x, 3)))), row.names = FALSE)

# RESULT (2026-10-02, XSLmodels 0.4.0 source vs. fits made with 0.3.0-era code):
# - Every deterministic uncfam-family model CHANGED at fixed parameters
#   (kachergis/FU, novelty, uncertainty, strength, uncfam_attention,
#   uncfam_predictive; item-level max |diff| 0.13-0.52, group SSE shifts of
#   -0.7 to +0.7). Cause: XSLmodels commit 6aaebcb (2026-09-06) fixed an
#   indexing bug in update_known() -- which(m[w, fam_objects] == 0) returned
#   positions WITHIN the familiar-object subset but was used as absolute
#   column indices, so startval landed in the wrong cell whenever some object
#   was still wholly unfamiliar. The ORIGINAL research code
#   (models/kachergis.R etc.) has the identical bug, so the published CogSci
#   numbers for these models carry it too. uncfam_predictive additionally
#   picks up 0.4.0's cap-at-beta change.
# - Bayesian_decay, rescorla-wagner, kalman_filter, fazly, fazlyt, baseline:
#   bit-identical (the get_perf() negative-association fix doesn't bind at
#   their fitted parameters).
# - Stochastic models (PbV, GnT, Pursuit, FUs, RL): within ~1-1.5x the Monte
#   Carlo noise floor -- no evidence of a real change.
# => The uncfam-family fits must be REFIT under 0.4.0 regardless of the
#    3 new conditions; saved SSEs for them are not what the package gives.
