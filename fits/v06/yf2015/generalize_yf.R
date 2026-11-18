# Generalization of the trial-level Yurovsky & Frank model to its own paper's
# data: each model's group-fit parameters (fit to the 47 xsl_datasets
# conditions by refit_yf2015.R) predict yurovsky_frank2015 with NO refitting.
#
# yf2015_paradigm() conditions on each learner's exposure-trial guess, as the
# experiments do, so the predictions are exact (no per-participant simulation).
#   Experiments 1-2: P(choose the repeated referent) per (experiment,
#     referents, interval, trial type) condition; binomial log-likelihood of
#     the counts, and r^2 over condition means (E1, and E1+2 as in Table 3).
#   Experiment 3: P(target), P(competitor) and P(other) per (referents, trial
#     type); multinomial log-likelihood, and r over the target/competitor means.
# Reference: the closed-form Integrated model fit to Experiment 1 itself
# (yf2015_fit(), maximum likelihood), whose Same/Switch predictions are those
# of yf2015(mu = 0) at its estimates.
#
# Usage (from xsl-bakeoff/), after refit_yf2015.R:
#   R_LIBS=$(pwd)/fits/v06/yf2015/Rlib Rscript fits/v06/yf2015/generalize_yf.R

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
source(file.path(DIR, "specs_yf2015.R"))
load(file.path(DIR, "yf2015_fits.Rdata"))  # prog

e12 <- yurovsky_frank2015$exp12 |>
  mutate(trial_type = as.character(trial_type)) |>
  group_by(experiment, n_referents, interval, trial_type) |>
  summarise(k = sum(chose_repeated), n = n(), .groups = "drop")
e3 <- yurovsky_frank2015$exp3 |>
  mutate(trial_type = as.character(trial_type)) |>
  group_by(n_referents, trial_type) |>
  summarise(n_t = sum(correct), n_c = sum(familiar), n = n(), .groups = "drop") |>
  mutate(n_o = n - n_t - n_c)
r2 <- \(obs, pred) cor(obs, pred)^2

score <- function(mod, label) {
  p12 <- yf2015_paradigm(mod, as.data.frame(e12))$p_target
  p3 <- yf2015_paradigm(mod, as.data.frame(e3))
  p3o <- pmax(1 - p3$p_target - p3$p_competitor, 0)
  e1 <- e12$experiment == 1
  tibble(model = label,
         e1_r2 = r2(e12$k[e1] / e12$n[e1], p12[e1]),
         e12_r2 = r2(e12$k / e12$n, p12),
         e12_loglik = sum(dbinom(e12$k, e12$n, p12, log = TRUE)),
         e3_r_target = cor(e3$n_t / e3$n, p3$p_target),
         e3_r_competitor = cor(e3$n_c / e3$n, p3$p_competitor),
         e3_loglik = sum(e3$n_t * log(p3$p_target) + e3$n_c * log(p3$p_competitor) +
                           ifelse(e3$n_o > 0, e3$n_o * log(p3o), 0)),
         params = paste(names(mod$params)[1:4], signif(unlist(mod$params[1:4]), 3), sep = "=", collapse = " "))
}

models <- names(Filter(\(x) !is.null(x$group), prog))
out <- map(models, \(m) score(specs_yf2015[[m]]$constructor(unname(prog[[m]]$group$best$optim$bestmem)),
                              paste(m, "(group fit, xsl_datasets)")))
cf <- yf2015_fit(model = "integrated", experiments = 1, seed = 1)
out <- c(out, list(score(yf2015(cf$params[["sigma"]], cf$params[["lambda"]], cf$params[["gamma"]]),
                         "reference: Integrated fit to Exp 1")))
res <- list_rbind(out)
saveRDS(list(summary = res, e12 = e12, e3 = e3), file.path(DIR, "generalization_yf.rds"))
print(as.data.frame(res), digits = 3)
