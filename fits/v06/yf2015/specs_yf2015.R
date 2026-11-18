# Specs for the trial-level Yurovsky & Frank (2015) model, yf2015() (XSLmodels
# PR #46, pinned in fits/v06/yf2015/XSLMODELS_COMMIT). Kept apart from
# fits/v05/specs.R so that the shared roster (RUN_ORDER, the paper's tables)
# is unchanged until we decide to add these models to it.
#
# All fit the deterministic mean-field mode (sampling = FALSE); each
# constructor maps the free-parameter vector to a model, whose choice_rule
# ("remember_guess") makes xsl_run() score by remember-or-guess.
# Bounds are xsl_model_registry()'s.

specs_yf2015 <- list(
  yf2015 = list(
    # mu = 0: the paper's model (no mutual exclusivity)
    constructor = \(p) XSLmodels::yf2015(sigma = p[1], lambda = p[2], gamma = p[3], sampling = FALSE),
    lower = c(0, 0, .01), upper = c(1, 3, 5), stochastic = FALSE),
  yf2015_me = list(
    # guesses avoid objects associated with other words, by exp(-mu * strength)
    constructor = \(p) XSLmodels::yf2015(sigma = p[1], lambda = p[2], gamma = p[3], mu = p[4], sampling = FALSE),
    lower = c(0, 0, .01, 0), upper = c(1, 3, 5, 10), stochastic = FALSE)
)
# Encoding strength fixed at the paper's value: the maximum-likelihood
# estimate of its Integrated model on Experiment 1 (yf2015_fit(); the
# paper's own, transposed, fit gave 1.50). Fit to xsl_datasets, gamma lands
# near .3 and the fits don't transfer to the paper's data (generalize_yf.R);
# these test whether decay (lambda) can take up the slack.
PAPER_GAMMA <- 1.62
# (local(): the value travels with the constructor to DEoptim's workers)
specs_yf2015$yf2015_g <- list(
  constructor = local({g <- PAPER_GAMMA; \(p) XSLmodels::yf2015(sigma = p[1], lambda = p[2], gamma = g, sampling = FALSE)}),
  lower = c(0, 0), upper = c(1, 3), stochastic = FALSE)
specs_yf2015$yf2015_me_g <- list(
  constructor = local({g <- PAPER_GAMMA; \(p) XSLmodels::yf2015(sigma = p[1], lambda = p[2], gamma = g, mu = p[3], sampling = FALSE)}),
  lower = c(0, 0, 0), upper = c(1, 3, 10), stochastic = FALSE)

YF_MODELS <- names(specs_yf2015)
