# Specs for the trial-level Yurovsky & Frank (2015) model, yf2015() (XSLmodels
# PR #46, pinned in fits/v06/yf2015/XSLMODELS_COMMIT). Kept apart from
# fits/v05/specs.R so that the shared roster (RUN_ORDER, the paper's tables)
# is unchanged until we decide to add these models to it.
#
# Both fit the deterministic mean-field mode (sampling = FALSE). yf2015()'s
# params start (sigma, lambda, gamma, mu), so update_params() sets the free
# ones positionally; its remaining params (sampling, choice_rule =
# "remember_guess") stay put, and xsl_run() scores by remember-or-guess.
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
YF_MODELS <- names(specs_yf2015)
