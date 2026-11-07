# Round-2 (XSLmodels 0.4.0) model roster for the journal paper: one spec per
# model, giving its constructor (parameter vector -> xslMod), DEoptim bounds,
# and whether it is stochastic. Supersedes, for round 2 only, the split
# between fits/model_specs.R (original 11) and fits/new_model_specs.R
# (round-1 additions) -- those files are left untouched so round-1 results
# stay reproducible.
#
# Roster (decided 2026-10-02): the original 11, the four round-1 additions,
# and four more -- uncfam_gamma, uncfam_elimination, minerva2, tilles -- plus
# the 0-parameter baseline (and fazlyt, a group-fit-only bonus as in round 1).
# todam() and rem() are deliberately left out (20-40x slower per evaluation
# than the next-slowest model; future-work mention only).
#
# Bounds:
#  - The original 11 keep the original paper's exact bounds/parameter order
#    (copied from fits/model_specs.R).
#  - EVERY uncfam-family variant gets the same X/B/C bounds as the original FU
#    model -- X in [.001, 5], B in [.1, 15], C in [.5, 1] -- plus its own
#    extra parameter. In round 1, FU-A/FU-P were fit with the registry's
#    B in [.8, 1], which excludes FU's own optimum (B ~ 13.5); that made the
#    variants look worse than FU partly for reasons of bounds, not mechanism.
#  - Other new models use xsl_model_registry() bounds (XSLmodels 0.4.0), with
#    the round-1 widenings for kalman_filter/softmax_rl.
#
# Requires XSLmodels (0.4.0, see fits/v04/XSLMODELS_COMMIT) to be attached.

UNCFAM_LOWER <- c(X = .001, B = .1, C = .5)
UNCFAM_UPPER <- c(X = 5,    B = 15, C = 1)

specs_v04 <- list(
  ## ---- original 11 (bounds/order exactly as fits/model_specs.R) ----------
  kachergis = list(
    constructor = \(p) uncfam(X = p[1], B = p[2], C = p[3], variant = "entropy"),
    lower = UNCFAM_LOWER, upper = UNCFAM_UPPER, stochastic = FALSE),
  novelty = list(
    constructor = \(p) uncfam(X = p[1], B = p[2], C = p[3], variant = "novelty"),
    lower = UNCFAM_LOWER, upper = UNCFAM_UPPER, stochastic = FALSE),
  uncertainty = list(
    constructor = \(p) uncfam(X = p[1], B = p[2], C = p[3], variant = "uncertainty-only"),
    lower = UNCFAM_LOWER, upper = UNCFAM_UPPER, stochastic = FALSE),
  strength = list(
    # familiarity-only special case of uncfam (B fixed at 0)
    constructor = \(p) xslMod(
      name = "strength", description = "uncfam() familiarity-only special case (B = 0)",
      model = \(params, data, control) XSLmodels:::uncfam_model(
        params = list(X = params$X, B = 0, C = params$C, variant = "entropy"),
        data = data, control = control),
      params = list(X = p[1], C = p[2]), stochastic = FALSE),
    lower = c(.001, .1), upper = c(5, 1), stochastic = FALSE),
  Bayesian_decay = list(
    constructor = \(p) bayesian_decay(alpha = p[1], delta = p[2], ch_dec = p[3]),
    lower = c(1e-5, 1e-5, 1e-5), upper = c(10, 10, 10), stochastic = FALSE),
  `rescorla-wagner` = list(
    # salience alpha fixed at 1
    constructor = \(p) xslMod(
      name = "rescorla_wagner", description = "Rescorla-Wagner with alpha (salience) fixed at 1",
      model = \(params, data, control) XSLmodels:::rescorla_wagner_model(
        params = list(C = params$C, alpha = 1, beta = params$beta, lambda = params$lambda),
        data = data, control = control),
      params = list(beta = p[1], C = p[2], lambda = p[3]), stochastic = FALSE),
    lower = c(1e-5, 1e-5, 1e-5), upper = c(1, 1, 1), stochastic = FALSE),
  fazly = list(
    constructor = \(p) fazly(lambda = p[1], beta = p[2]),
    lower = c(1e-10, 2), upper = c(2, 20000), stochastic = FALSE),
  trueswell2012 = list(
    constructor = \(p) propose_but_verify(alpha = p[1], alpha_increase = p[2]),
    lower = c(.0001, .0001), upper = c(1, 1), stochastic = TRUE),
  `guess-and-test` = list(
    constructor = \(p) guess_and_test(f = p[1], sa = p[2]),
    lower = c(.0001, .0001), upper = c(1, 1), stochastic = TRUE),
  pursuit = list(
    constructor = \(p) pursuit(gamma = p[1], threshold = p[2], lambda = p[3]),
    lower = c(1e-5, 1e-5, 1e-5), upper = c(1, 1, 1), stochastic = TRUE),
  kachergis_sampling = list(
    constructor = \(p) uncfam_sampling(X = p[1], B = p[2], C = p[3], K = 1),
    lower = UNCFAM_LOWER, upper = UNCFAM_UPPER, stochastic = TRUE),

  ## ---- round-1 additions ---------------------------------------------------
  uncfam_attention = list(
    constructor = \(p) uncfam_attention(X = p[1], B = p[2], C = p[3]),
    lower = UNCFAM_LOWER, upper = UNCFAM_UPPER, stochastic = FALSE),   # was B in [.8, 1]
  uncfam_predictive = list(
    constructor = \(p) uncfam_predictive(X = p[1], B = p[2], C = p[3]),
    lower = UNCFAM_LOWER, upper = UNCFAM_UPPER, stochastic = FALSE),   # was B in [.8, 1]
  kalman_filter = list(
    constructor = \(p) kalman_filter(tau2 = p[1], sigma2_obs = p[2], sigma2_0 = p[3]),
    lower = c(1e-6, .01, .001), upper = c(1, 500, 10), stochastic = FALSE),
  softmax_rl = list(
    constructor = \(p) softmax_rl(alpha = p[1], beta = p[2]),
    lower = c(.01, .1), upper = c(1, 100), stochastic = TRUE),

  ## ---- round-2 additions ---------------------------------------------------
  uncfam_gamma = list(
    constructor = \(p) uncfam_gamma(X = p[1], B = p[2], C = p[3], gamma = p[4]),
    lower = c(UNCFAM_LOWER, gamma = .1), upper = c(UNCFAM_UPPER, gamma = 6), stochastic = FALSE),
  uncfam_elimination = list(
    constructor = \(p) uncfam_elimination(X = p[1], B = p[2], C = p[3], eps = p[4]),
    lower = c(UNCFAM_LOWER, eps = 0), upper = c(UNCFAM_UPPER, eps = 20), stochastic = FALSE),
  minerva2 = list(
    # feature dimensionality D fixed at the registry default (100): a
    # representational hyperparameter, not a free cognitive parameter
    constructor = \(p) minerva2(X = p[1], D = 100),
    lower = .1, upper = 50, stochastic = TRUE),
  tilles = list(
    constructor = \(p) tilles(x = p[1], b = p[2], alpha_0 = p[3]),
    lower = c(0, 0, 0), upper = c(1, 1, 1), stochastic = FALSE),

  ## ---- baseline and group-fit-only bonus ------------------------------------
  baseline = list(
    constructor = \(p) baseline(), lower = numeric(0), upper = numeric(0), stochastic = FALSE),
  fazlyt = list(
    constructor = \(p) xslMod(
      name = "fazly", description = "Fazly et al. 2010 model with a threshold parameter",
      model = \(params, data, control) XSLmodels:::fazly_model(
        params = list(lambda = params$lambda, beta = params$beta, alpha = 10,
                      epsilon = 0.001, theta = params$theta),
        data = data, control = control),
      params = list(lambda = p[1], beta = p[2], theta = p[3]), stochastic = FALSE),
    lower = c(.01, 2, .01), upper = c(2, 1000, 1), stochastic = FALSE)
)

# models that are group-fit but not cross-validated (as in round 1)
GROUP_ONLY <- c("baseline", "fazlyt")

# run order: cheap deterministic models first, so a partial run is still
# useful, then stochastic models roughly cheapest-first
RUN_ORDER <- c(
  "baseline", "fazly", "Bayesian_decay", "rescorla-wagner", "kalman_filter", "tilles",
  "kachergis", "novelty", "uncertainty", "strength", "uncfam_attention",
  "uncfam_predictive", "uncfam_gamma", "uncfam_elimination", "fazlyt",
  "trueswell2012", "guess-and-test", "pursuit", "softmax_rl", "minerva2",
  "kachergis_sampling"
)
stopifnot(setequal(RUN_ORDER, names(specs_v04)))
