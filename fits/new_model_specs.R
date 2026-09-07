# Specs for the model variants added to XSLmodels after the original paper:
# uncfam_attention ("BAM+Attention"), uncfam_predictive ("BAM+Prediction"),
# kalman_filter, and softmax_rl. Kept separate from model_specs.R (the
# original paper's 11 models, needed verbatim for the pre-package/package
# comparison) because there is no original bakeoff fit to compare these
# against.
#
# Bounds/starting values match XSLmodels' own xsl_model_registry() (see
# R/helper.R), the package's canonical bounds for these models. kalman_filter
# and softmax_rl bounds were widened after an initial fit pinned parameters
# at the registry defaults (XSLmodels PR #7).
#
# Sourced by fit_new_models_all_experiments.R, cv_new_models.R, and
# generalization_with_package.R. Requires XSLmodels to be loaded.

new_model_specs <- list(
  uncfam_attention = list(
    constructor = \(p) uncfam_attention(X = p[1], B = p[2], C = p[3]),
    lower = c(0.01, 0.8, 0.8), upper = c(0.5, 1.0, 1.0), stochastic = FALSE
  ),
  uncfam_predictive = list(
    constructor = \(p) uncfam_predictive(X = p[1], B = p[2], C = p[3]),
    lower = c(0.01, 0.8, 0.8), upper = c(0.5, 1.0, 1.0), stochastic = FALSE
  ),
  kalman_filter = list(
    constructor = \(p) kalman_filter(tau2 = p[1], sigma2_obs = p[2], sigma2_0 = p[3]),
    lower = c(0.000001, 0.01, 0.001), upper = c(1, 500, 10), stochastic = FALSE
  ),
  softmax_rl = list(
    constructor = \(p) softmax_rl(alpha = p[1], beta = p[2]),
    lower = c(0.01, 0.1), upper = c(1, 100), stochastic = TRUE
  )
)
