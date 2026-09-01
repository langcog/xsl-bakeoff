# Shared model definitions for recreate_fits_with_package.R and
# compare_pre_package_vs_package.R -- maps each of the original bakeoff
# models onto the XSLmodels package, with EXACT parameter bounds/order
# copied from fit_group_and_cond.R and cv_group_fits.R, so DEoptim's
# bestmem lines up directly with the original fits' $optim$bestmem.
#
# Requires XSLmodels to already be loaded (library(XSLmodels)).
#
# Three models (strength, rescorla-wagner, fazlyt) fit a parameter that
# isn't simply "the model's leading parameters" (a fixed value sits between
# free ones, or there's a reparameterization), so they're built directly
# against the internal *_model() functions rather than the public
# constructors, to make the free/fixed split explicit and unambiguous.
#
# KNOWN DIFFERENCES between the original per-paper models and the package's
# implementations, confirmed by (a) reading both side by side and (b) an
# empirical same-parameters check (see compare_pre_package_vs_package.R's
# check #1): running the ORIGINAL fits' own best-fit parameters through the
# package's models and comparing item-level perf against the original
# fits' own perf, which isolates "does the model implementation agree" from
# "did DEoptim converge to the same optimum". Five models (kachergis,
# novelty, uncertainty, strength, rescorla-wagner) matched to within
# floating-point noise (~1e-13) on that check -- genuine confirmation the
# port is correct for those. The remaining four models did not, for the
# reasons below. For the two stochastic models NOT listed here (pursuit,
# kachergis_sampling), the same check showed differences of the same
# magnitude as simply running the package's own model twice with identical
# parameters (Monte Carlo noise from n_sim=500 simulations) -- i.e. no
# evidence of an actual implementation difference, just sampling noise.
#
#  - fazly (no threshold): the original computed perf = diag(probs) with NO
#    normalization; the package computes get_perf(probs) =
#    diag(probs)/rowSums(probs). Since rowSums(probs) is not close to 1
#    (the denominator involves beta, a "vocabulary size" prior ~1000s, not
#    the actual number of referents ~12-24), this is a substantial
#    difference and "fazly" fits/SSE are expected to differ materially.
#    "fazlyt" (with the theta threshold) is NOT affected -- both compute
#    diag(lexicon)/rowSums(lexicon+1e-12) identically.
#
#  - Bayesian_decay: at test time, the original computed
#    diag(chProb) where chProb = pWgO^chDec / outer(ones, colSums(pWgO^chDec))
#    -- i.e. diag(pWgO)^chDec normalized by its COLUMN sum (probability
#    mass across words, for a fixed object). The package's
#    get_perf(p_wgo, d=ch_dec) instead normalizes by the ROW sum
#    (probability mass across objects, for a fixed word) -- the choice
#    rule everywhere else in the package uses. Notably, the ORIGINAL
#    model's own source has a comment on the (separate, but analogous)
#    training-time normalization questioning this exact choice: "<- why is
#    this colSums? shouldn't it be rowSums(pWgO)?" -- so the package's
#    version may well be an intentional correction of something the
#    original author already suspected was wrong, rather than a porting
#    regression. Flagged here as a known difference either way, since it's
#    a substantive, not cosmetic, change in what the choice rule computes.
#
#  - guess-and-test: THIS IS A REAL BUG in the package, not a documented
#    design choice. models/stochastic/guess-and-test.R's disconfirmation
#    step -- "if the stored hypothesis is not on this trial, forget it" --
#    is implemented in R/model-guess_and_test.R as:
#      for (w in have_hypoths) {
#        if (length(which(m[w, ] == 1))) next
#        if (!is.element(which(m[w, ] == 1), tr_o)) m[w, ] <- m[w, ] * 0
#      }
#    But `have_hypoths` is already filtered to words THAT HAVE a stored
#    hypothesis (rowSums(m[tr_w,]) != 0), so `length(which(m[w,]==1))` is
#    always > 0 for every w in this loop -- the `next` fires unconditionally
#    and the disconfirmation check never runs. Hypotheses are therefore
#    never disconfirmed once formed, which is a core part of the
#    guess-and-test algorithm ("...on later encounters, learners attempt to
#    retrieve this hypothesis from memory and test it against a new
#    context, updating it only if it is disconfirmed"). Confirmed via the
#    same-parameters check to be well beyond Monte Carlo noise (~3x the
#    noise floor from running the same code twice). This should likely be
#    fixed in XSLmodels (drop the stray `if (length(...)) next` line) --
#    not attempted here since it's a substantive model-behavior change to
#    the package itself, out of scope for this comparison exercise.
#
#  - trueswell2012/propose_but_verify: when multiple words need a new
#    hypothesis on the same trial, the original drew new referents via
#    sample(tr_o, n, replace=FALSE) (no repeats across simultaneous new
#    hypotheses); the package uses replace=TRUE. This is a genuine
#    stochastic procedural difference (not just numerical noise), and is
#    confirmed (via the same-parameters check) to produce differences
#    beyond what Monte Carlo noise alone would explain.
#
# All other models' performance formulas were verified to match exactly,
# both by reading the code and via the same-parameters check.

known_discrepancies <- c(
  fazly = "perf formula differs (diag(probs) vs. diag(probs)/rowSums(probs))",
  Bayesian_decay = "choice rule normalizes over the wrong axis (colSums vs. rowSums) -- may be an intentional fix of a bug the original author flagged in their own code",
  `guess-and-test` = "REAL BUG in the package: an inverted guard clause disables hypothesis disconfirmation entirely",
  trueswell2012 = "stochastic sampling differs (replace=FALSE vs. TRUE for simultaneous new hypotheses), beyond what Monte Carlo noise explains"
)

model_specs <- list(
  kachergis = list(
    constructor = \(p) uncfam(X = p[1], B = p[2], C = p[3], variant = "entropy"),
    lower = c(.001, .1, .5), upper = c(5, 15, 1), stochastic = FALSE
  ),
  novelty = list(
    constructor = \(p) uncfam(X = p[1], B = p[2], C = p[3], variant = "novelty"),
    lower = c(.001, .1, .5), upper = c(5, 15, 1), stochastic = FALSE
  ),
  uncertainty = list(
    constructor = \(p) uncfam(X = p[1], B = p[2], C = p[3], variant = "uncertainty-only"),
    lower = c(.001, .1, .5), upper = c(5, 15, 1), stochastic = FALSE
  ),
  strength = list(
    # original: X, C free; B (uncertainty/entropy weight) fixed at 0
    constructor = \(p) xslMod(
      name = "strength",
      description = "uncfam() familiarity-only special case (B = 0)",
      model = \(params, data, control) XSLmodels:::uncfam_model(
        params = list(X = params$X, B = 0, C = params$C, variant = "entropy"),
        data = data, control = control
      ),
      params = list(X = p[1], C = p[2]),
      stochastic = FALSE
    ),
    lower = c(.001, .1), upper = c(5, 1), stochastic = FALSE
  ),
  Bayesian_decay = list(
    constructor = \(p) bayesian_decay(alpha = p[1], delta = p[2], ch_dec = p[3]),
    lower = c(1e-5, 1e-5, 1e-5), upper = c(10, 10, 10), stochastic = FALSE
  ),
  `rescorla-wagner` = list(
    # original: beta (as a proportion of lambda), C, lambda free; alpha
    # (salience) fixed at 1
    constructor = \(p) xslMod(
      name = "rescorla_wagner",
      description = "Rescorla-Wagner with alpha (salience) fixed at 1",
      model = \(params, data, control) XSLmodels:::rescorla_wagner_model(
        params = list(C = params$C, alpha = 1, beta = params$beta, lambda = params$lambda),
        data = data, control = control
      ),
      params = list(beta = p[1], C = p[2], lambda = p[3]),
      stochastic = FALSE
    ),
    lower = c(1e-5, 1e-5, 1e-5), upper = c(1, 1, 1), stochastic = FALSE
  ),
  fazly = list(
    constructor = \(p) fazly(lambda = p[1], beta = p[2]),
    lower = c(1e-10, 2), upper = c(2, 20000), stochastic = FALSE
  ),
  trueswell2012 = list(
    constructor = \(p) propose_but_verify(alpha = p[1], alpha_increase = p[2]),
    lower = c(.0001, .0001), upper = c(1, 1), stochastic = TRUE
  ),
  `guess-and-test` = list(
    constructor = \(p) guess_and_test(f = p[1], sa = p[2]),
    lower = c(.0001, .0001), upper = c(1, 1), stochastic = TRUE
  ),
  pursuit = list(
    constructor = \(p) pursuit(gamma = p[1], threshold = p[2], lambda = p[3]),
    lower = c(1e-5, 1e-5, 1e-5), upper = c(1, 1, 1), stochastic = TRUE
  ),
  kachergis_sampling = list(
    constructor = \(p) uncfam_sampling(X = p[1], B = p[2], C = p[3], K = 1),
    lower = c(.001, .1, .5), upper = c(5, 15, 1), stochastic = TRUE
  )
)

# bonus: fazly with a threshold parameter (in group_fits.Rdata's function
# definition, but not in the actually-saved artifact or in cv_group_fits at
# all -- included here for completeness, but not part of the main
# comparison since the paper's tables don't use it)
fazlyt_spec <- list(
  constructor = \(p) xslMod(
    name = "fazly",
    description = "Fazly et al. 2010 model with a threshold parameter",
    model = \(params, data, control) XSLmodels:::fazly_model(
      params = list(lambda = params$lambda, beta = params$beta, alpha = 10,
                    epsilon = 0.001, theta = params$theta),
      data = data, control = control
    ),
    params = list(lambda = p[1], beta = p[2], theta = p[3]),
    stochastic = FALSE
  ),
  lower = c(.01, 2, .01), upper = c(2, 1000, 1), stochastic = FALSE
)
