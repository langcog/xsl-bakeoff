# v06 fits

Fits against XSLmodels after its 0.5.0 CRAN release. v05 (`fits/v05/`) is
the record of the released package; v06 adds models and analyses that need
newer XSLmodels, each in its own subdirectory with a pinned private library
(`<dir>/Rlib`, from `<dir>/XSLMODELS_COMMIT`; not tracked).

v06 reuses v05's inputs, read-only: the 47 `xsl_datasets` conditions, the CV
folds (`fits/v05/adults/cv_fits.Rdata`), the other models' specs
(`fits/v05/specs.R`) and group fits (`fits/v05/adults/group_fits.Rdata`), so
its results are directly comparable to v05's.

## yf2015/ (2026-11-17)

The trial-level Yurovsky & Frank (2015) model, `yf2015()`, from XSLmodels PR
#46 (pinned at 49d9d61, branch add-yurovsky-frank2015, not yet merged).
`queue_yf2015.sh` runs the first two steps; `score_queue.sh` runs the third.

1. **`refit_yf2015.R`**: group fit and 5-fold CV of `yf2015` (mu = 0) and
   `yf2015_me` (mu free), in mean-field mode, with v05's procedure (n_subj-
   weighted SSE, DEoptim NP = 100, itermax = 100, best of 3 restarts).
   Results go to `yf2015_fits.Rdata`. `yf2015_refit_summary.rds` ranks them
   against the v05 models.
   - yf2015_me: CV SSE .456, the lowest in the bake-off (fazly .483).
   - yf2015: CV SSE .639.
2. **`generalize_yf.R`**: the group fits predict `yurovsky_frank2015` with no
   refitting, via `yf2015_paradigm()`. Output: `generalization_yf.rds`.
   - The pattern transfers: Exp 1 r^2 = .86.
   - The levels don't: Exp 1-2 log-likelihood -3014, against -364 for the
     paper's model fit to Exp 1.
   - The `xsl_datasets` fit sets encoding strength gamma at about .3, against
     1.6 in the paper. End-of-training tests can't separate encoding from
     decay.
3. **`score_yf_all.R`**: every v05 model, plus the two YF models, is scored
   on Experiments 1-3 with no refitting.
   - Method: a model's exposure-trial response is its own choice right after
     the trial, and Same/Switch/New Label are conditioned on it per
     simulated learner. Validated against `yf2015_paradigm()` with
     `--validate`.
   - Each model gets one lapse rate, fit to Exp 1; Exps 2-3 are held out.
     Per-model results are in `yf_scores/`, and the summary is
     `yf_scores_summary.rds`.
   - Best held-out log-likelihood: propose-but-verify and guess-and-test
     (-1564), then minerva2 (-1683). The paper's model fit to Exp 1 gives
     -1445, and chance -4143.
   - Luce associative models predict Same = Switch = 1: never-seen objects
     have zero strength.

### Fixed gamma (2026-11-18)

`yf2015_g` (sigma and lambda free) and `yf2015_me_g` (adds mu) fix gamma at
1.62. That is the paper's Integrated model refit to Exp 1 with the indexing
corrected; the paper reports 1.50. `queue_yf2015.sh` now also runs
`score_yf_all.R`. `generalize_yf.R` clamps probabilities as the scorer does,
because yf2015_g predicts Same = 1 at interval 1.

| model | CV SSE (xsl_datasets) | sigma | lambda | mu | held-out loglik (Exps 2-3) |
|---|---|---|---|---|---|
| yf2015_me | .456 | .926 | .237 | 3.52 | -3330 |
| yf2015 | .639 | .967 | .261 | - | -3096 |
| yf2015_me_g | .913 | .848 | 1.02 | 2.48 | -1874 |
| yf2015_g | .979 | .864 | .866 | - | -1721 |

- Fixing gamma lets the model transfer to the YF experiments: yf2015_g is
  third on held-out log-likelihood, behind propose-but-verify and
  guess-and-test (-1564) and just behind minerva2 (-1683). It has no lapse
  (fitted lapse ~0).
- The cost is on `xsl_datasets`. CV SSE rises from .64 to .98, which ranks
  21st of 27. With gamma fixed, the fit raises the decay rate lambda from .17
  (paper) to .87, so it can still match end-of-training accuracy. CV fold
  parameters are stable: sigma .80-.89, lambda .86-1.05.
- The fixed-gamma models no longer fit Exp 1 better than the free-gamma ones
  on r^2: .81 against .86. They do fit Exps 1-2 better (r^2 .74 against .63)
  and Exp 3 competitor choices better (r .82 against .51).
- mu helps neither dataset as much once gamma is fixed: yf2015_me_g has CV SSE
  .913 and held-out -1874.
- No setting of gamma suits both datasets: `xsl_datasets` wants slow decay
  and weak encoding, while the YF interval manipulation wants strong
  encoding.

### Plots against human data (2026-11-18)

`plot_yf_models.R` (run after `score_yf_all.R`) plots human performance in
Exps 1-2 and Exp 3 against models, and saves the predictions and fit
statistics in `yf_models_predictions.rds`.

- `yf_models_exp12.png`, `yf_models_exp3.png`: the paper's Integrated model
  (published parameters), the trial-level `yf2015()` at the corrected Exp 1
  fit with mu = 1.41, and the yf2015_g and yf2015 bake-off fits.
  - At the paper's own parameters, `yf2015()` equals the paper's model on Same
    and Switch to 1e-16.
  - Exps 1-2 r^2: paper .970, trial-level .975, yf2015_g .73, yf2015 .61.
- `yf_models_assoc_exp12.png`, `yf_models_assoc_exp3.png`: fazly,
  uncfam_gamma and kachergis_sampling, from the scorer's cached predictions,
  with and without the lapse fit to Exp 1 (.47 for each).
  - On Exps 1-2 they are identical: Same = Switch = 1 and New Label at chance
    without a lapse. Exps 1-2 r^2 is .65 with the lapse.
  - On Exp 3 (correlation with the human means, with the lapse): fazly .88,
    uncfam_gamma .89, kachergis_sampling .91. Only kachergis_sampling, which
    is stochastic, separates Same from Switch targets.

## clusters/ (2026-11-18)

`cluster_models.R` asks which models, and which experiments, cluster together.
It uses saved predictions only (v05 group fits plus the yf2015 variants) and
runs in about a minute. The findings are in `clusters/CLUSTERS.md`.

- **Stage 1** compares models by their predictions on a chance-corrected
  scale, with humans included: dendrograms, MDS, and each model's distance to
  humans against the human noise floor.
- **Stage 2** looks at residuals:
  - per-condition bias and rank heatmaps, both clustered;
  - shared misfit across all models;
  - a PCA of what separates the models;
  - relations to item and condition design features.
