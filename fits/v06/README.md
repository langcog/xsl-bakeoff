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
