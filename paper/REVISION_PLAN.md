# XSL bake-off paper — revision plan

_Drafted 2026-09-05, after XSLmodels reached release-ready (0.3.0). Pairs with the
package-recreated fits committed in `7cdfd92`._

_2026-09-06 — decisions locked (see §3):_
- **Scope C**: 53 conditions, 15 models (11 original + 4 new), **plus** a real
  naturalistic-corpus results section.
- **Full journal article** (not a CogSci short paper). Appendix folds into the main
  text as supplementary material; literature review expands.
- **Authors: Kachergis & Frank.** No Kevin Liang.

---

## 1. Where the manuscript stands

Three overlapping Rmd files are in `paper/`:

| file | author line | notes |
|---|---|---|
| `paper.Rmd` | Kachergis & Frank | CogSci-final text (submitted July 2024) |
| `paper-using-package.Rmd` | Kachergis & Frank | **byte-identical to `paper.Rmd`**; intent (per `7cdfd92`) is that this one points at the package-recreated fits |
| `journal-paper.Rmd` | Kachergis, **Liang**, Frank | older fork; adds a Liang author line, a stubbed "Corpus Results" section, a few later-cut sentences. 566 vs 553 lines. |

**Action 0 (housekeeping, do first).** Consolidate to one file:

1. `git mv paper/paper.Rmd paper/journal-paper.Rmd` (overwriting the old fork) — the
   name now matches what we're writing.
2. Drop the Liang author line; keep `\author{Kachergis \and Frank}`.
3. Point the three `load()` calls at the package-recreated fits (§ Results below).
4. Salvage from the old `journal-paper.Rmd` before it's overwritten: the "Corpus
   Results" section stub (now backed by real functionality) and the couple of extra
   framing sentences worth keeping. Everything else in that fork is superseded.
5. `git rm paper/paper-using-package.Rmd`.
6. Switch the CogSci `output:` / `cogsci2016::cogsci_paper` machinery to a journal
   template (see §4 — this affects `document-params`, the `cogsci.sty` include, the
   two-column figure envs, and all the `\vspace` hacks, which can go).

### The fits the manuscript loads no longer all exist

The current Rmd reads three files:

- `fits/group_fits.Rdata` ✅ exists (package recreation, 44 conditions) — but we're
  moving to 53, so this gets **replaced** by `fits/group_fits_all_experiments.Rdata` +
  `fits/group_fits_new_models.Rdata`.
- `fits/cv_group_fits.Rdata` ❌ **not at top level** — only `fits/pre-package/`. Replaced
  by `fits/cv_all_experiments.Rdata` + `fits/cv_new_models.Rdata` (both **incomplete** —
  §6).
- `fits/generalization_exp_results.RData` ❌ **not at top level**. Never regenerated with
  the package. New script needed (§6).

---

## 2. What changed since the CogSci submission

### 2a. The models are now the XSLmodels package

All 11 paper models were re-mapped onto the package (`fits/model_specs.R`) and re-fit
(`fits/recreate_fits_with_package.R`). `fits/compare_pre_package_vs_package.R` documents
a three-tier equivalence check. Bottom line:

- **5 models reproduce the published fits to floating-point precision**: FU (`kachergis`),
  Nov (`novelty`), Unc (`uncertainty`), Str (`strength`), R-W (`rescorla-wagner`).
- **4 models changed** for identified reasons (not porting bugs, mostly):
  - **PA / `fazly`** — package normalizes test perf as `diag(P)/rowSums(P)`; the paper
    code used bare `diag(P)`. Substantive. New fit rails `beta` to its upper bound (20000).
  - **BD / `Bayesian_decay`** — package choice rule normalizes over objects (row), paper
    code over words (column). The original author left a `# shouldn't this be rowSums?`
    comment on the analogous training step, so the package version is arguably the fix.
  - **PbV / `trueswell2012`** — package draws simultaneous new hypotheses with
    replacement; paper code without. Stochastic-procedure difference.
  - **GnT / `guess-and-test`** — the paper-era package had an inverted guard that
    disabled hypothesis disconfirmation entirely. **Fixed in XSLmodels 0.2.0.** The
    recreated fit uses the corrected model.
- Pursuit and FUs (`kachergis_sampling`) differ only by Monte-Carlo noise (n_sim).

### 2b. Four new models exist and are fit

Added to the package after the paper, fit to all 53 conditions
(`fits/group_fits_new_models.Rdata`, `fits/cv_new_models.Rdata`):

- **`uncfam_attention`** — BAM + system-level attention (Fitneva & Christiansen 2015)
- **`uncfam_predictive`** — BAM with an item-level RW-style prediction-error update
- **`kalman_filter`** — Kalman-filter generalization of R-W (adaptive learning rate;
  Dayan & Kakade 2000; Kruschke 2008)
- **`softmax_rl`** — Q-learning with softmax choice; the one model that makes a discrete
  per-word choice each trial. **In the all-53 group fit it is competitive with the top
  associative models** (group SSE 25.1, r .63) — worth foregrounding.

### 2c. The dataset grew: 44 → 53 conditions

Package `xsl_datasets` now has 53 conditions (`+9`, added in XSLmodels 0.2.0):

- 2 × Kachergis, Yu & Shiffrin 2009 (temporal contiguity)
- 3 × Suanda, Mugwanya & Namy 2014 (contextual diversity, 6-year-olds)
- 4 × Koehne, Trueswell & Gleitman 2013 (meaning-order)

| | conditions | participants | items |
|---|---|---|---|
| paper as submitted | 44 | 1,696 | 726 |
| package `xsl_datasets` | 53 | 2,266 | 818 |

**Consequence for the Generalization section:** Suanda 2014 and Koehne 2013 were
*generalization / held-out* experiments in the paper (Table 3). They are now *in* the
fitting set. **Medina 2011 and Smith & Yu 2008 are still external** — those two become
the held-out generalization test. Suanda/Koehne can be reported as "formerly held out,
now in-sample" to preserve the narrative thread. (Trial orders for all four live in
`other_exps/`.)

### 2d. Naturalistic corpora are now real

`rollins_corpus`, `fm_corpus`, and the `fgt2009()` / `fgt2009_rsa()` Bayesian model
(FGT 2009 + RSA) were ported into the package with gold-lexicon scoring
(`get_fscore`/`get_roc`/`get_roc_max`) and a fitting workflow. The package already ships:
`vignettes/corpora.Rmd` and `tests/bakeoff_comparison/{corpus_gold_comparison,corpus_gold_fits}.R`
(+ `.rds`/`.png` outputs) + `corpus_report.md`. This is the backbone of the new
Corpus Results section (§4).

---

## 3. Locked decisions & remaining minor calls

**Locked:** Scope C · full journal article · Kachergis & Frank.

**Minor calls still open (all can be one-liners in Methods):**

1. **`fazly` beta-at-bound.** Under `diag(P)/rowSums(P)` normalization the optimizer
   finds a flat ridge and `beta` rails to 20000. Options: (a) widen the bound and re-fit
   (won't help — it's flat), (b) fix `beta` at a principled value and drop it as a free
   parameter (PA becomes 1-param), (c) keep as-is with a footnote. **Recommend (b) or (c).**
2. **BD choice-rule change** — adopt the package's row-normalized version (recommended;
   note it corrects a normalization the original author already flagged) vs. force the
   old column-normalized behavior. **Recommend adopt + one Methods sentence.**
3. **n_sim for stochastic models** — original was 500; the 53-condition scripts use 200.
   Either re-run the finals at 500 (slow) or state 200 and note MC-noise magnitude is
   below the SSE differences that matter. **Recommend 200 + a sentence.**
4. **Journal target** — determines the exact template/`.cls`. (Cognition? Cognitive
   Science? Open Mind? JML?) Needed before the final typeset pass, not before the writing.

---

## 4. Section-by-section

### Title / Abstract
- Keep the title or lengthen for a journal ("A large-scale comparison of
  cross-situational word-learning models" is fine).
- `44` → `53` conditions; `1,696` → `2,266` participants; `several models` → `15 models`.
- Add: models now also tested on naturalistic parent–child corpora.
- "two best-fitting models" — still true (FUs + PA lead every regime); consider naming
  `softmax_rl` as a strong newcomer.
- Journal abstracts can run longer — the 152-word CogSci limit no longer binds.

### Introduction (expand for journal length)
- Keep the Quine / cross-situational framing.
- Expand the model taxonomy: logical inference / hypothesis-testing / associative, **plus**
  RL (`softmax_rl`, `kalman_filter`) and attention/prediction BAM variants. Cite
  Fitneva & Christiansen 2015, Dayan & Kakade 2000, Kruschke 2008, and the RL word-learning
  literature.
- New paragraph motivating the naturalistic-corpus test: lab paradigms are clean but
  small and homogeneous; does any of this transfer to CHILDES-scale input? Cite
  Frank, Goodman & Tenenbaum 2009; Yu & Ballard 2007.
- Fold in the "models mimic each other / identifiability" thread from the current
  Discussion as a stated goal, not just a finding.
- State up front that everything is released as the `XSLmodels` R package (currently
  only a Discussion aside).

### Method — Procedure
- Fitting is now done *through the package*: `get_group_model_fit()` /
  `get_crossvalidated_model_fit()`, `DEoptim`, SSE weighted by N.
- Reconcile `n_sim` text with the final runs (§3.3).
- Short "Implementation notes" paragraph: PA normalization, BD choice axis, GnT
  disconfirmation fix — one sentence each, with a pointer to the package's NEWS/commit.

### Method — Models
- Add a subsection per new model (same treatment as existing: mechanism, free params,
  test-time choice rule):
  - **`softmax_rl`** — RL / Q-learning; 2 params (learning rate α, softmax temp β)
  - **`kalman_filter`** — adaptive-LR associative; 3 params (τ², σ²_obs, σ²_0)
  - **`uncfam_attention`**, **`uncfam_predictive`** — BAM variants; 3 params each
- GnT description: now honestly says disconfirmation is implemented (it wasn't in the
  fits behind the submitted paper).
- Consider a summary table: model × class × #params × key mechanism, for all 15.

### Method — Data
- `44` → `53`; update provenance (add Kachergis/Yu/Shiffrin 2009 ×2, Suanda et al. 2014
  ×3, Koehne et al. 2013 ×4).
- Re-derive "18–108 trials", "12–24 pairs", AFC ranges from the 53-condition set.
- Repoint the condition-summary table (now in `appendix.Rmd`) at `xsl_datasets`, not the
  frozen `data/combined_data.RData`. Promote it to a numbered supplementary table.
- New **Corpora** subsection: describe `rollins_corpus` (Rollins/CHILDES, 619 utts, 416
  words, 22 objects, 34 gold pairs) and `fm_corpus` (Frank/Tenenbaum/Fernald, 4763 utts,
  1122 words, 30 objects, ~41 gold pairs), gold-lexicon construction, and that scoring is
  by ROC/F against the gold lexicon (no human accuracy vector, so no SSE).

### Results

**Table 1** — regenerate from the 53-condition package fits.
- Published CV ranking (n=726): FUs 17.7 < PA 18.7 < FU 18.9 < Nov 22.0 < Unc 23.1 <
  BD 24.0 < GnT 31.3 < R-W 31.8 < Str 39.0 < Pur 41.7 < PbV 42.9.
- New package CV on **53** conditions (n=818, **FUs + softmax_rl still to fit**):
  PA 23.4 < BD 25.7 < FU 25.8 < uncfam_attention 30.4 < Nov 30.7 < Unc 31.5 < PbV 31.9 <
  R-W 34.0 ≈ kalman 34.0 < GnT 34.3 < uncfam_predictive 42.8 < Pur 48.7 < Str 52.6.
- Ranking stable at the **top** (PA / FU / BD / FUs cluster) and **bottom** (Str, Pur).
  PbV moves up sharply.
- Add a P (param count) column and a model-class column.

**Text around Table 1**
- "Kachergis sampling best, then Fazly" — holds in the group fit (FUs 20.5 ≤ PA 21.7 on
  53 conds; FUs 18.0 ≤ PA 19.6 on 44). Recheck after CV runs finish.
- "baseline SSE = 32.0 and r = 0.53" — package baseline is SSE 31.8 (44) / 34.0 (53),
  **item-level r = 0.62**, condition-level r = 0.73. The `0.53` is stale; fix consistently.

**Table 2** (model–model prediction correlations) & the Discussion correlations —
regenerate. Most pairwise r's move ≤0.03; a few move a lot (§5). The headline change:
"PbV & GnT made correlated predictions (r = .65)" → **≈ .94**.

**Figure 1** (human vs. model scatter) — regenerate; expand to 15 panels. The "PbV, GnT,
Pursuit respond more extremely than people" caption claim survives.

**Generalization** — restructure (§2c):
- Held-out set = **Medina 2011 + Smith & Yu 2008**. Report RMSE vs. human, using the
  53-condition group-fit parameters.
- Suanda 2014 + Koehne 2013: report as "was held-out, now in-sample" — do their fits
  look like the rest of the in-sample conditions, or are they still hard?
- Needs the new package generalization script (§6). Re-check every specific claim
  ("Koehne best fit by novelty", "Medina best fit by R-W", "Smith & Yu best fit by R-W",
  "Suanda best fit by BD").

**New: Corpus Results** — from the package's `tests/bakeoff_comparison/`:
- Run all bake-off models + `fgt2009()` on `rollins_corpus` and `fm_corpus`; score
  against the gold lexicon (best-threshold F / ROC-max).
- Two regimes, mirroring the lab analysis: (a) **transfer** — models at their
  lab-fitted parameters; (b) **corpus-fitted** — parameters re-fit to maximize
  gold-lexicon F on each corpus.
- Headline from the package work: `bayesian_decay` and `fazly` recover the gold lexicon
  best on both corpora; `fgt2009` close behind with the highest precision. Fitting lifts
  every model, dramatically for those whose lab-tuned defaults transfer poorly.
- Discuss: do the lab winners (FUs, PA) stay winners on naturalistic input? (PA yes; the
  Kachergis family is middling — a real finding.)

### Discussion (rewrite against final numbers)
- "11 models" → 15. Update every inline stat against §5.
- **PbV story changes.** With disconfirmation fixed and sampling corrected, PbV's fit
  improves markedly (44-cond CV SSE 42.9 → rerun; group 30.8 → 27.9) and its predictions
  converge with GnT (r ≈ .94). The "hypothesis-testing models fit poorly" claim needs
  softening; the "they make near-identical predictions" claim gets stronger.
- "R-W ≈ baseline (r = .99)" → **≈ 1.00**. Fine.
- "Pursuit outperforms humans (M_Pur, M_hum = 0.48)" → M_Pur ≈ 0.50, M_hum 0.485.
  Effect is now marginal — recheck the sentence.
- Add a naturalistic-transfer paragraph: what the corpus results say about whether
  cross-situational learning scales.
- Add the four new models to the "future / included work" framing; `softmax_rl` deserves
  its own sentence as a strong, mechanistically distinct entrant.
- Keep and update: identifiability, optimal experiment design, need for developmental
  data + diverse samples.

### Supplementary material (was the appendix)
- Repoint `cv_group_fits.Rdata` and `combined_data.RData` loads at package outputs /
  `xsl_datasets`.
- Parametrize the hard-coded 11-model assumptions (`xm_mat[,2:12]`, the `case_when`
  relabel blocks) so they don't silently break at 15 models.
- Regenerate the misfit heatmaps and best/worst-condition tables; re-check the `filt`-
  condition and `freq369-3x3loCD` prose.
- Add: full parameter tables for every model × fold, corpus gold-lexicon details.

---

## 5. Numbers that changed materially

"Material" = SSE shift ≳ 1.5, or r / pairwise-r shift ≳ 0.05, or a rank change, on the
**44-condition group fit** (the only clean apples-to-apples comparison: same data, same
DEoptim settings, published vs package).

| quantity | published | package (44-cond) | change | why |
|---|---|---|---|---|
| **PbV group SSE** | 30.8 | **27.9** | −2.9, r .51→.56 | sampling w/o replacement; disconfirmation fixed |
| **PbV CV SSE** (old data) | 42.9 | *(rerun)* | — big | same |
| **Pursuit group SSE** | 40.6 | **37.9** | −2.7, r .59→.56 | MC noise + param mapping |
| **BD group SSE** | 23.7 | **22.2** | −1.5, r .63→.65 | choice rule row- vs column-normalized |
| **PA group SSE** | 18.8 | **19.6** | +0.8, r .73→.70 | `diag(P)/rowSums(P)` normalization |
| **PA `beta`** | 3.9 | **20000 (bound)** | rails | flat ridge under new normalization — §3.1 |
| **GnT group SSE** | 31.0 | 30.6 | ~0 (r +.01) | disconfirmation now runs; net effect small |
| FU, Nov, Unc, Str, R-W, baseline | — | — | **0 (exact)** | verified FP-identical |
| FUs group SSE | 17.8 | 18.0 | MC noise | — |
| **corr(PbV, GnT)** | .65 | **≈ .94** | +0.29 | PbV predictions shifted toward GnT |
| **corr(PbV, R-W)** | .67 | ≈ .87 | +0.20 | same |
| **corr(PA, R-W)** | .84 | ≈ .93 | +0.09 | — |
| corr(R-W, baseline) | .99 | ≈ 1.00 | +0.01 | — |
| corr(Pur, FUs) | .74 | .73 | ~0 | — |
| baseline item-level r | .53 (in text) | **.62** | +0.09 | text value appears stale regardless |

**Rank order:** top (FUs / PA / FU) and bottom (Str / Pur) unchanged. PbV climbs from
last to mid-pack. BD holds ~6th. The paper's core conclusion — associative models beat
hypothesis-testing models, but many models mimic each other — **survives**, and the
"mimic each other" point gets stronger.

On the **53-condition** set the ordering shifts more because the 9 new conditions
(Koehne meaning-order, Suanda child data especially) are harder: R-W, GnT, Pur, Str all
lose ground; `softmax_rl` and `uncfam_attention` slot into the upper-middle.

---

## 6. Computational to-do

Status against scope C. Scripts exist unless noted; all live in `fits/`.

| task | script | status |
|---|---|---|
| Group fit, 53 cond, 11 orig + baseline + fazlyt | `fit_all_models_all_experiments.R` | ✅ done → `group_fits_all_experiments.Rdata` |
| Group fit, 53 cond, 4 new models | `fit_new_models_all_experiments.R` | ✅ done → `group_fits_new_models.Rdata` |
| **CV, 53 cond, 11 orig models** | `cv_all_experiments.R` | 🔄 **10/11 — `kachergis_sampling` (FUs) fitting now** (run PID 39640, started ~Sep 3). Will finish + re-save on its own. |
| **CV, 53 cond, 4 new models** | `cv_new_models.R` | ✅ **done** — all 4 incl. `softmax_rl` (finished 2026-09-06) → `cv_new_models.Rdata` |
| **Package generalization run** (Medina, Smith & Yu; + Suanda/Koehne as now-in-sample) | `generalization_with_package.R` | ✅ **done** → `generalization_with_package.RData`. Section + table + prose rewritten. Re-check model names in the prose once the group fit is final. New finding: baseline / R-W / Kalman / RL generalize best to the held-out experiments; Koehne no longer best-fit by the novelty model. |
| **Corpus results** (transfer + corpus-fitted, all models + fgt2009, both corpora) | adapt `XSLmodels/tests/bakeoff_comparison/corpus_gold_{comparison,fits}.R` | 🟡 largely done in the package; needs a paper-facing wrapper that emits the table/figure the Rmd will load |
| Repoint paper + appendix at `xsl_datasets` / package fit files | — | ❌ Rmd edits (Action 0.3) |
| Re-knit, diff every inline `\r` stat vs. `cogsci-final.pdf` | — | ❌ final pass |
| Handle `fazly` beta (§3.1); adopt/annotate BD rule (§3.2) | `model_specs.R` | ❌ small |
| Parametrize appendix's hard-coded 11-model blocks | `appendix.Rmd` | ❌ small |

**Runtime note:** the two CV resumes are the long pole. FUs CV ≈ 5 folds of a stochastic
fit at n_sim=200; softmax_rl CV is much slower. Budget for an overnight (or a few) on a
machine that stays awake. Everything else is minutes-to-an-hour or pure writing.

---

## 7. Suggested sequence

1. **Action 0** — consolidate to one `journal-paper.Rmd`, drop Liang, `git rm` the
   duplicates, commit. (Cheap, unblocks everything.)
2. **CV fits** — `cv_new_models.R` done; `cv_all_experiments.R` finishing FUs now.
   Nothing to kick off; just wait for the FUs row and re-save.
3. **While they run — number-independent writing:**
   - Expand Intro (taxonomy + corpus motivation + package).
   - Write the 4 new-model Method subsections.
   - Update Data section counts + provenance; write the Corpora subsection.
   - Switch the template to the target journal's; strip the CogSci `\vspace` hacks.
   - Draft author response / cover-letter notes on the model-implementation changes.
4. ~~**Generalization script**~~ — done (`generalization_with_package.R`). Re-run once
   the group fit is final (it reads `group_fits_all_experiments.Rdata`).
5. **Corpus results** — write the paper-facing wrapper around the package scripts, run,
   draft the section.
6. **When CV lands** — regenerate Tables 1–3, Figure 1, supplementary; walk the §5 table
   and every inline stat.
7. **Rewrite Discussion** against final numbers (esp. the PbV story).
8. **Final knit + stat-by-stat diff** vs. `cogsci-final.pdf`; typeset pass for the venue.
