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

## R3. Round 3 (2026-10-07): writing up the 0.4.0 results — current plan

**Supersedes the status tables in §0 and below.** XSLmodels 0.4.0 is released; the
round-2 refit is complete: 19 models + baseline, 56 conditions, group fit + 5-fold CV,
plus holdout generalization (`fits/v04/`, summary in `fits/v04/summary.md`). The fit
build (`d0e2230`) has the same model code as the v0.4.0 release (only `xsl_fit()`
differs, which the refit doesn't use), so the paper can say "XSLmodels 0.4.0".

### R3.1 What the results say

CV test SSE by round. Item-level SSE summed over items, as the paper's Table 1 reports
it; ranks are within each round. Published = CogSci research code, 44 conditions /
726 items. Rounds 1–2 = the package, 53 / 818 and 56 / 872 items. SSE magnitudes
aren't comparable across rounds (different item counts); ranks are.

| model | published rank | round 1 rank (0.3.0) | **round 2 rank (0.4.0)** | round 2 CV SSE | round 2 r |
|---|---|---|---|---|---|
| FUs | 1 | 1 | **1** | 21.97 | .72 |
| PA | 2 | 2 | **2** | 23.43 | .68 |
| BD | 6 | 3 | **3** | 25.33 | .64 |
| RL (new) | – | 4 | **4** | 26.66 | .63 |
| M2 (new, 1 param) | – | – | **5** | 27.35 | .61 |
| FU-E (new) | – | – | **6** | 27.55 | .66 |
| FU | 3 | 5 | **7** | 27.84 | .66 |
| FU-G (new) | – | – | **8** | 28.24 | .65 |
| FU-A | – | 6 | **9** | 28.45 | .66 |
| Nov | 4 | 7 | **10** | 29.11 | .62 |
| PbV | 11 | 9 | **11** | 32.79 | .53 |
| Unc | 5 | 8 | **12** | 33.26 | .58 |
| TF (new) | – | – | **13** | 34.05 | .54 |
| GnT | 7 | 12 | **14** | 35.45 | .51 |
| KF | – | 11 | **15** | 35.47 | .61 |
| R-W | 8 | 10 | **16** | 35.50 | .61 |
| Str | 9 | 15 | **17** | 48.80 | .41 |
| Pur | 10 | 14 | **18** | 50.09 | .53 |
| FU-P | – | 13 | **19** | 51.11 | .52 |
| baseline (group fit) | – | – | – | 35.47 | .61 |

Headlines for the rewrite:

1. **The top two are stable across all three versions** (FUs and PA). Which one is
   first depends on the metric (see D1): FUs by summed item SSE; PA by the fitted
   objective (participant-weighted condition SSE, `summary.md`: PA .358, FUs .374,
   BD .379). The CV fold ranges overlap heavily, so these three are not clearly
   separable.
2. **BD climbs from 6th to 3rd** in both package rounds. This comes from the
   choice-rule correction, not the new data.
3. **FU falls from 3rd to 7th** (update_known fix plus new competitors). Its variants
   cluster around it, and none of the extensions helps out of sample:
   - **FU-E's elimination weight goes to ε = 0** (pinned at its lower bound), so it
     is FU exactly.
   - **FU-G** (γ ≈ 0.5, diminishing returns on familiarity) fits better in-sample
     (group SSE 23.8 vs 25.0) but not in CV (28.2 vs 27.8).
   - **FU-A** ≈ FU once the bounds are harmonized.
   - **FU-P** is worst: 0.4.0's cap-at-β made it worse, and its λ is pinned at the
     upper bound. This was checked: round-1 parameters score worse under 0.4.0, so
     it isn't an optimizer failure.
4. **New entrants place well:** RL is 4th, and MINERVA 2 is 5th with a *single* free
   parameter.
5. **The error-driven models collapse onto co-occurrence counting.** R-W and KF have
   their best fits at parameter bounds equivalent to counting; their group objective
   (.5005) is within .001 of the baseline (.5011), and their CV SSEs match the
   baseline to the second decimal. This explains round 1's R-W ≈ baseline (r ≈ 1)
   and is a substantive result.
6. **Hypothesis testing ≈ baseline at best.** Under the fitted (participant-weighted)
   metric, PbV ties the baseline (CV .503 vs .501) and GnT is worse (.536). PbV only
   looked better under the unweighted item metric. Pursuit, Strength and FU-P are at the
   bottom.
   - **Winner's curse for stochastic models:** DEoptim's reported group optimum is
     biased low by .007–.033, because it's the minimum over thousands of noisy
     evaluations. Table 1 uses a fresh simulation at the best parameters instead.
     `fits/v04/summarize.R` / `summary.md`'s `group_sse` still use the biased optimum.
7. **Generalization to the holdouts** (`benitez2020` children and adults,
   `kachergis_initial_accuracy`):
   - **Adults:** the counting cluster (baseline, R-W, KF) and PA are best (MSE .028).
   - **Children:** BD is best (MSE .022).
   - **No model tracks children's item-level pattern:** every child r is near 0. That
     belongs in the abstract and Discussion.
8. **Fitted parameters on a bound:**
   - PA's β = 20000, the known issue (needs one sentence).
   - Unc's χ = 5, at its upper bound.
   - TF's α₀ = 1, which switches off its uncertainty-dependent efficiency.
   - KF's σ²_obs is on a plateau (already documented).
   - TF also accounts for nearly all of the ~500 logged objective errors: numerical
     failures at extreme parameter values.

### R3.2 Decisions needed

- **D1 — primary fit metric.** Recommend the fitted objective (participant-weighted
  condition SSE) as primary, since that's what was optimized, with item-level r
  alongside and summed item SSE in the supplement. Say explicitly that PA and FUs
  swap places between the two metrics.
- **D2 — can the top models be told apart?** Recommend a paired comparison over CV
  folds, or a bootstrap over conditions, for the top ~5. That would turn "PA, FUs,
  BD" into a defensible "statistically indistinguishable top tier" (or not).
  Analysis only, no refitting.
- **D3 — `uncfam_general` (PR #27).** The per-condition fit is running now
  (`fits/general_percond/`). Include it as a new section ("which BAM extensions does
  each design prefer?"), put it in the supplement, or save it for a follow-up paper?
  Decide once the results are in.
- **D4 — generalization set.** Recommend the holdout registry (`benitez2020`,
  `kachergis_initial_accuracy`; `vlach_debrock2017` as a mean-only check) **plus**
  Medina 2011 and Smith & Yu 2008 rerun with round-2 parameters. Medina and Smith &
  Yu are currently still round-1 numbers.
- **D5 — the design-search work** (`fits/discriminative_design_search*`). Leave it
  out, mention it as future work, or make it a short supplementary section? It's
  round-1-based and its best design extrapolates far beyond the data (see its
  report).
- **D6 — target journal** (template; `cogsci2016` isn't installed) and the citation
  for the CogSci 2024 conference version.

### R3.3 Work items (in order)

1. ✅ **Housekeeping** (2026-10-07: results committed in `71b7243`; quarantine removed).
   Original item text:
   - Commit the round-2 outputs and scripts: `fits/v04/{group_fits,cv_fits,generalization}.Rdata`,
     `generalize.R`, `summarize.R`, `summary.{md,rds}`, and the `force(data)` fix in
     `refit.R`.
   - Delete `fits/v04/failed_2026-10-03/`.
   - Decide what happens to `fits/generalization_test.R` (superseded by
     `generalize.R`).
2. ✅ **Table 1** (2026-10-07; participant-weighted objective per D1, computed text) — rebuild for 20 rows: model, class, k, CV (mean and fold range),
   group, r, using the D1 metric. Drop the hard-coded abbreviation/ordering code.
3. **Table 2** — the model-by-model prediction correlation table at 20×20 is too big
   for a page. Replace it with a clustered heatmap figure, or a table restricted to
   the mechanism representatives; full matrix in the supplement.
4. **Figure 1** — 20 panels; the facet order and labels are hard-coded for the
   original 12, so the new models currently fall out of the figure.
5. **"What changed" supplement** — the R3.1 table, plus the correction notes already
   in Method. This also serves as material for the response letter.
6. **Generalization** (D4) — rewrite the section around `fits/v04/generalization.Rdata`
   plus a round-2 Medina / Smith & Yu run, and lead with the children result.
7. **Corpus section** — the paper's copies in `fits/corpus/` predate the fixed 0.4.0
   rerun (md5s differ from `v0.4.0:tests/bakeoff_comparison/`). Re-copy, re-run
   `fits/corpus_results.R`, and add the new models.
8. **Discussion** — rewrite around R3.1's headlines:
   - the stable top tier;
   - error-driven learning reducing to counting;
   - FU extensions not generalizing;
   - hypothesis testing ≈ baseline;
   - a 1-parameter episodic model in the top five;
   - children unexplained.

   Drop the round-1 numbers still inline (PbV–GnT r = .65, etc.).
9. **Abstract and Intro** — 19 models, 56 conditions, 2,359 participants, holdouts,
   corpora.
10. **Appendix** — regenerate everything with round-2 fits:
    - the disagreement index with its 12 mechanism representatives;
    - the best/worst tables;
    - re-check the hand-written prose about specific conditions.
11. **Final pass** — journal template (D6); render; check every inline number against
    `fits/v04/summary.md`.

---

## 0. Round 2 (2026-10-02): what XSLmodels 0.4.0 changes

Everything below §0 describes round 1, which ended with all fits complete (Sep 15)
against the then-installed **0.3.0**. The package is now **0.4.0**, and the paper needs
a second pass "using the package" as it now stands. All round-2 results use one
build: XSLmodels **`d0e2230`** = `release-0.4.0-prep` (`2a7d291`) + the `sample()` fix
below (PR #25), installed privately (`fits/v04/Rlib`, recorded in
`fits/v04/XSLMODELS_COMMIT`). Note the *system* XSLmodels install reports "0.3.0" but
is an Oct 2 build that already has the 0.4.0 models and the #14 change -- it was built
before the version bump.

**Every lab fit must be redone**, for two independent reasons:

1. **Fitting set grew 53 → 56 conditions** — 3 more Kachergis, Yu & Shiffrin (2009)
   temporal-contiguity conditions (`1olap3tr_contr`, `temp_cont_1olap2tr_contr`,
   `temp_cont_2olap2tr_contr`). Now 2,359 participants, 872 items.
2. **Package behavior changed for the uncfam family.** `fits/check_package_drift.R`
   reruns each model at its *saved* parameters under 0.4.0 (no fitting):

   | model(s) | at saved params under 0.4.0 | cause |
   |---|---|---|
   | FU, Nov, Unc, Str, FU-A, FU-P | **predictions change** (item max \|Δ\| 0.13–0.52; SSE ±0.7) | `update_known()` indexing fix (XSLmodels `6aaebcb`); FU-P also the cap-at-β fix |
   | BD, R-W, KF, PA, PA-θ, baseline | bit-identical | — |
   | PbV, GnT, Pur, FUs, RL | within Monte Carlo noise | — |

   **The original research code (`models/kachergis.R` etc.) has the identical
   `update_known()` bug**, so the *published* CogSci numbers for FU/Nov/Unc/Str carry
   it. The journal version should say so (one sentence in Method + a footnote).

**Other 0.4.0 changes that matter for the paper:**

- **5 new models:** `uncfam_gamma` (free familiarity exponent), `uncfam_elimination`
  (learning by elimination / ME), and three distributed/episodic memory models —
  `minerva2`, `todam`, `rem`. Cost per objective evaluation on 56 conditions: the two
  uncfam variants ~0.5 s (cheap); `minerva2` ~0.13 s/sim (≈ FUs); **`todam` ~2.3 s/sim
  and `rem` ~5 s/sim — 20–40× FUs**, i.e. weeks for a full CV at the paper's
  settings. `tests/bakeoff_comparison/memory_models_report.md` has reduced-budget
  group fits on 53 conditions (minerva2 SSE .420 — competitive; todam .876; rem .537,
  under-converged).
- **Holdout registry** (`xsl_holdout_datasets`): two *scorable* held-out datasets with
  per-item human accuracy — `benitez2020` (7 conditions, **children and adults**) and
  `kachergis_initial_accuracy` (2) — plus `vlach_debrock2017` (children, overall mean
  only). These make a much stronger generalization section than Medina + Smith & Yu
  alone; `fits/generalization_test.R` (untracked) is a prototype.
- **Bounds inconsistency inside the uncfam family (pre-existing, found now):** FU was
  fit with the original paper's bounds (entropy weight `B ∈ [0.1, 15]`, landed at
  **B ≈ 13.5**), but FU-A and FU-P were fit with the registry's `B ∈ [0.8, 1.0]`,
  which excludes FU's own optimum. Their worse fits may partly be a bounds artifact.
  The new `uncfam_gamma`/`uncfam_elimination` registry bounds inherit the same narrow
  `B`. Refit should give every uncfam variant the same `X`/`B`/`C` bounds.
- Unaffected: the #14 positional-label fix (no `xsl_datasets` condition has gapped
  labels); the corpus section (being re-run separately in
  `XSLmodels/tests/bakeoff_comparison/`).

**Decisions (2026-10-02):** add `uncfam_gamma` (FU-G), `uncfam_elimination` (FU-E),
`minerva2` (M2), and `tilles` (TF) → **19 models + baseline**; `todam`/`rem` out
(future-work mention only); every uncfam variant gets FU's `X`/`B`/`C` bounds;
refit queued behind the corpus jobs that were running.

**Package bug found and fixed (XSLmodels PR #25, merged to `master` as `08fac1f`; local branch `release-0.4.0-prep-sample-fix` = `d0e2230` is the release branch + fix -- `release-0.4.0-prep` itself still needs master merged in):** `propose_but_verify()` (line 83) and
`pursuit()` (line 91) call `sample(tr_o, ...)`; when a trial has exactly one object
`k`, R samples from `1:k` instead, i.e. usually a referent not on the trial. No
`xsl_datasets` condition has single-object trials, so **the lab refit is unaffected**,
but `kachergis_initial_accuracy` (18 single-object familiarization trials per
condition) and the naturalistic corpora (many single-object utterances) are — so
PbV/Pursuit's holdout and corpus numbers were wrong. Fixed with
`tr_o[sample.int(length(tr_o), n, replace = TRUE)]` (as `guess_and_test()` already does).
Verified on the pre-#14 commit: corpora (string labels) were fine before #14, so the
round-1 corpus results in the paper are unaffected; integer-labelled data was already
affected. The Oct 2 night corpus runs (built with #14, without the fix) were affected
-- ~40% of Rollins and ~18% of FM utterances have one object -- and were discarded and
rerun (2026-10-03) on the fixed build.

### Round-2 status

| item | status |
|---|---|
| Drift check, saved fits vs. 0.4.0 (`fits/check_package_drift.R`) | ✅ uncfam family changed; others identical / within noise |
| Pinned package (`fits/v04/install_pinned_xslmodels.sh`, `XSLMODELS_COMMIT` = `d0e2230`, i.e. 0.4.0 + PR #25) | ✅ installed to `fits/v04/Rlib` (re-pinned from `2a7d291` before the refit started; lab results identical either way) |
| Refit pipeline (`fits/v04/{specs,refit}.R`, `queue_refit.sh`) | ❌ **first run failed** (2026-10-03 06:08): every parallel-worker evaluation errored, so all fits came back `bestval = Inf` (cause not reproduced; outputs quarantined in `fits/v04/failed_2026-10-03/`). `refit.R` now logs worker errors, runs a worker pre-flight, refuses non-finite fits, and QUICK uses the real parallel path -- **needs a smoke test, then relaunch** → `fits/v04/{group_fits,cv_fits}.Rdata`; progress in `fits/v04/refit_*.log` |
| Methods: 4 new model subsections, implementation-notes paragraph, PbV/FU-P text fixes | ✅ |
| Data: 56 conditions, provenance, computed ranges | ✅ |
| Shared fit loader (`paper/load_fits.R`) + PRELIMINARY banner while refit runs | ✅ paper + appendix |
| Generalization: rewrite around `xsl_holdout_datasets` (`benitez2020` kids+adults, `kachergis_initial_accuracy`, `vlach_debrock2017` mean) + Medina/Smith & Yu; fold in `fits/generalization_test.R` prototype | ❌ next — needs group fits, and the `sample()` fix for PbV/Pur on `kachergis_initial_accuracy` |
| Round-1 → round-2 "numbers that changed" table (0.3.0/53 cond → 0.4.0/56 cond, and vs. published) | ❌ after refit |
| Results/Discussion prose, Tables 1–3, figures, appendix numbers | ❌ after refit |
| `sample()` bug fix (XSLmodels PR #25) | ✅ merged (`08fac1f`) |
| Corpus re-run on the fixed build (`XSLmodels/tests/bakeoff_comparison/corpus_gold_{comparison,fits}.R`, `R_LIBS=../XSLmodels-fix-sample/.Rlib-corpus`) | ✅ done 2026-10-03 (comparison 02:14, fits 06:08); outputs in `XSLmodels/tests/bakeoff_comparison/` (uncommitted there); logs `../XSLmodels-fix-sample/corpus_rerun_*.log` |
| Corpus section: pick up the re-run (+ new models) | ❌ after re-run |
| Abstract/Intro: 19 models, 56 conditions, 2,359 participants, memory-model framing | ❌ |
| Template: replace `cogsci2016::cogsci_paper` (not installed here) with the target journal's | ❌ needs journal choice |
| Cite the CogSci 2024 conference version (ToDo in Method) | ❌ needs bib entry |

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

### Supplementary material (was the appendix) — ✅ mostly done (2026-09-13)
- ✅ Repointed `cv_group_fits.Rdata`/`combined_data.RData` loads at package outputs /
  `xsl_datasets`; generalized the hard-coded 11-model blocks (`xm_mat[,2:12]`, etc.).
- ✅ Added "Which experimental designs are easy or hard, overall?" (mean SSE across all
  models per condition) — the piece George asked about that wasn't there before.
- ✅ Enabled "Best/Worst-Fitting Models per Experiment" (was `include=F`); fixed a
  `left_join`→`full_join` bug that silently dropped models with zero worst-fits.
- ✅ Fixed a real bug the repoint surfaced: the 4 Koehne + 3 Suanda conditions have only
  condition-level human accuracy (flat across items), so correlation is undefined for
  every model on them — was producing a bogus "NA-model" row in the best/worst tables.
  Now excluded from every correlation-based view (kept in SSE-based ones).
- Still to re-check once `cv_all` lands: the `filt`-condition / `freq369-3x3loCD` misfit
  prose (written against the old 44-cond/11-model heatmap) and the specific
  best/worst-condition names once FUs is in the CV set.
- Not yet done: full parameter tables for every model × fold; corpus gold-lexicon detail
  tables (the Corpus Results section in the main text covers the summary numbers).

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
| **CV, 53 cond, 11 orig models** | `cv_all_experiments.R` | 🔄 **10/11 — `kachergis_sampling` (FUs) still fitting** (same PID 39640, running continuously since ~Sep 3 — over a week as of 2026-09-13). This is far slower than the equivalent softmax_rl CV run (~5 days) and worth checking on directly rather than assuming it will finish; see note below. |
| **CV, 53 cond, 4 new models** | `cv_new_models.R` | ✅ **done** — all 4 incl. `softmax_rl` (finished 2026-09-06) → `cv_new_models.Rdata` |
| **Package generalization run** (Medina, Smith & Yu; Suanda as now-in-sample) | `generalization_with_package.R` | ✅ **done** → `generalization_with_package.RData`. Koehne 2013 deliberately **dropped** from this comparison (2026-09-13, author feedback) — it's in the fit set like Suanda, but is no longer reported as a "formerly held out" result at all, just folded into Table 1 with everything else. New finding: baseline / R-W / Kalman / RL generalize best to the two genuinely held-out experiments. |
| **Corpus results** | `corpus_results.R` (wraps `XSLmodels/tests/bakeoff_comparison/corpus_gold_{comparison,fits}.R`'s saved `.rds`, copied into `fits/corpus/`) | ✅ **done** → `corpus_results.RData` + Table 4 in the main text. Headline: Bayesian decay (mediocre in the lab) is the best corpus model on both corpora; FGT close behind with RSA a no-op; PA's lab rank holds on corpora, FUs's doesn't (Spearman ρ = 0.59 between lab and corpus rank). |
| Repoint paper + appendix at `xsl_datasets` / package fit files | — | ✅ done (Action 0 + appendix pass) |
| Re-knit, diff every inline `\r` stat vs. `cogsci-final.pdf` | — | ❌ final pass, blocked on `cv_all` |
| Handle `fazly` beta (§3.1); adopt/annotate BD rule (§3.2) | `model_specs.R` | ❌ small |
| Parametrize appendix's hard-coded 11-model blocks | `appendix.Rmd` | ✅ done |

**Runtime note:** `cv_all_experiments.R`'s FUs fold has now run continuously for over a
week — worth checking it's actually progressing (e.g. is DEoptim on a later fold, or
hung?) rather than just waiting for it. Everything else needed for the fits is done.

---

## 7. Suggested sequence

1. ~~**Action 0**~~ — done. Consolidated to one `journal-paper.Rmd`, dropped Liang.
2. **CV fits** — `cv_new_models.R` done; `cv_all_experiments.R` still finishing FUs
   (running over a week now — check it hasn't hung rather than just waiting further).
3. **Number-independent writing — done:**
   - ✅ Expanded Intro (taxonomy + corpus motivation + package).
   - ✅ Wrote the 4 new-model Method subsections.
   - ✅ Updated Data section counts/provenance; wrote the Corpora subsection.
   - ❌ Still open: switch the template to the target journal's; strip the CogSci
     `\vspace` hacks; author response / cover-letter notes.
4. ~~**Generalization script**~~ — done (`generalization_with_package.R`), and revised
   2026-09-13 to drop Koehne 2013 entirely (was double-counted as both "in fit set" and
   "generalization"; now just an in-sample condition, not reported here at all).
5. ~~**Corpus results**~~ — done: `corpus_results.R` wrapper + Table 4 + Results
   subsection, built from the package's already-run `corpus_gold_{comparison,fits}.R`
   output (copied into `fits/corpus/`).
6. ~~**Appendix**~~ — done: repointed at package/53-cond data, added the "overall
   easy/hard conditions" table, enabled the best/worst-models-per-condition table,
   fixed the Koehne/Suanda zero-variance NA bug.
7. **When `cv_all` lands** — regenerate Tables 1–3, Figure 1, the appendix heatmaps/
   best-worst tables (currently 14/15 models); walk the §5 table and every inline stat.
8. **Rewrite Discussion** against final numbers (esp. the PbV story; add corpus results).
9. **Final knit + stat-by-stat diff** vs. `cogsci-final.pdf`; typeset pass for the venue.
