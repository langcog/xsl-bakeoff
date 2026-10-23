# Design: a nested comparison of the biased associative model with `uncfam_general()`

_Sketch, 2026-10-14. Nothing here has been run yet; it's a plan for doing it properly._

## The question

The paper fits six members of the familiarity/uncertainty (FU) family as separate
models: FU, Str, Unc, Nov, FU-A, FU-G and FU-E, plus FU-P, which is not nested. Each is
then ranked on its own. This doesn't answer the question the family was built to
answer: **which of the mechanisms does a learner need in order to predict new
conditions?** `uncfam_general()` (XSLmodels PR #27, 990ef49) nests the whole family in
one function, so the variants become cells of a factorial model space. We can then
estimate what each mechanism adds, averaging over the others. This follows the factorial
model comparison of van den Berg, Awh & Ma (2014, *Psychological Review*).

## What the existing per-condition run can and can't tell us

`fits/general_percond/` fit 10 variants separately to each condition, by binomial
likelihood, with AIC/BIC and LR tests. The short-term trace run built on it.

- **Stale data.** It ran on 56 conditions. These include the old filtering scoring (AtoA
  only), the mis-assigned freqCD `n_subj`, the duplicates, and Suanda/Koehne (which have
  flat item accuracies). It has to be rerun on the 47.
- **Underdetermined per-condition fits.** Three or more free parameters per condition,
  on about 18 items, can't separate B from γ. "Strength" (B = 0) wins in 22 of 56
  conditions by AIC, which more likely says *B isn't identifiable in one condition* than
  *those designs need no uncertainty bias*. The question the paper asks is a group one:
  which mechanisms predict held-out designs.
- **Overstated evidence.** The binomial likelihood treats every item response as
  independent. A single item's count across participants *is* binomial, but items
  within a condition share participants, so summed LRs are anti-conservative (the
  script's header says so). Rankings survive; p-values and AIC weights don't.
- **A different objective from the paper.** Binomial likelihood here, n-weighted SSE in
  Table 1. Pick one primary objective for the nested comparison and use the same one as
  Table 1.

The per-condition analysis is still worth keeping as a secondary "which designs pull
toward which mechanism" view (step 7). It shouldn't be the main test.

## Model space

All cells are deterministic `uncfam_general(..., K = Inf)`. X ∈ [.001, 5] and
C ∈ [.5, 1] are always free.

| factor | levels | parameter |
|---|---|---|
| **F** familiarity | off (γ = 0), linear (γ = 1), free | γ ∈ [0, 6] |
| **U** uncertainty | off (B = 0), entropy, novelty | B ∈ [0, 15]; `uncertainty = "entropy"/"novelty"` |
| **E** elimination | off (ε = 0), free | ε ∈ [0, 20] |
| **A** trial-level attention | off (κ = 0), free | κ ∈ [0, 3] |

That gives 3 × 3 × 2 × 2 = **36 cells**, with 2–6 free parameters.

- When U is off, entropy and novelty coincide, which leaves 3 × 2 × 2 × 2 + 3 × 2 × 2 = **28 distinct cells**.
- **F off and U off** gives equal attention to every co-present pair: a decaying
  co-occurrence counter. Keep it as the family's internal null. It should land near the
  baseline, which checks the whole setup.
- Existing models are cells, per `tests/testthat/test-uncfam_general.R`. Note that the
  paper's description of Nov at :225 is wrong: `uncfam(variant = "novelty")` *keeps* the
  familiarity term (`model-uncfam.R:53`).

  | model | γ | U | ε | κ |
  |---|---|---|---|---|
  | FU | 1 | entropy | 0 | 0 |
  | Str | 1 | off | 0 | 0 |
  | Unc | 0 | entropy | 0 | 0 |
  | Nov | 1 | novelty | 0 | 0 |
  | FU-G | free | entropy | 0 | 0 |
  | FU-E | 1 | entropy | free | 0 |
  | FU-A | 1 | entropy | 0 | 1 (fixed) |

  A free κ generalizes FU-A. Keep κ = 1 as one extra cell, so that FU-A appears exactly.

**Stage 2**, only on the top 2–3 cells from stage 1:
- **Sampling** (K = 1, stochastic, n_sim = 200). FUs is a top model in Table 1, so
  sampling might interact with the winning mechanisms.
- **Short-term trace** (ρ ∈ [0, 20], Cs ∈ [0, .95]). The per-condition trace run
  suggested that γ > 1 may stand in for recency.

FU-P (prediction error) isn't nested and stays a separate row in Table 1.

## Fitting

1. **Objective and folds are identical to the main refit.** That means the n_subj-weighted
   SSE (`xsl_run()$sse`), and 5-fold CV with `set.seed(123); caret::createFolds(labels, k = 5)`
   on the 47 v05 labels. That makes every cell directly comparable with the Table 1 rows,
   and with FU, Str, Unc, Nov, FU-G, FU-E and FU-A as fitted in v05.
2. **The same bounds in every cell.** The registry's `uncfam_general` bounds
   (`B ∈ [.8, 1]`, `X ∈ [.01, .5]`) exclude FU's own optimum (B ≈ 14 in v05). That is the
   same bounds bug fixed for the variants in v04. Use the table above, and fix the
   registry in XSLmodels too.
3. **Nested seeding.** Seed each cell's initial DE population with the optima of the
   cells nested in it, mapped into its parameter space, as `fit_general_percond.R` does.
   A superset then never trains worse than its subsets. **Assert** this per fold, and
   refit any cell that violates it.
4. **DEoptim:** NP = max(50, 10 × #params), itermax = 100, reltol = .001, keeping the
   best of 2 runs. These are cheap deterministic fits.
5. **Pin the package.** The v05 Rlib (2999e8b) predates `uncfam_general`. Pin 990ef49 or
   the 0.5.0 release. `data/` is unchanged between 2999e8b and 990ef49 (checked), so
   v05's fits stay comparable. Before fitting anything, check that the seven existing
   cells at their v05 group-fit parameters reproduce v05's SSEs exactly.

## Analysis

1. **Cell ranking:** CV SSE per cell, with the noise floor (.117, `noise_ceiling.md`) and
   the baseline for scale.
2. **Factor effects:**
   - **Knock-in:** for each factor level, the mean change in CV SSE against its "off"
     level, averaged over every combination of the other factors.
   - **Knock-out:** the CV SSE increase from removing each factor from the best cell.
   - Plot both as van den Berg et al.'s factor-level figure.
3. **Uncertainty:** a paired bootstrap over the 47 conditions, resampling conditions'
   out-of-fold SSEs with the same draw for every cell. This gives:
   - each cell's probability of being best;
   - each factor level's probability of being in the best cell (an inclusion frequency);
   - CIs on the knock-in and knock-out effects.

   This is the same machinery R4.4 item 19 needs for the top tier of Table 1, so build it once.
4. **A second objective:** score each cell's out-of-fold predictions at the choice level
   (`choice_level.R`'s machinery: accuracy part and foil part). Does elimination, for
   instance, help predict *which* wrong object people choose, even if not accuracy?
5. **Robustness:** leave-one-experiment-out CV (folds = source papers) for the top cells.
6. **Identifiability inside the family:**
   - profile the CV SSE over γ (and B) for the best cell;
   - run parameter recovery for the winning cell (`recovery.R`, adding it to
     `PARAM_MODELS`);
   - report the γ–B trade-off (the fold-wise parameter correlation).
7. **Per-condition view (secondary):** rerun `general_percond` on the 47 with the same
   cells and bounds.
   - Report only rank-based summaries: which cell each condition prefers by
     out-of-sample criteria, where possible.
   - Or replace the LR tests with a participant-cluster bootstrap. The raw responses are
     in XSLmodels `data-raw/raw_responses.R` and map onto the v05 conditions via
     `extract_response_matrices.R`.
   - Then relate preferences to design features (`design_features.R`), with conditions
     as the units.

## Compute

Stage 1 is 36 cells × (1 group + 5 CV) = **216 deterministic fits**. In v05, FU-family
group fits took 4–8 minutes on 6 workers. Expect about 5–10 minutes with 2 runs and up to
6 parameters, so about **20–35 h on 6 workers** (about 12–18 h on 12).

Stage 2:
- sampling: 3 cells × 6 fits × 1–3 h ≈ 1–2 days on 6 workers (or the best cell only);
- trace: 3 × 6 small deterministic fits.

**Order:** after the v05 CV finishes, alongside or after model recovery.

## In the paper

A short section, "Which mechanisms does the biased associative model need?":
- a factor-effects figure;
- one paragraph on the stage-2 results;
- the per-condition view in the supplement.

The seven FU-family rows in Table 1 could then collapse to FU, FUs and FU-P, with a
pointer to this section. Alternatively keep them, since they are the published models.
That one is your call.
