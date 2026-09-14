# Can we mutate an experimental design to be more discriminative?

Prototype for `fits/discriminative_design_search.R`. First run 2026-09-13;
revised same day to fix a Monte-Carlo-noise problem in the search objective
(see "Revision" below) -- **the Results section reflects the revised,
noise-free run.** Extends
`appendix.Rmd`'s "Model disagreement" index (SD of `ModelPerf` across a
one-per-mechanism panel of models, at their current group-fit parameters)
from *measuring* which existing conditions discriminate best between
theories to *searching* for a better one.

## Method

- **Representation.** Every condition in `xsl_datasets` has words/trial ==
  objects/trial (a trial's ambiguity *is* its co-occurrence structure: the
  learner hears `k` words and sees `k` objects, one of which is each word's
  true referent, but not which). A design therefore reduces to: `T` trials,
  each a `k`-subset of `{1..V}` word/object indices. This is a much smaller
  space than free-form trial construction, and it's exactly the space every
  real condition already lives in.
- **Objective.** Mean, over words, of the SD of `ModelPerf` across a
  10-model panel (one representative per distinct mechanism: FU, PA, BD,
  R-W, PbV, GnT, Pursuit, RL, Kalman, baseline) -- same panel and metric as
  the appendix's "Model disagreement" table. Every model is run **once**,
  at its already-fit group-fit parameters; the search never touches model
  parameters, only the design.
- **Search.** Simulated annealing: repeatedly pick a random trial, swap one
  of its members for a value not already in that trial (rejecting any swap
  that would drop an index out of the design entirely), accept improving
  moves always and worsening moves with probability `exp(delta/temperature)`
  (temperature decays geometrically over the run, from 0.02 to 0.0003, so
  early iterations can cross small dips to escape local optima). 15,000
  iterations per seed. The search-time objective uses only the **6
  deterministic models** in the panel (FU, PA, BD, R-W, Kalman, baseline) --
  no Monte Carlo noise at all -- and the four stochastic models (PbV, GnT,
  Pursuit, RL) are reintroduced only to **re-score the winning design at
  `n_sim = 200`** for the number actually reported. See "Revision" below for
  why (the first version searched on the full noisy 10-model panel).
- **Two seeds**, both real 12-word/12-object, 2-words/trial conditions:
  `filt9E_9L` (currently the *most* discriminative condition in the whole
  53-condition set) and `filt0E_9L` (currently one of the *least*
  discriminative, same size) -- can we beat the best natural design, and
  can we rescue a bad one, using the same amount of data collection (same
  `V`, `T`, `k`)?

## Revision (2026-09-13): the first run's noise problem, and the fix

The first version searched on the full 10-model panel at `n_sim = 20` for
the four stochastic models. Result: `filt0E_9L` improved hugely and
robustly (0.040 -> 0.142 at re-verified `n_sim = 200`), but `filt9E_9L`'s
apparent +20% search-time gain evaporated on re-verification (0.1595 ->
0.1575, i.e. no real improvement) -- the hill-climber was partly just
exploiting Monte Carlo noise in the small-`n_sim` stochastic-model
estimates, and a plain accept-only-if-strictly-better rule has no way to
tell a real gain from a lucky noisy draw. The fix: search-time scoring now
uses only the 6 deterministic models (zero noise, so every accepted
improvement is real), with the 4 stochastic models brought back only to
verify the final winner. Because a deterministic-only evaluation is ~25x
cheaper, the iteration budget also went up (1,200 -> 15,000) and the search
itself was upgraded from plain hill-climbing to simulated annealing (see
Method), since a cheap, noise-free objective makes a more thorough search
affordable.

## Results (revised, noise-free run)

| seed | seed score (full panel, n_sim=200) | best-found (full panel, n_sim=200) | trials changed |
|---|---|---|---|
| `filt9E_9L` (already best) | 0.1595 | **0.1804 (+13.1%)** | 80 / 81 |
| `filt0E_9L` (near-worst) | 0.0441 | **0.1861 (+322.5%)** | 53 / 54 |

Both seeds now show real, verified improvement -- including `filt9E_9L`,
the design that was itself hand-built to be maximally discriminative and
that the first (noisy) run couldn't improve on at all. And both seeds
converge toward similarly high scores (0.180 vs. 0.186) despite starting
nearly 4x apart and ending up almost entirely rewritten (80/81 and 53/54
trials changed) -- a reasonable cross-check that the search is finding a
real, shared high-discriminability region of this design space rather than
two unrelated, seed-dependent flukes.

**But the two winning designs differ sharply in plausibility, and this
matters for how much to trust the headline numbers.** Comparing per-word
frequency in each winning design:

| | `filt9E_9L` winner | `filt0E_9L` winner |
|---|---|---|
| per-word frequency (min / mean / max) | 12 / 13.5 / 15 | **1 / 9 / 27** |
| frequency SD | 1.0 | **10.3** |
| distinct co-occurrence partners per word (mean, out of 11 possible) | 9.5 | 3.7 |

The `filt9E_9L` winner is a well-formed design: every word appears a
similar number of times (12-15) and co-occurs densely with almost every
other word (9.5/11 partners on average) -- exactly the "everyone talks to
everyone, evenly" structure that makes `filt`-style conditions
discriminative in the first place, just pushed further. **The `filt0E_9L`
winner is not well-formed**: one word appears in only 1 of 54 trials (all
but unlearnable) while another appears in 27 (half the experiment), and
partners are much sparser overall. Nothing in the search currently
penalizes this -- the objective only rewards disagreement, and an extreme
frequency imbalance is an easy way to buy disagreement (a model that needs
many exposures and one that doesn't will predict very differently for a
word seen once) without it reflecting the kind of mutual-exclusivity-style
theoretical split the `filt9E_*` family actually probes. **The headline
+322.5% for `filt0E_9L` should therefore be read as an upper bound found by
an unconstrained search, not as a proposal for an actual experiment**; the
`filt9E_9L` result (+13.1%, frequency-balanced) is the more directly
trustworthy one of the two.

## Caveats / what a more rigorous version would need

- **Frequency-balance constraint.** The clear next fix, motivated directly
  by the `filt0E_9L` result above: penalize (or hard-constrain) per-word
  frequency variance in the objective, e.g. reject/penalize any candidate
  whose per-word frequency range exceeds the seed's own, or add a soft
  penalty term (disagreement minus a multiple of frequency SD).
- **Only one design "shape" explored per run.** `V`, `T`, and `k` are fixed
  at the seed's values; a fuller search would also vary those (more like
  the actual `filt`/`freq369` family's own parameterization), or use
  `DEoptim` over that smaller, continuous knob space instead of/alongside
  the discrete trial-level search here.
- **This is a proof of concept, not a validated experiment design.**
  Before proposing either winning design as an actual study, it should be
  re-examined for face validity beyond frequency balance (does the
  co-occurrence pattern correspond to something a real training procedure
  could produce and that participants could plausibly be run through?) and
  ideally cross-checked against a formal mutual-information-based
  criterion (Cavagnaro, Myung, & Pitt's adaptive design optimization is the
  standard reference) rather than just this SD-based heuristic.
- **`filt9E_9L`'s winner barely resembles its seed** (80/81 trials
  changed) -- simulated annealing with 15,000 iterations essentially
  re-solves the design from scratch rather than lightly perturbing the
  original. That's fine for "what's the best design of this shape" but
  means this is no longer really a *mutation* of `filt9E_9L` in any
  meaningful sense; if the goal is specifically small, targeted edits to an
  existing design, the acceptance criterion or iteration budget would need
  to change (e.g. cap the number of trials allowed to differ from the seed).

## v2 (2026-09-14): generalizing the design space

Per author feedback, v1's fixed "shape" (vocabulary size `V`, trial count
`T`, and a single words/trial = objects/trial `k`, all pinned at the seed's
own values) was too narrow. `fits/discriminative_design_search_v2.R`
generalizes the genome to make `V`, `T`, and per-trial words/objects counts
all mutable, adds a pure trial-reorder mutation, and deliberately does
**not** constrain per-word frequency -- word/referent frequency is
plausibly Zipf-distributed in the real world, so skew is treated as
something to measure, not a defect to penalize away. See the script's
header comment for the full dimension table, bounds, and rationale
(grounded in the real `xsl_datasets` range plus the author's stated
practical ceilings: `V` <= 40, words or objects per trial <= 10).
Deliberately deferred: true many-to-many ground truth (synonymy/homonymy --
the package's scoring is a strict 1-to-1 label match, so this needs new
scoring code, not just a richer generator) and category-learning
manipulations, per instruction.

Two seeds, 12,000 annealing iterations each: `filt9E_9L` (the v1 winner)
and a "blank" neutral design at the dataset's median shape (`V=18, T=45,
k=2`).

**A real bug surfaced during development**, worth recording: several
models index their internal word x object matrix by numeric *position*,
not by label. Once trials could have different word-sets and object-sets,
it became possible for a pair to appear only as an object and never as a
word (or vice versa) -- this crashed several models with a subscript-out-
of-bounds error. Fix: the validity check now requires the word-label-set
*and* the object-label-set to each independently cover `{1..V}`, not just
their union.

### Results

| seed | seed score | best-found score | final shape (V, T) | trials changed |
|---|---|---|---|---|
| `filt9E_9L` | 0.1595 | **0.2894 (+81.5%)** | V=6, T=130 | (full redesign) |
| blank median | 0.1804 | **0.2513 (+39.3%)** | V=6, T=130 | (full redesign) |

**Both independent searches converged on the same design shape: V=6 (the
floor) and T=130 (the ceiling) -- both search bounds, hit from opposite
starting points.** That's a real, meaningful signal (two searches landing
in the same corner of a much larger space isn't a coincidence), but it also
means **the true unconstrained optimum lies beyond the bounds explored
here** -- the search wanted to go further in both directions and simply
couldn't. Reading the disagreement objective's shape: fewer words with far
more trials means each word-object pair gets repeated far more than in any
real condition -- the winning designs' repetitions-per-pair are **~53-62**,
compared to **3-13.5 across every real condition in `xsl_datasets`**, a
4-5x extrapolation beyond anything these models' parameters were ever fit
on. Per-trial words/objects also range all the way from 1 to the full
vocabulary (6) within a single design -- some trials show one word, others
show all six words and objects together.

**This is an important caveat, not just a footnote.** The whole premise of
this exercise is "how discriminative would an experiment be, given what we
currently know about these models' best-fit behavior" -- but a design this
far outside the training data's regime asks the models to extrapolate into
territory their parameters were never informed by. The large disagreement
found there may partly reflect genuine, interpretable divergence in
overtraining/asymptotic behavior (some models saturate near ceiling fast;
others -- especially anything with persistent decay, forgetting, or a noise
floor -- do not), or it may partly reflect arbitrary, underdetermined
behavior in a regime the fitting procedure never constrained. Both are
scientifically interesting, but only the former would replicate in a real
experiment; distinguishing them needs either (a) tighter bounds (e.g. cap
repetitions-per-pair near the observed historical range, ~3-14, so the
search stays within what these particular fits can speak to), or (b)
inspecting *why* each model diverges at V=6/T=130 specifically, model by
model, rather than trusting the aggregate SD.

Two secondary diagnostics, both real findings independent of the
extrapolation concern above:

- **Order sensitivity is real.** Re-scoring each winning design's exact
  same trial *set* in 5 random orders scores meaningfully lower than the
  optimized order (`filt9E_9L`-seeded: 0.288 optimized vs. 0.249 mean
  shuffled; blank-seeded: 0.254 vs. 0.180) -- these models are not
  order-invariant, and the search is exploiting that (consistent with the
  recency/decay mechanisms several of them have built in).
- **Frequency skew emerged, but only mildly.** Log-frequency vs. log-rank
  slopes were -0.13 and -0.22 (0 = uniform; real Zipf's law for word
  frequency is closer to -1) -- some skew, well short of dramatic. Word
  frequency inequality alone wasn't the main lever the search pulled;
  vocabulary size and repetition count were.

### Suggested next step

Re-run with `V_MIN` raised and/or `T_MAX` lowered so repetitions-per-pair
stays within (or just outside) the observed historical range, to get a
result that's trustworthy as an actual proposal rather than an
unconstrained upper bound on the metric. Flagging for a decision rather
than just doing it, since "how far outside the historical range is worth
exploring anyway" is a judgment call about what counts as a fair test of
these models, not a bug to fix.

## Reproducing

```r
Rscript fits/discriminative_design_search.R      # v1 (fixed shape), ~4 min, both seeds
Rscript fits/discriminative_design_search_v2.R   # v2 (variable V/T/k), ~4.5 min, both seeds
```

Common model-panel/objective setup was factored out to
`fits/design_search_common.R`, sourced by both. Output:
`fits/discriminative_design_search.RData` (v1) /
`_v2.RData` (v2), both holding a `results` list keyed by seed label. v2's
entries additionally carry `shape_trace` (V/T over the run) and
`order_sensitivity` (optimized-order vs. shuffled scores).
