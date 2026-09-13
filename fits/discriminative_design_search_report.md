# Can we mutate an experimental design to be more discriminative?

Prototype for `fits/discriminative_design_search.R`, run 2026-09-13. Extends
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
- **Search.** Stochastic hill-climbing: repeatedly pick a random trial,
  swap one of its members for a value not already in that trial (rejecting
  any swap that would drop an index out of the design entirely), and keep
  the mutation iff the objective improves. 1200 iterations per seed,
  `n_sim = 20` for the four stochastic models during search (cheap), then
  the winning design is **re-scored at `n_sim = 200`** for an honest final
  number.
- **Two seeds**, both real 12-word/12-object, 2-words/trial conditions:
  `filt9E_9L` (currently the *most* discriminative condition in the whole
  53-condition set) and `filt0E_9L` (currently one of the *least*
  discriminative, same size) -- can we beat the best natural design, and
  can we rescue a bad one, using the same amount of data collection (same
  `V`, `T`, `k`)?

## Results

| seed | seed score (n_sim=200) | best-found, search estimate (n_sim=20) | best-found, re-verified (n_sim=200) | trials changed |
|---|---|---|---|---|
| `filt9E_9L` (already best) | 0.1595 | 0.1918 (+20%) | **0.1575 (‑1%)** | 6 / 81 |
| `filt0E_9L` (near-worst) | 0.0400 | 0.1709 (+327%) | **0.1424 (+256%)** | 21 / 54 |

**The `filt0E_9L` result is real and large, and it's interpretable.** The
seed's co-occurrence structure is a fixed 1-to-1 pairing: word 1 always
co-occurs with word 7 and nothing else, 2 with 8, 3 with 9, etc. -- every
word has exactly one, constant competitor, so cross-situational learning is
trivial and every model (any mechanism, any bias) converges to the same
unique answer. The search reshapes this into overlapping, many-to-many
co-occurrence (e.g. word 1 ends up co-occurring with 3, 4, 6, 7, 9, and 11
at varying frequencies) -- i.e., it rediscovered, purely by hill-climbing
on the disagreement objective and with no knowledge of the dataset's own
design logic, the same kind of dense mutual-exclusivity-violation structure
that makes the hand-designed `filt9E_*` family so discriminative. That's a
genuinely encouraging proof of concept: a bad design can be turned into
one nearly as good as the best design in the dataset (0.142 vs. 0.160),
with no change in trial count, vocabulary size, or words/objects per trial.

**The `filt9E_9L` result is a null result, and an instructive one.** The
apparent +20% gain during search evaporates on re-verification -- the
"improvement" was mostly the hill-climber exploiting Monte-Carlo noise in
the four stochastic models' `n_sim = 20` estimates, not a real gain. Two
things are going on: (1) `filt9E_9L` may simply already be close to a local
optimum in this design space, since it was itself hand-designed to maximize
exactly this kind of disagreement; (2) more importantly, **the search's
noise budget was too small to reliably detect further improvement from an
already-high baseline**, whereas the far larger, real gain available at
`filt0E_9L` was easy to find even through the noise.

## Caveats / what a more rigorous version would need

- **Noise-robust acceptance.** The simplest fix: use only the 6
  deterministic models for the search-time objective (no Monte Carlo noise
  at all), and bring in the 4 stochastic models only for final verification
  of the winning design. A more careful fix: require a candidate to beat
  the current best by more than its estimated noise margin (e.g. re-run
  both at the same `n_sim` a couple of times) before accepting.
- **Plausibility is not enforced beyond "every word appears at least
  once."** The winning `filt0E_9L` design still looks like a plausible lab
  design (same `V`/`T`/`k`, no word left unpaired for long), but nothing
  currently penalizes e.g. wildly uneven per-word frequency; worth adding
  an explicit frequency-balance constraint or penalty if this is taken
  further.
- **Only one design "shape" explored per run.** `V`, `T`, and `k` are fixed
  at the seed's values; a fuller search would also vary those (more like
  the actual `filt`/`freq369` family's own parameterization), or use
  `DEoptim` over that smaller, continuous knob space instead of/alongside
  the discrete trial-level search here.
- **This is a proof of concept, not a validated experiment design.**
  Before proposing `filt0E_9L`'s mutated design as an actual study, it
  should be re-examined for face validity (does the co-occurrence pattern
  correspond to something a real training procedure could produce?) and
  ideally cross-checked against a formal mutual-information-based
  criterion (Cavagnaro, Myung, & Pitt's adaptive design optimization is the
  standard reference) rather than just this SD-based heuristic.

## Reproducing

```r
Rscript fits/discriminative_design_search.R   # ~6 min, both seeds
```

Output: `fits/discriminative_design_search.RData` (`results`, keyed by seed
label: `seed_genome`/`seed_score`, `best_genome`/`best_score`, `trace`,
`n_trials_changed`).
