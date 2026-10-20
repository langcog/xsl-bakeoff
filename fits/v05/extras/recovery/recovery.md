# Model and parameter recovery (v05)

Generated 2026-10-20 14:53:38 by `fits/v05/extras/recovery.R`.

## Parameter recovery

r = correlation of true and recovered values across draws (log scale where marked); error on the same scale.

|model              |param |  n| n_distinct_true|logscale |r    | median_abs_err|at_bound |
|:------------------|:-----|--:|---------------:|:--------|:----|--------------:|:--------|
|Bayesian_decay     |p1    | 20|              20|TRUE     |0.83 |        0.06490|0%       |
|Bayesian_decay     |p2    | 20|              20|TRUE     |0.80 |        0.12500|0%       |
|Bayesian_decay     |p3    | 20|              20|TRUE     |0.74 |        0.13800|0%       |
|fazly              |p1    | 20|              20|TRUE     |0.90 |        0.69200|0%       |
|fazly              |p2    | 20|              20|TRUE     |0.99 |        0.09870|0%       |
|kachergis          |p1    | 20|              20|TRUE     |1.00 |        0.02580|0%       |
|kachergis          |p2    | 20|              20|TRUE     |1.00 |        0.03920|0%       |
|kachergis          |p3    | 20|              20|FALSE    |1.00 |        0.00113|0%       |
|kachergis_sampling |p1    | 20|              20|TRUE     |0.99 |        0.16500|0%       |
|kachergis_sampling |p2    | 20|              20|TRUE     |0.78 |        0.19000|0%       |
|kachergis_sampling |p3    | 20|              20|FALSE    |0.99 |        0.00733|0%       |
|minerva2           |p1    | 20|              20|TRUE     |0.98 |        0.06370|0%       |
|softmax_rl         |p1    | 20|              20|FALSE    |0.88 |        0.05800|0%       |
|softmax_rl         |p2    | 20|              20|TRUE     |0.80 |        0.35000|0%       |
|tilles             |p1    | 20|              20|FALSE    |0.96 |        0.01440|0%       |
|tilles             |p2    | 20|              20|FALSE    |0.93 |        0.01360|0%       |
|tilles             |p3    | 20|              20|FALSE    |0.99 |        0.00600|0%       |
|trueswell2012      |p1    | 20|              20|FALSE    |0.33 |        0.17700|0%       |
|trueswell2012      |p2    | 20|              20|FALSE    |0.82 |        0.14900|0%       |

## Model recovery

45 complete synthetic datasets (generator x rep). Confusion: P(winning fitter | generator), rows = generator.

|gen                |Bayesian_decay |baseline |fazly |kachergis |kachergis_sampling |minerva2 |softmax_rl |tilles |trueswell2012 |
|:------------------|:--------------|:--------|:-----|:---------|:------------------|:--------|:----------|:------|:-------------|
|Bayesian_decay     |1.00           |0        |0     |0         |0                  |0        |0          |0      |0             |
|baseline           |0              |1.00     |0     |0         |0                  |0        |0          |0      |0             |
|fazly              |0              |0        |1.00  |0         |0                  |0        |0          |0      |0             |
|kachergis          |0              |0        |0     |1.00      |0                  |0        |0          |0      |0             |
|kachergis_sampling |0              |0        |0     |0         |1.00               |0        |0          |0      |0             |
|minerva2           |0              |0        |0     |0         |0                  |1.00     |0          |0      |0             |
|softmax_rl         |0              |0        |0     |0         |0                  |0        |1.00       |0      |0             |
|tilles             |0              |0        |0     |0         |0                  |0        |0          |1.00   |0             |
|trueswell2012      |0              |0        |0     |0         |0                  |0        |0          |0      |1.00          |

Inversion: P(generator | winning fitter), rows = winning fitter.

|fitter             |Bayesian_decay |baseline |fazly |kachergis |kachergis_sampling |minerva2 |softmax_rl |tilles |trueswell2012 |
|:------------------|:--------------|:--------|:-----|:---------|:------------------|:--------|:----------|:------|:-------------|
|Bayesian_decay     |1.00           |0        |0     |0         |0                  |0        |0          |0      |0             |
|baseline           |0              |1.00     |0     |0         |0                  |0        |0          |0      |0             |
|fazly              |0              |0        |1.00  |0         |0                  |0        |0          |0      |0             |
|kachergis          |0              |0        |0     |1.00      |0                  |0        |0          |0      |0             |
|kachergis_sampling |0              |0        |0     |0         |1.00               |0        |0          |0      |0             |
|minerva2           |0              |0        |0     |0         |0                  |1.00     |0          |0      |0             |
|softmax_rl         |0              |0        |0     |0         |0                  |0        |1.00       |0      |0             |
|tilles             |0              |0        |0     |0         |0                  |0        |0          |1.00   |0             |
|trueswell2012      |0              |0        |0     |0         |0                  |0        |0          |0      |1.00          |

Margin: the generator's own objective minus the best other model's (negative = generator wins).

|gen                | mean_margin|closest_rival      |
|:------------------|-----------:|:------------------|
|Bayesian_decay     |     -0.1150|fazly              |
|baseline           |     -0.0182|softmax_rl         |
|fazly              |     -0.1500|kachergis_sampling |
|kachergis          |     -0.0945|kachergis_sampling |
|kachergis_sampling |     -0.1150|kachergis          |
|minerva2           |     -0.0783|fazly              |
|softmax_rl         |     -0.0892|minerva2           |
|tilles             |     -0.2200|fazly              |
|trueswell2012      |     -0.0762|softmax_rl         |
