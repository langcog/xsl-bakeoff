# Model and parameter recovery (v05, error-driven models)

Generated 2026-11-06 10:43:41 by `fits/v05/extras/recovery.R` with REC_SET=rwkf, XSLmodels rwkf library.

## Parameter recovery

r = correlation of true and recovered values across draws (log scale where marked); error on the same scale.

|model                          |param |  n| n_distinct_true|logscale |r    | median_abs_err|at_bound |
|:------------------------------|:-----|--:|---------------:|:--------|:----|--------------:|:--------|
|baseline_softmax               |p1    | 20|              20|TRUE     |1.00 |        0.00729|0%       |
|kalman_filter_full             |p1    | 20|              20|FALSE    |0.80 |        0.02750|5%       |
|kalman_filter_full             |p2    | 20|              20|TRUE     |0.95 |        0.14000|5%       |
|kalman_filter_full             |p3    | 20|              20|TRUE     |1.00 |        0.01040|0%       |
|rescorla-wagner                |p1    | 20|              20|FALSE    |0.31 |        0.21700|15%      |
|rescorla-wagner                |p2    | 20|              20|FALSE    |1.00 |        0.01590|0%       |
|rescorla-wagner                |p3    | 20|              20|FALSE    |0.68 |        0.19600|15%      |
|rescorla-wagner_discriminative |p1    | 20|              20|FALSE    |0.99 |        0.00589|0%       |
|rescorla-wagner_discriminative |p2    | 20|              20|FALSE    |1.00 |        0.00101|0%       |
|rescorla-wagner_discriminative |p3    | 20|              20|TRUE     |1.00 |        0.01570|0%       |
|rescorla-wagner_softmax        |p1    | 20|              20|FALSE    |0.96 |        0.01120|0%       |
|rescorla-wagner_softmax        |p2    | 20|              20|FALSE    |1.00 |        0.00591|0%       |
|rescorla-wagner_softmax        |p3    | 20|              20|TRUE     |0.98 |        0.03600|0%       |

## Model recovery

40 complete synthetic datasets (generator x rep). Confusion: P(winning fitter | generator), rows = generator.

|gen                            |baseline |rescorla-wagner |baseline_softmax |fazly |kachergis |kalman_filter_full |rescorla-wagner_discriminative |rescorla-wagner_softmax |
|:------------------------------|:--------|:---------------|:----------------|:-----|:---------|:------------------|:------------------------------|:-----------------------|
|baseline                       |0.20     |0.80            |0                |0     |0         |0                  |0                              |0                       |
|baseline_softmax               |0        |0               |1.00             |0     |0         |0                  |0                              |0                       |
|fazly                          |0        |0               |0                |1.00  |0         |0                  |0                              |0                       |
|kachergis                      |0        |0               |0                |0     |1.00      |0                  |0                              |0                       |
|kalman_filter_full             |0        |0               |0                |0     |0         |1.00               |0                              |0                       |
|rescorla-wagner                |0        |1.00            |0                |0     |0         |0                  |0                              |0                       |
|rescorla-wagner_discriminative |0        |0               |0                |0     |0         |0                  |1.00                           |0                       |
|rescorla-wagner_softmax        |0        |0               |0                |0     |0         |0                  |0                              |1.00                    |

Inversion: P(generator | winning fitter), rows = winning fitter.

|fitter                         |baseline |baseline_softmax |fazly |kachergis |kalman_filter_full |rescorla-wagner |rescorla-wagner_discriminative |rescorla-wagner_softmax |
|:------------------------------|:--------|:----------------|:-----|:---------|:------------------|:---------------|:------------------------------|:-----------------------|
|baseline                       |1.00     |0                |0     |0         |0                  |0               |0                              |0                       |
|baseline_softmax               |0        |1.00             |0     |0         |0                  |0               |0                              |0                       |
|fazly                          |0        |0                |1.00  |0         |0                  |0               |0                              |0                       |
|kachergis                      |0        |0                |0     |1.00      |0                  |0               |0                              |0                       |
|kalman_filter_full             |0        |0                |0     |0         |1.00               |0               |0                              |0                       |
|rescorla-wagner                |0.44     |0                |0     |0         |0                  |0.56            |0                              |0                       |
|rescorla-wagner_discriminative |0        |0                |0     |0         |0                  |0               |1.00                           |0                       |
|rescorla-wagner_softmax        |0        |0                |0     |0         |0                  |0               |0                              |1.00                    |

Margin: the generator's own objective minus the best other model's (negative = generator wins).

|gen                            | mean_margin|closest_rival                  |
|:------------------------------|-----------:|:------------------------------|
|baseline                       |    0.000109|rescorla-wagner                |
|baseline_softmax               |   -0.016400|rescorla-wagner_softmax        |
|fazly                          |   -0.104000|rescorla-wagner_softmax        |
|kachergis                      |   -0.267000|fazly                          |
|kalman_filter_full             |   -0.032500|rescorla-wagner_discriminative |
|rescorla-wagner                |   -0.000477|baseline                       |
|rescorla-wagner_discriminative |   -0.028000|rescorla-wagner_softmax        |
|rescorla-wagner_softmax        |   -0.045900|fazly                          |
