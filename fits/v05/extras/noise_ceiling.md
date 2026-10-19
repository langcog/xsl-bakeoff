# Noise ceiling (v05 data)

Generated 2026-10-18 21:46:00 by `fits/v05/extras/noise_ceiling.R`. Models: v05 group fits (`fits/v05/adults/group_fits.Rdata`, 21 models so far).

## Accuracy level

- **SSE floor** (fitted objective: per-condition SSE, n_subj-weighted mean): **0.117**. This is the expected error of a model that knew every item's true accuracy.
- **Reliability** of item accuracies (924 items): 0.865; **item-level r ceiling = 0.930**.

|Model              |group SSE |excess over floor |share of baseline's excess removed |r    |r / ceiling |
|:------------------|:---------|:-----------------|:----------------------------------|:----|:-----------|
|fazly              |0.473     |0.356             |76%                                |0.70 |0.75        |
|fazlyt             |0.473     |0.356             |76%                                |0.70 |0.75        |
|uncfam_gamma       |0.510     |0.393             |74%                                |0.72 |0.78        |
|kachergis_sampling |0.518     |0.400             |73%                                |0.72 |0.77        |
|kachergis          |0.555     |0.438             |71%                                |0.71 |0.76        |
|uncfam_elimination |0.555     |0.438             |71%                                |0.71 |0.76        |
|uncfam_attention   |0.560     |0.443             |71%                                |0.71 |0.76        |
|tilles             |0.613     |0.495             |67%                                |0.60 |0.64        |
|novelty            |0.620     |0.502             |67%                                |0.67 |0.72        |
|uncfam_predictive  |0.663     |0.545             |64%                                |0.58 |0.62        |
|uncertainty        |0.678     |0.561             |63%                                |0.63 |0.68        |
|minerva2           |0.795     |0.678             |55%                                |0.51 |0.55        |
|Bayesian_decay     |0.799     |0.682             |55%                                |0.55 |0.59        |
|strength           |0.910     |0.793             |48%                                |0.53 |0.57        |
|softmax_rl         |1.467     |1.350             |11%                                |0.42 |0.45        |
|pursuit            |1.468     |1.350             |11%                                |0.51 |0.55        |
|rescorla-wagner    |1.614     |1.497             |1%                                 |0.34 |0.37        |
|baseline           |1.628     |1.511             |0%                                 |0.34 |0.37        |
|kalman_filter      |1.628     |1.511             |-0%                                |0.34 |0.37        |
|trueswell2012      |2.049     |1.932             |-28%                               |0.35 |0.38        |
|guess-and-test     |2.086     |1.969             |-30%                               |0.33 |0.36        |

Conditions with the most explainable error left (best model's SSE minus the floor, weighted by n_subj):

|label                    |condition                     | n_subj| n_items|floor |best_model     |best_sse |excess |
|:------------------------|:-----------------------------|------:|-------:|:-----|:--------------|:--------|:------|
|freq369-3x3hiCD          |freq369-3x3hiCD               |    104|      18|0.041 |uncfam_gamma   |0.302    |0.261  |
|freq369_39mx             |freq369_39mx                  |     40|      18|0.099 |trueswell2012  |0.574    |0.475  |
|212                      |2x3 6/9 4AFC                  |     38|      18|0.099 |Bayesian_decay |0.426    |0.327  |
|301                      |2x2 5x 24AFC                  |     46|      24|0.126 |novelty        |0.324    |0.198  |
|filt0E_3L                |filt0E_3L                     |     31|      24|0.177 |Bayesian_decay |0.461    |0.283  |
|2_x8_39_4x4              |2_x8_39_4x4                   |     30|      18|0.134 |fazlyt         |0.414    |0.279  |
|1olap3tr_contr           |1 overlap per 3 trials, contr |     31|      18|0.117 |trueswell2012  |0.378    |0.261  |
|freq369-3x3loCD          |freq369-3x3loCD               |     34|      18|0.110 |tilles         |0.348    |0.238  |
|205                      |2x4                           |     33|      18|0.107 |tilles         |0.350    |0.244  |
|213                      |2x4 6/12 4AFC                 |     31|      18|0.133 |tilles         |0.378    |0.245  |
|temp_cont_1olap2tr_contr |1 overlap per 2 trials, contr |     31|      18|0.110 |strength       |0.313    |0.204  |
|225                      |2x2                           |     19|      18|0.155 |trueswell2012  |0.479    |0.324  |

## Choice level (log-likelihood per response)

Ceiling = leave-one-participant-out empirical choice proportions, smoothed by alpha (a conservative, attainable ceiling); saturated = in-sample proportions (an upper bound no model can beat). All 924 items are scored (no exclusions since XSLmodels PR #31 fixed the 211/214 test specs).

|part                     | n_responses|  alpha| ll_ceiling_per_resp| ll_saturated_per_resp| ll_chance_per_resp|
|:------------------------|-----------:|------:|-------------------:|---------------------:|------------------:|
|all responses            |       31270| 0.8851|             -2.0700|               -1.7350|             -2.648|
|correct vs error         |       31270| 2.3810|             -0.6128|               -0.5848|             -1.223|
|errors only (which foil) |       16930| 3.4650|             -2.5880|               -2.1240|             -2.631|
