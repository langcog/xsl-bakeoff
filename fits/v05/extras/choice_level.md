# Choice-level evaluation (v05 fits)

Generated 2026-10-18 21:46:01 by `fits/v05/extras/choice_level.R` (n_sim = 200 for stochastic models). Log-likelihoods are per response (ll_total, ll_acc) or per error (ll_foil); lapse rates fit per model and per part. 'share' = fraction of the way from chance to the leave-one-out noise ceiling for that part (above 100% = better than predicting from the other participants' choices); 'acc beyond baseline' measures from the co-occurrence baseline instead. All 924 items are scored (no exclusions since XSLmodels PR #31).

- Chance / LOO ceiling / saturated: all responses -2.648 / -2.070 / -1.735; correct vs error -1.223 / -0.613 / -0.585; errors only -2.631 / -2.588 / -2.124.

## Out of fold (5-fold CV parameters; primary)

Each condition scored once, at parameters fit without it. Spearman rho between the SSE ranking and the choice-level ranking: 0.88 (all responses), -0.18 (foil choice only). (SSE = CV SSE.)

|Model              |  k|CV SSE |ll total |ll acc |share acc |acc beyond baseline |ll foil |share foil |top-foil hits |(chance) |foil lapse |
|:------------------|--:|:------|:--------|:------|:---------|:-------------------|:-------|:----------|:-------------|:--------|:----------|
|fazly              |  2|0.483  |-2.083   |-0.643 |95%       |77%                 |-2.615  |38%        |11%           |8%       |0.70       |
|kachergis_sampling |  3|0.530  |-2.087   |-0.649 |94%       |72%                 |-2.617  |33%        |11%           |8%       |0.71       |
|novelty            |  3|0.638  |-2.088   |-0.660 |92%       |63%                 |-2.616  |36%        |11%           |8%       |0.66       |
|uncfam_predictive  |  3|0.702  |-2.091   |-0.669 |91%       |56%                 |-2.610  |50%        |12%           |8%       |0.55       |
|uncfam_gamma       |  4|0.524  |-2.095   |-0.649 |94%       |72%                 |-2.621  |23%        |11%           |8%       |0.78       |
|Bayesian_decay     |  3|0.830  |-2.096   |-0.678 |89%       |49%                 |-2.608  |54%        |11%           |8%       |0.57       |
|uncfam_attention   |  3|0.585  |-2.100   |-0.658 |93%       |64%                 |-2.623  |18%        |11%           |8%       |0.77       |
|kachergis          |  3|0.587  |-2.100   |-0.659 |92%       |64%                 |-2.623  |18%        |11%           |8%       |0.78       |
|uncfam_elimination |  4|0.587  |-2.100   |-0.659 |92%       |64%                 |-2.623  |18%        |11%           |8%       |0.78       |
|minerva2           |  1|0.802  |-2.105   |-0.677 |89%       |49%                 |-2.608  |53%        |12%           |8%       |0.61       |
|tilles             |  3|0.639  |-2.108   |-0.661 |92%       |62%                 |-2.624  |17%        |11%           |8%       |0.79       |
|strength           |  2|0.957  |-2.123   |-0.699 |86%       |32%                 |-2.614  |40%        |12%           |8%       |0.62       |
|uncertainty        |  3|0.730  |-2.127   |-0.672 |90%       |54%                 |-2.620  |27%        |11%           |8%       |0.80       |
|softmax_rl         |  2|1.469  |-2.132   |-0.691 |87%       |38%                 |-2.610  |48%        |11%           |8%       |0.82       |
|baseline           |  0|1.628  |-2.189   |-0.740 |79%       |0%                  |-2.609  |52%        |11%           |8%       |0.80       |
|kalman_filter      |  3|1.628  |-2.189   |-0.740 |79%       |-0%                 |-2.609  |52%        |11%           |8%       |0.80       |
|rescorla-wagner    |  3|1.615  |-2.195   |-0.743 |79%       |-2%                 |-2.610  |50%        |12%           |8%       |0.80       |
|trueswell2012      |  2|2.065  |-2.202   |-0.708 |84%       |25%                 |-2.624  |18%        |10%           |8%       |0.94       |
|pursuit            |  3|1.490  |-2.205   |-0.723 |82%       |14%                 |-2.622  |21%        |10%           |8%       |0.90       |
|guess-and-test     |  2|2.088  |-2.206   |-0.711 |84%       |23%                 |-2.624  |18%        |10%           |8%       |0.94       |

## Group-fit parameters

Spearman rho between the SSE ranking and the choice-level ranking: 0.86 (all responses), -0.10 (foil choice only). (SSE = group SSE.)

|Model              |  k|group SSE |ll total |ll acc |share acc |acc beyond baseline |ll foil |share foil |top-foil hits |(chance) |foil lapse |
|:------------------|--:|:---------|:--------|:------|:---------|:-------------------|:-------|:----------|:-------------|:--------|:----------|
|fazlyt             |  3|0.473     |-2.081   |-0.642 |95%       |77%                 |-2.615  |38%        |11%           |8%       |0.70       |
|fazly              |  2|0.473     |-2.081   |-0.642 |95%       |77%                 |-2.615  |38%        |11%           |8%       |0.70       |
|uncfam_predictive  |  3|0.663     |-2.085   |-0.664 |92%       |60%                 |-2.610  |50%        |12%           |8%       |0.55       |
|novelty            |  3|0.620     |-2.086   |-0.658 |93%       |65%                 |-2.616  |36%        |11%           |8%       |0.65       |
|kachergis_sampling |  3|0.518     |-2.087   |-0.648 |94%       |72%                 |-2.616  |34%        |11%           |8%       |0.70       |
|Bayesian_decay     |  3|0.799     |-2.094   |-0.676 |90%       |51%                 |-2.608  |54%        |11%           |8%       |0.57       |
|uncfam_gamma       |  4|0.510     |-2.094   |-0.647 |94%       |73%                 |-2.621  |23%        |11%           |8%       |0.77       |
|uncfam_elimination |  4|0.555     |-2.099   |-0.656 |93%       |66%                 |-2.623  |18%        |11%           |8%       |0.77       |
|kachergis          |  3|0.555     |-2.099   |-0.656 |93%       |66%                 |-2.623  |18%        |11%           |8%       |0.77       |
|uncfam_attention   |  3|0.560     |-2.099   |-0.657 |93%       |66%                 |-2.624  |18%        |11%           |8%       |0.77       |
|tilles             |  3|0.613     |-2.104   |-0.658 |93%       |64%                 |-2.624  |18%        |11%           |8%       |0.79       |
|minerva2           |  1|0.795     |-2.104   |-0.676 |90%       |50%                 |-2.608  |55%        |11%           |8%       |0.61       |
|strength           |  2|0.910     |-2.120   |-0.695 |87%       |35%                 |-2.614  |40%        |11%           |8%       |0.62       |
|uncertainty        |  3|0.678     |-2.124   |-0.668 |91%       |57%                 |-2.620  |27%        |11%           |8%       |0.79       |
|softmax_rl         |  2|1.467     |-2.141   |-0.699 |86%       |33%                 |-2.610  |50%        |11%           |8%       |0.81       |
|baseline           |  0|1.628     |-2.189   |-0.740 |79%       |0%                  |-2.609  |52%        |11%           |8%       |0.80       |
|kalman_filter      |  3|1.628     |-2.189   |-0.740 |79%       |-0%                 |-2.609  |52%        |11%           |8%       |0.80       |
|rescorla-wagner    |  3|1.614     |-2.195   |-0.743 |79%       |-2%                 |-2.610  |50%        |12%           |8%       |0.80       |
|trueswell2012      |  2|2.049     |-2.196   |-0.703 |85%       |29%                 |-2.624  |18%        |10%           |8%       |0.94       |
|pursuit            |  3|1.468     |-2.205   |-0.716 |83%       |19%                 |-2.624  |18%        |9%            |8%       |0.91       |
|guess-and-test     |  2|2.086     |-2.209   |-0.713 |84%       |21%                 |-2.624  |18%        |10%           |8%       |0.94       |
