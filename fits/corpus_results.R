# Paper-facing wrapper around XSLmodels' naturalistic-corpus comparison.
#
# The actual model runs live in the XSLmodels package repo (they need the
# package's internal model registry and take ~30 min / ~3 h respectively):
#   tests/bakeoff_comparison/corpus_gold_comparison.R  (defaults, both corpora)
#   tests/bakeoff_comparison/corpus_gold_fits.R        (fit-to-corpus, both corpora)
# See tests/bakeoff_comparison/corpus_report.md there for the full write-up
# this section is based on.
#
# This script does NOT re-run those -- it takes their saved .rds output
# (copied here as fits/corpus/corpus_gold_{comparison,fits}.rds; re-copy from
# the package repo if the corpus comparison is re-run there) and reshapes it
# into what the paper needs: a model x corpus table using the paper's
# abbreviations, restricted to models in our lab roster (plus fgt2009/
# fgt2009_rsa, which have no lab counterpart), and a comparison of each
# model's corpus rank against its lab (53-condition group fit) rank.
#
# Output: fits/corpus_results.RData with
#   corpus_tab    one row per (package model name, corpus): f_default,
#                 f_fitted, precision/recall/threshold at default, lab_abbr,
#                 in_lab_roster
#   rank_compare  one row per model in both the lab roster and the corpus
#                 comparison: lab_rank (by 53-cond group-fit SSE, ascending),
#                 corpus_rank (by mean best-F across corpora, descending)

library(dplyr)
library(tidyr)
library(here)

## ---------------------------------------------------------------------------
## Map corpus-comparison model names (XSLmodels registry names) onto the
## paper's abbreviations. uncfam() with variant = "entropy" (the registry
## default) is exactly the lab FU model; uncfam_sampling() is FUs. `decay`,
## `tilles`, and `multi_sampling` are not part of the lab's 15-model roster
## (dropped by the original author / numerically unstable / not in the
## paper); fgt2009 and fgt2009_rsa have no lab counterpart at all.
## ---------------------------------------------------------------------------

corpus_to_lab <- c(
  bayesian_decay      = "Bayesian_decay",
  fazly                = "fazly",
  uncfam                = "kachergis",
  uncfam_attention       = "uncfam_attention",
  uncfam_predictive      = "uncfam_predictive",
  uncfam_sampling        = "kachergis_sampling",
  rescorla_wagner        = "rescorla-wagner",
  kalman_filter          = "kalman_filter",
  softmax_rl             = "softmax_rl",
  guess_and_test         = "guess-and-test",
  propose_but_verify     = "trueswell2012",
  pursuit                = "pursuit",
  baseline               = "baseline"
  # decay, tilles, multi_sampling, fgt2009, fgt2009_rsa: no lab mapping
)

default_df <- readRDS(here("fits/corpus/corpus_gold_comparison.rds")) %>%
  select(corpus, model, f_default = f_max, precision, recall, threshold)
fits_df <- readRDS(here("fits/corpus/corpus_gold_fits.rds")) %>%
  select(corpus, model, f_default_check = f_default, f_fitted)

corpus_tab <- full_join(default_df, fits_df, by = c("corpus", "model")) %>%
  mutate(
    f_fitted = coalesce(f_fitted, f_default), # a few models weren't (re)fit; best = default
    lab_model = corpus_to_lab[model],
    in_lab_roster = !is.na(lab_model)
  ) %>%
  select(-f_default_check) %>%
  arrange(desc(f_fitted))

## ---------------------------------------------------------------------------
## Lab rank (53-condition group fit, ascending SSE = better) vs. corpus rank
## (mean best-F across both corpora, descending = better), for models in both
## ---------------------------------------------------------------------------

lab_group_fit <- local({
  e1 <- new.env(); load(here("fits/group_fits_all_experiments.Rdata"), envir = e1)
  e2 <- new.env(); load(here("fits/group_fits_new_models.Rdata"),       envir = e2)
  bind_rows(e1$gfd_all, e2$gfd_new)
}) %>%
  filter(!is.na(HumanPerf)) %>%
  group_by(Model) %>%
  summarise(SSE = sum((ModelPerf - HumanPerf)^2), .groups = "drop") %>%
  mutate(lab_rank = rank(SSE))

corpus_overall <- corpus_tab %>%
  filter(in_lab_roster) %>%
  group_by(lab_model) %>%
  summarise(mean_best_f = mean(f_fitted), .groups = "drop") %>%
  mutate(corpus_rank = rank(-mean_best_f))

rank_compare <- inner_join(lab_group_fit, corpus_overall,
                           by = c("Model" = "lab_model")) %>%
  arrange(lab_rank)

save(corpus_tab, rank_compare, file = here("fits/corpus_results.RData"))

message("Rank correlation (lab SSE rank vs. corpus F rank), Spearman rho = ",
       round(cor(rank_compare$lab_rank, rank_compare$corpus_rank, method = "spearman"), 2))
print(rank_compare, n = Inf)
