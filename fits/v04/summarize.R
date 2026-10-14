# Round-2 results summary: group fit, 5-fold CV, and the holdout
# generalization test (split by adults vs. children), one row per model.
#
# Usage (from xsl-bakeoff/, after refit.R group + cv and generalize.R):
#   R_LIBS=$(pwd)/fits/v04/Rlib Rscript fits/v04/summarize.R
#
# Columns
#   group_sse        group fit to all 56 conditions (DEoptim bestval: the
#                    n_subj-weighted mean of per-condition SSE)
#   cv_train/cv_test the same objective, averaged over the 5 folds; cv_test is
#                    each fold's held-out conditions scored at that fold's fit
#   cv_test_min/max  range of the 5 folds' test SSE
#   cv_mse / cv_r    per-item MSE / model-human r pooled over all 56
#                    conditions' out-of-fold predictions
#   adult_* / kid_*  holdout per-item MSE / r at the group-fit params:
#                    adults = benitez2020 adult conditions + kachergis_initial_accuracy
#                    (MTurk adults); kids = benitez2020 child conditions
# baseline has no parameters, so its CV test SSE is just its SSE on each
# fold's test conditions (a reference row); fazlyt is group-fit only.
#
# Writes fits/v04/summary.rds and fits/v04/summary.md

suppressMessages({
  library(XSLmodels)
  library(purrr)
  library(dplyr)
  library(tidyr)
  library(here)
})
source(here("fits/v04/specs.R"))

load(here("fits/v04/group_fits.Rdata"))       # group_fits, gfd, meta
load(here("fits/v04/cv_fits.Rdata"))          # cv_fits, folds, meta
fit_meta <- meta
load(here("fits/v04/generalization.Rdata"))   # gen, gen_summary (and its own meta)
meta <- fit_meta

# baseline's per-fold test SSE, for reference
base_folds <- map_dbl(folds, \(f) xsl_run(baseline(), get_train_test_split(f, xsl_datasets)$test)$sse)
base_testdf <- map(xsl_datasets, \(d) {
  f <- xsl_run(baseline(), d)$fits[[1]]
  tibble(ModelPerf = as.vector(f$perf), HumanPerf = d$accuracy)
}) |> list_rbind()

cv_row <- function(m) {
  if (m == "baseline") {
    return(tibble(cv_train = NA_real_, cv_test = mean(base_folds),
                  cv_test_min = min(base_folds), cv_test_max = max(base_folds),
                  cv_mse = mean((base_testdf$ModelPerf - base_testdf$HumanPerf)^2, na.rm = TRUE),
                  cv_r = cor(base_testdf$ModelPerf, base_testdf$HumanPerf, use = "complete.obs"),
                  cv_folds = 5L))
  }
  fo <- compact(cv_fits[[m]]$folds)
  if (!length(fo)) return(tibble(cv_folds = 0L))
  test <- map_dbl(fo, "test_sse")
  td <- list_rbind(map(fo, "testdf"))
  tibble(cv_train = mean(map_dbl(fo, "train_sse")), cv_test = mean(test),
         cv_test_min = min(test), cv_test_max = max(test),
         cv_mse = mean((td$ModelPerf - td$HumanPerf)^2, na.rm = TRUE),
         cv_r = cor(td$ModelPerf, td$HumanPerf, use = "complete.obs"),
         cv_folds = length(fo))
}

holdout <- gen |>
  mutate(grp = if_else(grepl("kids", condition), "kid", "adult")) |>
  group_by(model, grp) |>
  summarise(mse = sum(sse) / sum(n_items),
            r = suppressWarnings(cor(unlist(model_perf), unlist(human), use = "complete.obs")),
            .groups = "drop") |>
  pivot_wider(names_from = grp, values_from = c(mse, r), names_glue = "{grp}_{.value}")

summary_tbl <- tibble(model = RUN_ORDER) |>
  mutate(n_params = map_int(model, \(m) length(specs_v04[[m]]$lower)),
         stochastic = map_lgl(model, \(m) specs_v04[[m]]$stochastic),
         group_sse = map_dbl(model, \(m) group_fits[[m]]$optim$bestval),
         cv = map(model, cv_row)) |>
  unnest(cv) |>
  left_join(holdout, by = "model") |>
  select(model, n_params, stochastic, group_sse, cv_train, cv_test, cv_test_min, cv_test_max,
         cv_mse, cv_r, cv_folds, adult_mse, adult_r, kid_mse, kid_r) |>
  arrange(is.na(cv_test), cv_test)

saveRDS(list(summary = summary_tbl, base_folds = base_folds, meta = meta,
             made = format(Sys.time())), here("fits/v04/summary.rds"))

fmt <- summary_tbl |>
  mutate(cv_range = sprintf("%.3f-%.3f", cv_test_min, cv_test_max),
         cv_range = if_else(is.na(cv_test_min), NA_character_, cv_range)) |>
  select(model, k = n_params, group_sse, cv_train, cv_test, cv_range, cv_r,
         adult_mse, adult_r, kid_mse, kid_r)
md <- c("# Round-2 refit: group fit, 5-fold CV, holdout generalization", "",
        paste0("XSLmodels ", meta$xslmodels_version, " @ ", substr(meta$xslmodels_commit, 1, 7),
               "; 56 conditions; n_sim = ", meta$n_sim, "; generated ", format(Sys.time(), "%F %R")), "",
        "Sorted by CV test SSE (lower is better). See fits/v04/summarize.R for column definitions.", "",
        knitr::kable(fmt, digits = 3))
writeLines(md, here("fits/v04/summary.md"))
print(as.data.frame(fmt), digits = 3, row.names = FALSE)
