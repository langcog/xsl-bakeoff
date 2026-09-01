# Compares fits/pre-package/{group_fits,cv_group_fits}.Rdata (the original,
# ad hoc-fitting_functions.R-based fits) against fits/{group_fits,cv_group_fits}.Rdata
# (the same fits recreated with the XSLmodels package, via
# recreate_fits_with_package.R), to confirm the package reproduces the
# original paper's results.
#
# Three checks, in increasing order of how much they depend on DEoptim
# actually converging:
#   1. Same-parameters check (fast, no fitting involved): plugs the
#      ORIGINAL fits' best-fit parameters directly into the package's
#      models and compares item-level perf. This isolates "does the model
#      implementation agree" from "did the optimizer land on the same
#      optimum" -- it works even if fits/*.Rdata was produced with
#      recreate_fits_with_package.R's quick <- TRUE (a non-converged smoke
#      test), since it never uses the NEW fits' own parameters.
#   2. Group-fit SSE/perf comparison: compares the two runs' own best-fit
#      parameters and resulting SSE. Only meaningful if both were fit with
#      real (quick <- FALSE) settings.
#   3. Cross-validated fit comparison, including a check that the fold
#      assignments actually match the original (they should, if
#      recreate_fits_with_package.R's caret::createFolds/seed setup was
#      left unchanged).
#
# See model_specs.R for the model-by-model mapping and its documented,
# KNOWN discrepancies (fazly, trueswell2012) -- those two are expected to
# NOT match regardless of fit quality, and are called out separately below
# rather than silently averaged in with everything else.

library(XSLmodels)
library(purrr)
library(dplyr)
library(tibble)
library(here)

source(here("fits/model_specs.R")) # model_specs, fazlyt_spec, known_discrepancies

flag <- \(model) if (model %in% names(known_discrepancies)) " [KNOWN DISCREPANCY]" else ""

data44 <- xsl_datasets[1:44]

old_group <- local({ load(here("fits/pre-package/group_fits.Rdata")); list(group_fits = group_fits, gfd = gfd) })
old_cv <- local({ load(here("fits/pre-package/cv_group_fits.Rdata")); cv_group_fits })

## ==== 1. Same-parameters check: original bestmem -> package models ====
# No fitting at all here -- just runs the package's models once, using the
# ORIGINAL fits' own best parameters, and compares against the ORIGINAL
# fits' own item-level perf. A mismatch here means the model
# implementations disagree; it can't be blamed on DEoptim not converging.

cat("=== Same-parameters check: original bestmem run through package models ===\n")
same_param_tab <- map_df(names(model_specs), \(m) {
  if (!m %in% names(old_group$group_fits)) return(NULL)
  bestmem <- old_group$group_fits[[m]]$optim$bestmem
  spec <- model_specs[[m]]
  ctrl <- if (spec$stochastic) xslControl(n_sim = 500) else xslControl()
  new_result <- xsl_run(spec$constructor(bestmem), data44, control = ctrl)

  # one row per (condition, item), condition order matching data44's label
  # order; a stable sort by condnum preserves each condition's own
  # (already-consistent, ascending-item-index) row order, so sorting both
  # old and new the same way aligns them item for item
  new_df <- map(new_result$fits, \(f) {
    tibble(condnum = f$data$label, ModelPerf = as.vector(f$perf))
  }) |> list_rbind() |> arrange(condnum)
  old_df <- old_group$gfd |> filter(Model == m) |> arrange(condnum)

  stopifnot(identical(old_df$condnum, new_df$condnum))
  d <- abs(old_df$ModelPerf - new_df$ModelPerf)
  tibble(Model = m, max_abs_diff = max(d), mean_abs_diff = mean(d),
        all_equal = isTRUE(all.equal(old_df$ModelPerf, new_df$ModelPerf)),
        note = trimws(flag(m)))
})
print(same_param_tab, n = Inf)
cat("\n(fazly and trueswell2012 are EXPECTED to fail this check -- see model_specs.R.",
    "\n Any other model failing this check indicates a real parameter-mapping bug.)\n\n")

## ==== 2. Group fits: best-fit SSE per model (own parameters) ====

if (file.exists(here("fits/group_fits.Rdata"))) {
  new_group <- local({ load(here("fits/group_fits.Rdata")); list(group_fits = group_fits, gfd = gfd) })

  cat("=== Group fits: best-fit (all 44 conditions) SSE, own parameters ===\n")
  common_models <- intersect(names(old_group$group_fits), names(new_group$group_fits))

  group_sse_tab <- map_df(common_models, \(m) {
    old_sse <- old_group$group_fits[[m]]$optim$bestval
    new_sse <- new_group$group_fits[[m]]$optim$bestval
    tibble(Model = m, `Old SSE` = old_sse, `New SSE` = new_sse, Diff = new_sse - old_sse,
          Note = trimws(known_discrepancies[m] %||% ""))
  }) |> arrange(`Old SSE`)
  print(group_sse_tab, n = Inf)

  cat("\n=== Group fits: item-level ModelPerf agreement (gfd), by model ===\n")
  gfd_compare <- map_df(common_models, \(m) {
    o <- old_group$gfd |> filter(Model == m) |> arrange(condnum)
    n <- new_group$gfd |> filter(Model == m) |> arrange(condnum)
    if (nrow(o) != nrow(n)) {
      return(tibble(Model = m, n_items = NA, max_abs_diff = NA, all_equal = FALSE,
                    note = sprintf("row count differs: old=%d new=%d", nrow(o), nrow(n))))
    }
    d <- abs(o$ModelPerf - n$ModelPerf)
    tibble(Model = m, n_items = nrow(o), max_abs_diff = max(d),
          all_equal = isTRUE(all.equal(o$ModelPerf, n$ModelPerf)), note = flag(m))
  })
  print(gfd_compare, n = Inf)
} else {
  cat("=== Group fits: fits/group_fits.Rdata not found -- run recreate_fits_with_package.R first ===\n")
}

## ==== 3. Cross-validated fits ====

if (file.exists(here("fits/cv_group_fits.Rdata"))) {
  new_cv <- local({ load(here("fits/cv_group_fits.Rdata")); cv_group_fits })
  cv_models <- intersect(names(old_cv), names(new_cv))

  cat("\n=== Cross-validated fits: mean test SSE across folds ===\n")
  cv_sse_tab <- map_df(cv_models, \(m) {
    old_test_sse <- map_dbl(old_cv[[m]]$test, "SSE")
    new_test_sse <- map_dbl(new_cv[[m]]$test, \(t) t$sse)
    tibble(Model = m, `Mean Old Test SSE` = mean(old_test_sse),
          `Mean New Test SSE` = mean(new_test_sse),
          `Mean Diff` = mean(new_test_sse - old_test_sse))
  }) |> arrange(`Mean Old Test SSE`) |>
    mutate(Note = trimws(map_chr(Model, \(m) known_discrepancies[m] %||% "")))
  print(cv_sse_tab, n = Inf)

  cat("\n=== Fold assignment check ===\n")
  # both should have used caret::createFolds(<44 labels, in combined_data
  # order>, k=5) with set.seed(123); confirm the actual held-out conditions
  # per fold match, since a silent fold mismatch would invalidate the CV
  # comparison above.
  labels44 <- map_chr(data44, "label")
  set.seed(123)
  folds <- caret::createFolds(labels44, k = 5, list = TRUE)
  new_fold_conds <- map(folds, \(idx) sort(labels44[idx]))
  old_fold_conds <- map(old_cv[[cv_models[1]]]$test, \(t) sort(setdiff(names(t), "SSE")))
  fold_match <- map2_lgl(old_fold_conds, new_fold_conds, identical)
  cat("folds identical to original:", all(fold_match), "\n")
  if (!all(fold_match)) cat("MISMATCHED FOLDS:", which(!fold_match), "\n")
} else {
  cat("\n=== Cross-validated fits: fits/cv_group_fits.Rdata not found -- run recreate_fits_with_package.R first ===\n")
}

cat("\nDone. See model_specs.R's known_discrepancies for models expected to",
    "differ regardless of fit quality.\n")
