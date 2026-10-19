# Noise ceiling for the v05 lab fits: how much of each model's error could any
# model remove, given that human item accuracies are themselves noisy
# estimates from n_subj participants?
#
# 1. SSE floor. An item's observed accuracy a is the mean of n independent
#    participants' 0/1 responses, so even the true accuracy p misses it by
#    Var(a) = p(1 - p)/n on average (exactly, for participants sampled
#    independently from a population, whatever their individual differences).
#    p(1-p)/n is estimated without bias by a(1-a)/(n-1). Summed over a
#    condition's items, that is the condition's expected SSE for a perfect
#    model; weighting conditions by n_subj as the fits do gives the floor of
#    the fitted objective.
# 2. Correlation ceiling. With error variance e = mean(a(1-a)/(n-1)) and
#    observed variance v = var(a) across all items, the reliability of the item
#    accuracies is 1 - e/v, and no model's item-level r can exceed its square
#    root (in expectation).
# 3. Choice-level ceiling. For each response, the probability of the chosen
#    object under the other participants' choices on that item (leave one
#    out), smoothed by alpha over the item's alternatives, with alpha chosen
#    to maximize the total. A model that predicted choices as well as another
#    sample of people would reach this log-likelihood. Also computed for
#    errors only (which foil was chosen, given an error), and against chance.
#
# Writes fits/v05/extras/noise_ceiling.rds and noise_ceiling.md.
#
# Usage (from xsl-bakeoff/):
#   R_LIBS=fits/v05/Rlib Rscript fits/v05/extras/noise_ceiling.R

source(here::here("fits/v05/extras/common.R"))

## ---- 1-2. accuracy-level: SSE floor and r ceiling ---------------------------
items <- imap(xsl_datasets, \(d, i) tibble(
  label = d$label, condition = d$condition, item = seq_along(d$accuracy),
  acc = d$accuracy, n = rowSums(d$response_matrix), n_subj = d$n_subj)) |> list_rbind() |>
  mutate(err_var = acc * (1 - acc) / (n - 1))

cond_floor <- items |> group_by(label, condition) |>
  summarise(n_subj = first(n_subj), n_items = n(), mean_acc = mean(acc), floor = sum(err_var), .groups = "drop")
floor_w <- with(cond_floor, sum(floor * n_subj) / sum(n_subj))
reliability <- 1 - mean(items$err_var) / var(items$acc)
r_ceiling <- sqrt(reliability)

## models: group-fit predictions (re-simulated at the best parameters)
e <- new.env(); load(here("fits/v05/adults/group_fits.Rdata"), envir = e)
gfd <- e$gfd
model_cond <- gfd |> group_by(Model, condnum) |>
  summarise(sse = sum((ModelPerf - HumanPerf)^2), n = first(Nsubj), .groups = "drop")
model_tab <- model_cond |> group_by(Model) |>
  summarise(group_sse = sum(sse * n) / sum(n), .groups = "drop") |>
  left_join(gfd |> group_by(Model) |> summarise(r = cor(ModelPerf, HumanPerf)), by = "Model")
base_sse <- model_tab$group_sse[model_tab$Model == "baseline"]
model_tab <- model_tab |>
  mutate(excess = group_sse - floor_w,
         share_of_baseline_excess_removed = 1 - excess / (base_sse - floor_w),
         r_over_ceiling = r / r_ceiling) |>
  arrange(group_sse)

# where is the explainable error? best model per condition vs that condition's floor
best_per_cond <- model_cond |> filter(Model != "baseline") |>
  group_by(condnum) |> slice_min(sse, n = 1, with_ties = FALSE) |> ungroup() |>
  rename(label = condnum, best_model = Model, best_sse = sse) |> select(-n) |>
  left_join(cond_floor, by = "label") |>
  mutate(excess = best_sse - floor, excess_w = excess * n_subj / sum(cond_floor$n_subj)) |>
  arrange(desc(excess_w))

## ---- 3. choice-level ceiling ------------------------------------------------
# counts over each item's alternatives (the human side of choice_items(); a
# baseline run supplies the alternatives with no fitting involved)
base <- run_model("baseline", numeric(0))
ch <- map(base$fits, choice_items) |> list_rbind() |> filter(!excluded)
stopifnot(all(ch$n_out == 0))
k_alt <- lengths(ch$alts)
correct_idx <- map2_int(ch$alts, ch$correct, \(a, c) match(c, a))

loo_ll <- function(alpha, counts, k) {
  sum(map2_dbl(counts, k, \(c, k) {
    n <- sum(c); if (n < 2) return(0)
    sum(c * log((pmax(c - 1, 0) + alpha) / (n - 1 + k * alpha)))
  }))
}
foil_counts <- map2(ch$human, correct_idx, \(c, j) c[-j])
fit_alpha <- function(counts, k) optimize(\(a) -loo_ll(a, counts, k), c(1e-4, 20))$minimum
# in-sample empirical proportions: an upper bound no model can beat (it is
# fit to the very responses it scores); the LOO value is the realistic,
# conservative ceiling
sat_ll <- function(counts) sum(map_dbl(counts, \(c) { c <- c[c > 0]; sum(c * log(c / sum(c))) }))

acc_counts <- map2(ch$human, correct_idx, \(c, j) c(c[j], sum(c[-j])))   # correct vs error
a_all  <- fit_alpha(ch$human, k_alt)
a_acc  <- fit_alpha(acc_counts, rep(2, length(acc_counts)))
a_foil <- fit_alpha(foil_counts, k_alt - 1)
n_resp <- sum(map_dbl(ch$human, sum))
n_err  <- sum(map_dbl(foil_counts, sum))
# chance for correct-vs-error is guessing among the k alternatives: 1/k correct
chance_acc <- sum(map2_dbl(acc_counts, k_alt, \(c, k) c[1] * log(1 / k) + c[2] * log(1 - 1 / k)))
choice_ceiling <- tibble(
  part = c("all responses", "correct vs error", "errors only (which foil)"),
  n_responses = c(n_resp, n_resp, n_err),
  alpha = c(a_all, a_acc, a_foil),
  ll_ceiling_per_resp = c(loo_ll(a_all, ch$human, k_alt) / n_resp,
                          loo_ll(a_acc, acc_counts, rep(2, length(acc_counts))) / n_resp,
                          loo_ll(a_foil, foil_counts, k_alt - 1) / n_err),
  ll_saturated_per_resp = c(sat_ll(ch$human) / n_resp, sat_ll(acc_counts) / n_resp, sat_ll(foil_counts) / n_err),
  ll_chance_per_resp = c(sum(map2_dbl(ch$human, k_alt, \(c, k) -sum(c) * log(k))) / n_resp,
                         chance_acc / n_resp,
                         sum(map2_dbl(foil_counts, k_alt, \(c, k) -sum(c) * log(k - 1))) / n_err))

## ---- save + report -----------------------------------------------------------
res <- list(items = items, cond_floor = cond_floor, floor_w = floor_w,
            reliability = reliability, r_ceiling = r_ceiling, model_tab = model_tab,
            best_per_cond = best_per_cond, choice_ceiling = choice_ceiling,
            excluded = EXCLUDE_CHOICE, models_from = "fits/v05/adults/group_fits.Rdata",
            generated = format(Sys.time()))
saveRDS(res, file.path(EXTRAS_DIR, "noise_ceiling.rds"))

f3 <- \(x) sprintf("%.3f", x)
md <- c(
  "# Noise ceiling (v05 data)", "",
  paste0("Generated ", res$generated, " by `fits/v05/extras/noise_ceiling.R`. Models: v05 group fits ",
         "(`fits/v05/adults/group_fits.Rdata`, ", n_distinct(gfd$Model), " models so far)."), "",
  "## Accuracy level", "",
  paste0("- **SSE floor** (fitted objective: per-condition SSE, n_subj-weighted mean): **", f3(floor_w),
         "**. This is the expected error of a model that knew every item's true accuracy."),
  paste0("- **Reliability** of item accuracies (", nrow(items), " items): ", f3(reliability),
         "; **item-level r ceiling = ", f3(r_ceiling), "**."), "",
  knitr::kable(model_tab |> transmute(Model, `group SSE` = f3(group_sse), `excess over floor` = f3(excess),
                                      `share of baseline's excess removed` = sprintf("%.0f%%", 100 * share_of_baseline_excess_removed),
                                      r = sprintf("%.2f", r), `r / ceiling` = sprintf("%.2f", r_over_ceiling)),
               format = "pipe"), "",
  "Conditions with the most explainable error left (best model's SSE minus the floor, weighted by n_subj):", "",
  knitr::kable(head(best_per_cond, 12) |> transmute(label, condition, n_subj, n_items, floor = f3(floor),
                                                    best_model, best_sse = f3(best_sse), excess = f3(excess)),
               format = "pipe"), "",
  "## Choice level (log-likelihood per response)", "",
  paste0("Ceiling = leave-one-participant-out empirical choice proportions, smoothed by alpha (a conservative, ",
         "attainable ceiling); saturated = in-sample proportions (an upper bound no model can beat). ",
         "All 924 items are scored (no exclusions since XSLmodels PR #31 fixed the 211/214 test specs)."), "",
  knitr::kable(choice_ceiling |> mutate(across(where(is.double), \(x) signif(x, 4))), format = "pipe"))
writeLines(md, file.path(EXTRAS_DIR, "noise_ceiling.md"))
cat(md, sep = "\n")
