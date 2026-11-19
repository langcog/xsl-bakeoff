# Human performance in Yurovsky & Frank (2015) against the paper's Integrated
# model, the trial-level yf2015() model, and other bake-off models.
#
# Figure set 1 (yf_models_exp12.png, yf_models_exp3.png):
#   - Paper (published): the closed-form Integrated model, yf2015_predict(),
#     at the paper's posterior means (sigma .604, lambda .184, gamma 1.495; fit
#     with the transposed index), with its New Label rule. This is the paper's
#     Fig. 3. The paper has no model of Exp 3.
#   - Trial-level, Exp 1 fit: yf2015() at the corrected Exp 1 estimates
#     (sigma .568, lambda .167, gamma 1.617), with mutual exclusivity
#     mu = 1.41 (the maximum-likelihood value for Exp 2's New Label trials;
#     PROVENANCE.md). Its Same and Switch predictions equal the closed form's.
#   - yf2015_g and yf2015: the bake-off group fits to xsl_datasets (gamma
#     fixed at 1.62, and free), never fit to these data.
#   The trial-level models are run with yf2015_paradigm(): the response on the
#   exposure trial is the learner's sampled hypothesis.
#
# Figure set 2 (yf_models_assoc_exp12.png, yf_models_assoc_exp3.png):
#   fazly, uncfam_gamma and kachergis_sampling, from their xsl_datasets group
#   fits, as score_yf_all.R predicts them (cached in yf_scores/; the exposure
#   response is the model's own choice right after the trial). Solid: with
#   the lapse rate fit to Exp 1; dashed: no lapse. The paper's model is shown
#   for reference.
#
# Usage (from xsl-bakeoff/, after score_yf_all.R):
#   R_LIBS=$(pwd)/fits/v06/yf2015/Rlib Rscript fits/v06/yf2015/plot_yf_models.R
# Output: the four figures and yf_models_predictions.rds.

suppressPackageStartupMessages({
  library(XSLmodels)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

DIR <- "fits/v06/yf2015"
source(file.path(DIR, "specs_yf2015.R"))
load(file.path(DIR, "yf2015_fits.Rdata"))  # prog

PAPER <- "Paper (published)"
LAPSE <- c("fitted lapse", "no lapse")
clamp <- \(p) pmin(pmax(p, 1e-9), 1 - 1e-9)
beta_ci <- \(d) mutate(d, lo = qbeta(.025, k + .5, n - k + .5), hi = qbeta(.975, k + .5, n - k + .5))

# ---- human data ----
obs12 <- yurovsky_frank2015$exp12 |>
  mutate(trial_type = as.character(trial_type)) |>
  group_by(n_referents, interval, trial_type) |>
  summarise(k = sum(chose_repeated), n = n(), .groups = "drop") |>
  mutate(p = k / n) |>
  beta_ci()
conds12 <- as.data.frame(select(obs12, n_referents, interval, trial_type))
obs3 <- yurovsky_frank2015$exp3 |>
  mutate(trial_type = as.character(trial_type)) |>
  group_by(n_referents, trial_type) |>
  summarise(target = sum(correct), competitor = sum(familiar), n = n(), .groups = "drop") |>
  pivot_longer(c(target, competitor), names_to = "choice", values_to = "k") |>
  mutate(p = k / n) |>
  beta_ci()
conds3 <- as.data.frame(distinct(obs3, n_referents, trial_type))

paper <- list(sigma = .604, lambda = .184, gamma = 1.495)
pred_paper <- mutate(conds12, model = PAPER, lapse = NA_character_,
                     pred = yf2015_predict(paper, conds12)$p_repeated)

# ---- set 1: trial-level yf2015 models ----
group_par <- \(m) unname(prog[[m]]$group$best$optim$bestmem)
exp1 <- list(sigma = .568, lambda = .167, gamma = 1.617)
yf_models <- list(
  "Trial-level, Exp 1 fit (mu = 1.41)" = do.call(yf2015, c(exp1, mu = 1.41)),
  "yf2015_g (xsl_datasets, gamma = 1.62)" = specs_yf2015$yf2015_g$constructor(group_par("yf2015_g")),
  "yf2015 (xsl_datasets, gamma free)" = specs_yf2015$yf2015$constructor(group_par("yf2015"))
)
yf12 <- bind_rows(pred_paper, bind_rows(lapply(names(yf_models), \(m)
  mutate(conds12, model = m, lapse = "no lapse", pred = yf2015_paradigm(yf_models[[m]], conds12)$p_target))))
yf3 <- bind_rows(lapply(names(yf_models), \(m) {
  out <- yf2015_paradigm(yf_models[[m]], conds3)
  mutate(conds3, model = m, lapse = "no lapse", target = out$p_target, competitor = out$p_competitor)
})) |> pivot_longer(c(target, competitor), names_to = "choice", values_to = "pred")

# at the paper's own parameters, the trial-level model is the paper's model
same_pars <- yf2015_paradigm(do.call(yf2015, paper), conds12)$p_target
ss <- conds12$trial_type != "New Label"
cat(sprintf("max |trial-level - paper| on Same/Switch at the paper's parameters: %.2g\n",
            max(abs(same_pars[ss] - pred_paper$pred[ss]))))

# ---- set 2: other bake-off models, from score_yf_all.R's cache ----
assoc <- c("fazly", "uncfam_gamma", "kachergis_sampling")
scores <- readRDS(file.path(DIR, "yf_scores_summary.rds"))
with_lapse <- \(p, eps, n_ref) (1 - eps) * p + eps / n_ref
as12 <- lapply(assoc, \(m) {
  x <- readRDS(file.path(DIR, "yf_scores", paste0(m, ".rds")))
  eps <- max(scores$lapse[scores$model == m])
  e12 <- as.data.frame(x$pred$e12)
  e3 <- as.data.frame(x$pred$e3)
  list(e12 = bind_rows(
         mutate(e12, model = m, lapse = LAPSE[1], pred = with_lapse(p_target, eps, n_referents)),
         mutate(e12, model = m, lapse = LAPSE[2], pred = p_target)),
       e3 = bind_rows(
         mutate(e3, model = m, lapse = LAPSE[1],
                target = with_lapse(p_target, eps, n_referents),
                competitor = with_lapse(p_competitor, eps, n_referents)),
         mutate(e3, model = m, lapse = LAPSE[2], target = p_target, competitor = p_competitor)),
       eps = tibble(model = m, eps = eps))
})
as12 <- lapply(c(e12 = "e12", e3 = "e3", eps = "eps"), \(k) bind_rows(lapply(as12, `[[`, k)))
assoc12 <- bind_rows(pred_paper, select(as12$e12, n_referents, interval, trial_type, model, lapse, pred))
assoc3 <- as12$e3 |>
  select(n_referents, trial_type, model, lapse, target, competitor) |>
  pivot_longer(c(target, competitor), names_to = "choice", values_to = "pred")
print(as12$eps, digits = 3)

# ---- fit statistics ----
stats12 <- function(pred) pred |>
  left_join(obs12, by = c("n_referents", "interval", "trial_type")) |>
  group_by(model, lapse = coalesce(lapse, "no lapse")) |>
  summarise(e12_r2 = cor(p, pred)^2, e12_rmse = sqrt(mean((p - pred)^2)),
            e12_loglik = sum(dbinom(k, n, clamp(pred), log = TRUE)), .groups = "drop")
stats3 <- function(pred) pred |>
  left_join(obs3, by = c("n_referents", "trial_type", "choice")) |>
  group_by(model, lapse) |>
  summarise(e3_r = cor(p, pred), e3_rmse = sqrt(mean((p - pred)^2)), .groups = "drop")
fit_stats <- bind_rows(
  left_join(stats12(yf12), stats3(yf3), by = c("model", "lapse")),
  left_join(stats12(filter(assoc12, model != PAPER)), stats3(assoc3), by = c("model", "lapse")))
print(as.data.frame(fit_stats), digits = 3)

# ---- plotting ----
trial_lev <- c("Same", "Switch", "New Label")
lab12 <- \(d) mutate(d, trial_type = factor(trial_type, trial_lev),
                     n_referents = factor(paste(n_referents, "referents"),
                                          paste(c(2, 3, 4, 8), "referents")))
lab3 <- \(d) mutate(d, choice = factor(choice, c("target", "competitor"),
                                       c("Chose target", "Chose familiar competitor")),
                    n_referents = factor(paste(n_referents, "referents")))
chance12 <- lab12(distinct(obs12, n_referents, trial_type) |> mutate(chance = 1 / n_referents))
chance3 <- lab3(distinct(obs3, n_referents, choice) |> mutate(chance = 1 / n_referents))

plot12 <- function(pred, cols, title, subtitle, file, labels = names(cols)) {
  pred <- mutate(pred, model = factor(model, names(cols)))
  # solid lines for the paper's model, and for every model when no lapse
  # variants are compared
  solid <- is.na(pred$lapse) | n_distinct(pred$lapse, na.rm = TRUE) == 1
  ref <- pred[solid, ]
  pred <- mutate(pred[!solid, ], lapse = factor(lapse, LAPSE))
  g <- ggplot(lab12(obs12), aes(interval, p)) +
    geom_hline(data = chance12, aes(yintercept = chance), linetype = "dotted", colour = "grey55") +
    geom_line(data = lab12(ref), aes(y = pred, colour = model), linewidth = .8) +
    geom_line(data = lab12(pred), aes(y = pred, colour = model, linetype = lapse), linewidth = .8) +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = .25, colour = "black") +
    facet_grid(trial_type ~ n_referents) +
    scale_x_continuous(trans = "log2", breaks = c(1, 2, 4, 8)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .25)) +
    scale_colour_manual(values = cols, labels = labels) +
    scale_linetype_manual(values = c("fitted lapse" = "solid", "no lapse" = "dashed"),
                          drop = TRUE, guide = if (n_distinct(pred$lapse) > 1) "legend" else "none") +
    labs(x = "Interval (trials)", y = "P(choose repeated / correct object)", colour = NULL,
         linetype = NULL, title = title, subtitle = subtitle) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom", legend.direction = "vertical")
  ggsave(file.path(DIR, file), g, width = 10, height = 8.8, dpi = 150)
}
plot3 <- function(pred, cols, title, subtitle, file) {
  pred <- mutate(pred, model = factor(model, names(cols)), lapse = factor(lapse, LAPSE))
  g <- ggplot(lab3(obs3), aes(trial_type, p)) +
    geom_col(fill = "grey80", width = .75) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = .2) +
    geom_hline(data = chance3, aes(yintercept = chance), linetype = "dotted", colour = "grey40") +
    geom_point(data = lab3(pred), aes(y = pred, colour = model, shape = lapse),
               position = position_dodge(width = .6), size = 2.4, stroke = 1) +
    facet_grid(choice ~ n_referents) +
    scale_colour_manual(values = cols) +
    scale_shape_manual(values = c("fitted lapse" = 16, "no lapse" = 1),
                       guide = if (n_distinct(pred$lapse) > 1) "legend" else "none") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Target vs. competitor (exposure history)", y = "Proportion of test choices",
         colour = NULL, shape = NULL, title = title, subtitle = subtitle) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom", legend.direction = "vertical",
          axis.text.x = element_text(angle = 20, hjust = 1))
  ggsave(file.path(DIR, file), g, width = 9, height = 7, dpi = 150)
}
r2_lab <- \(ms) vapply(ms, \(m) {
  s <- filter(fit_stats, model == m, lapse == "no lapse")
  sprintf("%s  (Exps 1-2 r² = %.2f)", m, s$e12_r2)
}, "")

# set 1
cols1 <- setNames(c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"), c(PAPER, names(yf_models)))
plot12(yf12, cols1, "Yurovsky & Frank (2015), Experiments 1-2: trial-level YF model",
       "Points: human means with 95% intervals (Same/Switch: Exps 1-2 pooled; New Label: Exp 2). Lines: models.",
       "yf_models_exp12.png", labels = r2_lab(names(cols1)))
plot3(yf3, cols1[-1], "Yurovsky & Frank (2015), Experiment 3: trial-level YF model",
      "Bars: human means with 95% intervals. Points: trial-level models (the paper has no Exp 3 model).",
      "yf_models_exp3.png")

# set 2
cols2 <- setNames(c("#1b9e77", "#e6ab02", "#1f78b4", "#a6761d"), c(PAPER, assoc))
eps_txt <- paste(sprintf("%s %.2f", as12$eps$model, as12$eps$eps), collapse = ", ")
plot12(assoc12, cols2, "Yurovsky & Frank (2015), Experiments 1-2: fazly, uncfam_gamma, kachergis_sampling",
       paste0("xsl_datasets group fits; all three give identical predictions here, so their lines overlap.\n",
              "Solid: with lapse fit to Exp 1 (", eps_txt, "); dashed: no lapse.\n",
              "Points: human means with 95% intervals. The paper's model is shown for reference."),
       "yf_models_assoc_exp12.png")
plot3(assoc3, cols2[-1], "Yurovsky & Frank (2015), Experiment 3: fazly, uncfam_gamma, kachergis_sampling",
      paste0("xsl_datasets group fits. Bars: human means with 95% intervals.\n",
             "Filled: with lapse fit to Exp 1 (", eps_txt, "); open: no lapse."),
      "yf_models_assoc_exp3.png")

saveRDS(list(obs12 = obs12, obs3 = obs3, yf12 = yf12, yf3 = yf3, assoc12 = assoc12, assoc3 = assoc3,
             lapse = as12$eps, fit_stats = fit_stats),
        file.path(DIR, "yf_models_predictions.rds"))
