# Stage 3, part 2: mimicry and qualitative signatures across parameter space,
# from stage3_sweep.R's parameter clouds.
#
# Three prediction sets, each on the chance-corrected scale of stages 1-2,
# (p - 1/N) / (1 - 1/N):
#   filtering  the 12 filtering conditions of xsl_datasets (288 items)
#   other xsl  the other 35 xsl_datasets conditions (636 items)
#   YF         Yurovsky & Frank (2015), 64 cells (Exp 1-2 p_target, Exp 3
#              target and competitor)
# On YF a lapse eps mixes a response with a uniform guess; on the
# chance-corrected scale it multiplies the prediction vector by (1 - eps).
# Every YF point of a generating model gets a lapse drawn from U(0, .5); a
# mimicking model may use any lapse (the best scaling s = 1 - eps in [0, 1]).
#
# Mimicry. For a generating model A and a mimicking model B, each point of
# A's cloud is matched to the nearest point of B's (RMSD; on YF with B's best
# lapse). Reported per pair:
#   - the median nearest distance, and
#   - the share of A's points that B matches within tolerance, the human
#     sampling-noise RMSD of that set (a perfect model's expected distance
#     from the human means): B mimics A as closely as an experiment of this
#     size could tell.
# Simulation noise of stochastic models (estimated from points run twice
# with independent seeds) is reported but not removed: it varies by orders of
# magnitude across a model's parameter space (guess-and-test is noiseless
# where it is perfect and very noisy where it isn't), so no single correction
# fits. It inflates distances involving the noisiest models. For A = B the
# nearest point excludes the point itself, so the diagonal shows how densely
# the cloud samples the model's own behaviour.
#
# Humans. Each model's nearest cloud point to the human data on each set, and
# jointly (the point minimizing the sum of the three squared distances in units
# of each set's noise floor), against its group fit (point 0).
#
# Signatures. Ordinal patterns of the human data (a difference counts if it
# exceeds DELTA = .05 chance-corrected; otherwise "none"), and for each model
# the share of its parameter space that produces the human pattern, without a
# lapse (a lapse rescales but can't reverse a difference):
#   YF:  Same > Switch; Switch > chance; Same falls with interval (1 vs 8);
#        New Label vs chance; Exp 3 target > competitor; Exp 3 target
#        higher when it was chosen at exposure (Same) than not (Switch)
#   filtering (conditions with extra exposures, E > 0): partner-object
#        accuracy below half of own-object accuracy (an ordinal own > partner
#        is too weak: co-occurrence counting gives it too, by .1); partner
#        choice change from 3E to 9E; own-object accuracy change from 3L to 9L
#
# Usage (from xsl-bakeoff/):
#   R_LIBS=$(pwd)/fits/v06/yf2015/Rlib Rscript fits/v06/clusters/stage3_analyse.R
# Output: fits/v06/clusters/stage3_*.png, stage3.rds

suppressPackageStartupMessages({
  library(XSLmodels)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
})
DIR <- "fits/v06/clusters"
CLOUDS <- Sys.getenv("CLOUDS", file.path(DIR, "stage3_clouds"))
YF <- "fits/v06/yf2015"
DELTA <- .05
norm_acc <- \(p, n_alt) (p - 1 / n_alt) / (1 - 1 / n_alt)
HUMAN <- "HUMAN"
family_of <- function(m) case_when(
  m == HUMAN ~ "humans",
  m %in% c("trueswell2012", "guess-and-test", "pursuit") ~ "hypothesis testing",
  m %in% c("rescorla-wagner_softmax", "rescorla-wagner_discriminative", "kalman_filter_full",
           "baseline_softmax", "softmax_rl") ~ "associative, softmax",
  m == "kachergis_sampling" ~ "associative, sampled attention",
  m == "minerva2" ~ "memory traces",
  grepl("^yf2015", m) ~ "YF (hypothesis + traces)",
  TRUE ~ "associative, Luce")
fam_cols <- c("humans" = "black", "hypothesis testing" = "#d95f02", "associative, Luce" = "#1b9e77",
              "associative, softmax" = "#7570b3", "associative, sampled attention" = "#66a61e",
              "memory traces" = "#e6ab02", "YF (hypothesis + traces)" = "#e7298a")
SETS <- c("filtering", "other xsl", "YF")

## ---- item and cell metadata, human data -------------------------------------------------
xsl_items <- map(xsl_datasets, function(d) {
  n_alt <- if (length(d$test) && length(d$test$words)) lengths(d$test$objects)
           else rep(length(unique(unlist(d$train$objects))), length(d$accuracy))
  tibble(condnum = d$label, item = seq_along(d$accuracy), n_alt = n_alt, human = unname(d$accuracy),
         n = d$n_subj)
}) |> list_rbind() |>
  mutate(set = if_else(grepl("^filt", condnum), "filtering", "other xsl"),
         E = as.integer(sub("^filt(\\d)E_.*", "\\1", if_else(set == "filtering", condnum, NA))),
         L = as.integer(sub("^filt\\dE_(\\d)L$", "\\1", if_else(set == "filtering", condnum, NA))),
         filt_item = if_else(set == "filtering", if_else(item <= 12, "own", "partner"), NA))
# YF cells in score_model()'s order: Exp 1-2 rows, then Exp 3 targets, then competitors
tmpl <- readRDS(file.path(YF, "yf_scores", "baseline.rds"))$pred
yf_cells <- bind_rows(
  as.data.frame(tmpl$e12) |> transmute(n_referents, interval, trial_type, choice = "target"),
  as.data.frame(tmpl$e3) |> transmute(n_referents, interval = NA, trial_type, choice = "target"),
  as.data.frame(tmpl$e3) |> transmute(n_referents, interval = NA, trial_type, choice = "competitor")) |>
  mutate(exp3 = is.na(interval))
obs12 <- yurovsky_frank2015$exp12 |> mutate(trial_type = as.character(trial_type)) |>
  group_by(n_referents, interval, trial_type) |>
  summarise(target = mean(chose_repeated), n = n(), .groups = "drop") |>
  pivot_longer(target, names_to = "choice", values_to = "human")
obs3 <- yurovsky_frank2015$exp3 |> mutate(trial_type = as.character(trial_type)) |>
  group_by(n_referents, trial_type) |>
  summarise(target = mean(correct), competitor = mean(familiar), n = n(), .groups = "drop") |>
  pivot_longer(c(target, competitor), names_to = "choice", values_to = "human") |> mutate(interval = NA)
yf_cells <- yf_cells |> left_join(bind_rows(obs12, obs3), by = c("n_referents", "interval", "trial_type", "choice"))
stopifnot(!anyNA(yf_cells$human), nrow(yf_cells) == 64)

meta <- list(filtering = filter(xsl_items, set == "filtering"),
             `other xsl` = filter(xsl_items, set == "other xsl"),
             YF = yf_cells |> mutate(n_alt = n_referents))
idx <- list(filtering = which(xsl_items$set == "filtering"), `other xsl` = which(xsl_items$set == "other xsl"))
human <- map(meta, \(m) norm_acc(m$human, m$n_alt))
floor_rmsd <- map_dbl(meta, \(m) sqrt(mean(m$human * (1 - m$human) / m$n / (1 - 1 / m$n_alt)^2)))
cat("human noise floor (RMSD):", sprintf("%s %.3f", names(floor_rmsd), floor_rmsd), "\n")

## ---- clouds -----------------------------------------------------------------------------
files <- list.files(CLOUDS, "\\.rds$", full.names = TRUE)
chunks <- map(files, readRDS)
models <- unique(map_chr(chunks, "model"))
to_sets <- function(x_raw, y_raw) list(
  filtering = norm_acc(x_raw[, idx$filtering, drop = FALSE], rep(meta$filtering$n_alt, each = nrow(x_raw))),
  `other xsl` = norm_acc(x_raw[, idx$`other xsl`, drop = FALSE], rep(meta$`other xsl`$n_alt, each = nrow(x_raw))),
  YF = norm_acc(y_raw, rep(meta$YF$n_alt, each = nrow(y_raw))))
clouds <- map(set_names(models), function(m) {
  pts <- chunks[map_chr(chunks, "model") == m] |> map("points") |> list_flatten()
  pts <- pts[order(map_int(pts, "point"))]
  X <- do.call(rbind, map(pts, "xsl")); Y <- do.call(rbind, map(pts, "yf"))
  ok <- rowSums(is.na(X)) == 0 & rowSums(is.na(Y)) == 0
  S <- to_sets(X[ok, , drop = FALSE], Y[ok, , drop = FALSE])
  # simulation noise: half the mean squared RMSD between replicate runs of a point
  rep_pts <- keep(pts, \(p) !is.null(p$rep_xsl) && !anyNA(p$rep_xsl) && !anyNA(p$rep_yf))
  noise <- set_names(rep(0, 3), SETS)
  if (length(rep_pts)) {
    R1 <- to_sets(do.call(rbind, map(rep_pts, "xsl")), do.call(rbind, map(rep_pts, "yf")))
    R2 <- to_sets(do.call(rbind, map(rep_pts, "rep_xsl")), do.call(rbind, map(rep_pts, "rep_yf")))
    noise <- map_dbl(set_names(SETS), \(s) mean(rowMeans((R1[[s]] - R2[[s]])^2)) / 2)
  }
  list(model = m, point = map_int(pts, "point")[ok], par = do.call(rbind, map(pts, "par"))[ok, , drop = FALSE],
       sets = S, noise2 = noise, n_failed = sum(!ok))
})
walk(clouds, \(c) if (c$n_failed) cat(sprintf("%s: %d points failed\n", c$model, c$n_failed)))
cat("simulation noise (RMSD of one run from its expectation):\n")
print(round(sqrt(do.call(rbind, map(clouds, "noise2"))[map_dbl(clouds, \(c) sum(c$noise2)) > 0, , drop = FALSE]), 3))

# a lapse for each YF point of a generating model (point 0, the group fit, keeps none)
set.seed(3)
for (m in models) {
  eps <- runif(length(clouds[[m]]$point), 0, .5); eps[clouds[[m]]$point == 0] <- 0
  clouds[[m]]$yf_gen <- clouds[[m]]$sets$YF * (1 - eps)
}

## ---- distances ------------------------------------------------------------------------
# squared RMSD between every row of A and every row of B; with `scale`, B's
# rows may be multiplied by the s in [0, 1] closest to A's (a lapse)
sqdist <- function(A, B, scale = FALSE) {
  aa <- rowSums(A^2); bb <- rowSums(B^2); AB <- A %*% t(B)
  if (!scale) return(pmax(outer(aa, bb, "+") - 2 * AB, 0) / ncol(A))
  s <- sweep(AB, 2, pmax(bb, 1e-12), "/") |> pmax(0) |> pmin(1)
  pmax(aa - 2 * s * AB + sweep(s^2, 2, bb, "*"), 0) / ncol(A)
}
gen_mat <- \(m, s) if (s == "YF") clouds[[m]]$yf_gen else clouds[[m]]$sets[[s]]
nearest <- function(a, b, s) {
  d2 <- sqdist(gen_mat(a, s), clouds[[b]]$sets[[s]], scale = s == "YF")
  if (a == b) diag(d2) <- Inf
  sqrt(apply(d2, 1, min))
}
pairs <- expand_grid(set = SETS, gen = models, mimic = models)
mimicry <- pairs |>
  mutate(d = pmap(list(gen, mimic, set), \(a, b, s) if (length(clouds[[a]]$point) > 1 || a != b) nearest(a, b, s) else NA_real_)) |>
  mutate(median_d = map_dbl(d, median), share = map2_dbl(d, set, \(x, s) mean(x <= floor_rmsd[[s]]))) |>
  select(-d)

# symmetric dissimilarity for clustering: mean of the two directions' median distances
sym_D <- function(s) {
  M <- mimicry |> filter(set == s) |> select(gen, mimic, median_d) |>
    pivot_wider(names_from = mimic, values_from = median_d) |> tibble::column_to_rownames("gen") |> as.matrix()
  M <- M[models, models]
  D <- (M + t(M)) / 2; diag(D) <- 0; D[is.na(D)] <- 0
  D
}
Dsym <- map(set_names(SETS), sym_D)

## ---- humans ---------------------------------------------------------------------------
to_h <- function(m, s) {
  h <- matrix(human[[s]], nrow = 1)
  sqrt(sqdist(h, clouds[[m]]$sets[[s]], scale = s == "YF"))[1, ]
}
human_fit <- map(models, function(m) {
  d <- map(set_names(SETS), \(s) to_h(m, s))
  z <- do.call(cbind, map(SETS, \(s) d[[s]] / floor_rmsd[[s]]))
  j <- which.min(rowSums(z^2)); g0 <- which(clouds[[m]]$point == 0)
  map(SETS, \(s) tibble(model = m, set = s, group_fit = d[[s]][g0], best = min(d[[s]]),
                        joint = d[[s]][j], joint_point = clouds[[m]]$point[j])) |> list_rbind()
}) |> list_rbind()
human_points <- map(models, \(m) tibble(model = m, point = clouds[[m]]$point,
                                        filtering = to_h(m, "filtering"), other = to_h(m, "other xsl"),
                                        YF = to_h(m, "YF"))) |> list_rbind()

## ---- signatures -----------------------------------------------------------------------
sgn <- \(x) case_when(x > DELTA ~ ">", x < -DELTA ~ "<", TRUE ~ "≈")
yf_sig <- function(v) {  # v: chance-corrected YF vector(s), one per row
  v <- matrix(v, ncol = 64); c12 <- meta$YF[!meta$YF$exp3, ]; c3 <- meta$YF[meta$YF$exp3, ]
  V12 <- v[, !meta$YF$exp3, drop = FALSE]; V3 <- v[, meta$YF$exp3, drop = FALSE]
  m12 <- \(sel) rowMeans(V12[, sel, drop = FALSE])
  m3 <- \(sel) rowMeans(V3[, sel, drop = FALSE])
  tibble(`YF: Same vs Switch` = sgn(m12(c12$trial_type == "Same") - m12(c12$trial_type == "Switch")),
         `YF: Switch vs chance` = sgn(m12(c12$trial_type == "Switch")),
         `YF: Same, interval 1 vs 8` = sgn(m12(c12$trial_type == "Same" & c12$interval == 1) -
                                             m12(c12$trial_type == "Same" & c12$interval == 8)),
         `YF: New Label vs chance` = sgn(m12(c12$trial_type == "New Label")),
         `YF E3: target vs competitor` = sgn(m3(c3$choice == "target") - m3(c3$choice == "competitor")),
         `YF E3: target if chosen vs not` = sgn(m3(c3$choice == "target" & grepl("^Same", c3$trial_type)) -
                                                  m3(c3$choice == "target" & grepl("^Switch", c3$trial_type))))
}
filt_sig <- function(v) {
  v <- matrix(v, ncol = nrow(meta$filtering)); f <- meta$filtering
  mm <- \(sel) rowMeans(v[, sel, drop = FALSE])
  own <- mm(f$E > 0 & f$filt_item == "own"); partner <- mm(f$E > 0 & f$filt_item == "partner")
  tibble(`filtering: partner / own object` = if_else(partner < own / 2, "< 1/2", ">= 1/2"),
         `filtering: partner, 9E vs 3E` = sgn(mm(f$E == 9 & f$filt_item == "partner") - mm(f$E == 3 & f$filt_item == "partner")),
         `filtering: own, 9L vs 3L` = sgn(mm(f$E > 0 & f$L == 9 & f$filt_item == "own") - mm(f$E > 0 & f$L == 3 & f$filt_item == "own")))
}
human_sig <- bind_cols(filt_sig(human$filtering), yf_sig(human$YF))
cat("human signatures:\n"); print(t(human_sig))
sig_long <- map(models, function(m) {
  s <- bind_cols(filt_sig(clouds[[m]]$sets$filtering), yf_sig(clouds[[m]]$sets$YF))
  hit <- as.matrix(s) == matrix(unlist(human_sig), nrow(s), ncol(s), byrow = TRUE)
  bind_rows(tibble(model = m, signature = names(s), share = colMeans(hit)),
            tibble(model = m, signature = c("all filtering", "all YF", "all nine"),
                   share = c(mean(rowSums(hit[, 1:3, drop = FALSE]) == 3), mean(rowSums(hit[, 4:9, drop = FALSE]) == 6),
                             mean(rowSums(hit) == 9))))
}) |> list_rbind()
# which patterns each model can produce at all
patterns <- map(models, function(m) {
  bind_cols(filt_sig(clouds[[m]]$sets$filtering), yf_sig(clouds[[m]]$sets$YF)) |>
    pivot_longer(everything(), names_to = "signature", values_to = "pattern") |>
    count(signature, pattern) |> mutate(model = m, share = n / sum(n) * n_distinct(signature))
}) |> list_rbind()

## ---- figures --------------------------------------------------------------------------
model_order <- tibble(model = models, family = family_of(models)) |>
  mutate(family = factor(family, names(fam_cols))) |> arrange(family, model) |> pull(model)
lab_col <- fam_cols[family_of(model_order)]

p_mim <- mimicry |>
  mutate(gen = factor(gen, rev(model_order)), mimic = factor(mimic, model_order), set = factor(set, SETS)) |>
  ggplot(aes(mimic, gen, fill = share)) + geom_tile(colour = "white", linewidth = .2) +
  geom_text(aes(label = ifelse(share >= .05, sprintf("%.0f", 100 * share), "")), size = 1.9) +
  facet_wrap(~set, ncol = 1) +
  scale_fill_gradient(low = "white", high = "#08519c", limits = c(0, 1), labels = scales::percent) +
  labs(x = "mimicking model", y = "generating model",
       fill = "share of the generating\nmodel's points matched\nwithin human noise",
       title = "Mimicry across parameter space",
       subtitle = "Row model A, column model B: % of A's parameter points that some point of B predicts within the human sampling noise of the set") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, colour = lab_col),
        axis.text.y = element_text(colour = rev(lab_col)), panel.grid = element_blank())
ggsave(file.path(DIR, "stage3_mimicry.png"), p_mim, width = 11, height = 26, dpi = 150)

dendro_plot <- function(Dm, title) {
  hc <- hclust(as.dist(Dm), method = "average")
  dd <- dendextend::as.ggdend(as.dendrogram(hc))
  lab <- dd$labels |> mutate(family = family_of(label))
  ggplot() +
    geom_segment(data = dd$segments, aes(x, y, xend = xend, yend = yend), colour = "grey40") +
    geom_text(data = lab, aes(x, y - .005, label = label, colour = family), angle = 90, hjust = 1,
              size = 2.9, show.legend = FALSE) +
    geom_point(data = lab, aes(x, 0, colour = family), size = 0, alpha = 0) +
    scale_colour_manual(values = fam_cols, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(.9, .03))) +
    labs(title = title, x = NULL, y = "median nearest-point RMSD\n(mean of both directions)", colour = NULL) +
    guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
}
p_den <- wrap_plots(imap(Dsym, \(D, s) dendro_plot(D, sprintf("%s (tolerance %.3f)", s, floor_rmsd[[s]]))), ncol = 1) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(file.path(DIR, "stage3_dendrograms.png"), p_den, width = 11, height = 13, dpi = 150)

p_hum <- human_fit |>
  pivot_longer(c(group_fit, best, joint), names_to = "which", values_to = "rmsd") |>
  mutate(which = factor(recode(which, group_fit = "group fit (xsl_datasets SSE)", best = "best point for this set",
                               joint = "best point for all three sets"),
                        c("group fit (xsl_datasets SSE)", "best point for this set", "best point for all three sets")),
         model = factor(model, rev(model_order)), set = factor(set, SETS)) |>
  ggplot(aes(rmsd, model, colour = which, shape = which)) +
  geom_vline(data = tibble(set = factor(SETS, SETS), f = floor_rmsd), aes(xintercept = f), linetype = "dotted") +
  geom_point(size = 2) +
  facet_wrap(~set, scales = "free_x") +
  scale_colour_manual(values = c("grey50", "#d95f02", "#1b9e77")) +
  labs(x = "RMSD to humans (chance-corrected; YF with the best lapse)", y = NULL, colour = NULL, shape = NULL,
       title = "How close can each model get to humans?",
       subtitle = "Dotted: human sampling noise. Best points are the nearest in the model's parameter cloud") +
  theme_minimal(base_size = 9) + theme(legend.position = "bottom", axis.text.y = element_text(colour = rev(lab_col)))
ggsave(file.path(DIR, "stage3_humans.png"), p_hum, width = 11, height = 8, dpi = 150)

p_trade <- human_points |>
  mutate(model = factor(model, model_order), g0 = point == 0) |>
  ggplot(aes(filtering, YF)) +
  geom_point(aes(colour = family_of(as.character(model))), size = .5, alpha = .4, show.legend = FALSE) +
  geom_point(data = \(d) filter(d, g0), shape = 4, size = 2.5, stroke = 1) +
  geom_vline(xintercept = floor_rmsd[["filtering"]], linetype = "dotted") +
  geom_hline(yintercept = floor_rmsd[["YF"]], linetype = "dotted") +
  facet_wrap(~model, ncol = 6) +
  scale_colour_manual(values = fam_cols) +
  coord_cartesian(xlim = c(0, .8), ylim = c(0, .6)) +
  labs(x = "RMSD to humans, filtering conditions", y = "RMSD to humans, YF (best lapse)",
       title = "Each model's parameter points: closeness to humans on filtering vs. YF",
       subtitle = "×: group fit. Dotted: human sampling noise. Points near the origin would fit both") +
  theme_minimal(base_size = 9)
ggsave(file.path(DIR, "stage3_tradeoff.png"), p_trade, width = 12, height = 11, dpi = 150)

sig_names <- c(names(human_sig), "all filtering", "all YF", "all nine")
sig_lab <- \(x) ifelse(x %in% names(human_sig), paste0(x, ": ", unlist(human_sig)[x]), x)
p_sig <- sig_long |>
  mutate(model = factor(model, rev(model_order)), signature = factor(sig_lab(signature), sig_lab(sig_names))) |>
  ggplot(aes(signature, model, fill = share)) + geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.0f", 100 * share)), size = 2.3) +
  scale_fill_gradient(low = "white", high = "#a63603", limits = c(0, 1), labels = scales::percent) +
  labs(x = NULL, y = NULL, fill = "share of\nparameter\nspace",
       title = "Which models can produce the human pattern, and how much of their parameter space does",
       subtitle = sprintf("Column labels give the human pattern: a difference of more than %.2f chance-corrected (≈ otherwise), or the partner / own accuracy ratio", DELTA)) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), axis.text.y = element_text(colour = rev(lab_col)),
        panel.grid = element_blank())
ggsave(file.path(DIR, "stage3_signatures.png"), p_sig, width = 11, height = 8.5, dpi = 150)

## ---- summaries ------------------------------------------------------------------------
flex <- mimicry |> filter(gen != mimic) |> group_by(set, model = mimic) |>
  summarise(mimics = mean(share), .groups = "drop") |>
  left_join(mimicry |> filter(gen != mimic) |> group_by(set, model = gen) |>
              summarise(mimicked = mean(share), .groups = "drop"), by = c("set", "model")) |>
  left_join(mimicry |> filter(gen == mimic) |> transmute(set, model = gen, self_d = median_d), by = c("set", "model")) |>
  left_join(map(models, \(m) map(SETS, \(s) {
    Z <- clouds[[m]]$sets[[s]]
    tibble(model = m, set = s, spread = if (nrow(Z) > 1) median(sqrt(sqdist(Z, Z)[upper.tri(diag(nrow(Z)))])) else 0)
  }) |> list_rbind()) |> list_rbind(), by = c("set", "model"))
cat("\nflexibility: mimics = mean share of other models' points it matches; mimicked = mean share of its points others match;\n",
    "self_d = median nearest distance within its own cloud; spread = median pairwise distance in its cloud\n")
print(flex |> select(model, set, mimics, mimicked, spread) |>
        pivot_wider(names_from = set, values_from = c(mimics, mimicked, spread)) |>
        select(model, starts_with("mimics"), starts_with("mimicked"), starts_with("spread")) |>
        arrange(desc(mimics_YF)) |> as.data.frame(), digits = 2)
cat("\nhumans:\n")
print(human_fit |> pivot_wider(names_from = set, values_from = c(group_fit, best, joint)) |> as.data.frame(), digits = 2)
cat("\nsignature shares:\n")
print(sig_long |> pivot_wider(names_from = signature, values_from = share) |> as.data.frame(), digits = 2)

saveRDS(list(floor = floor_rmsd, noise = map(clouds, "noise2"), mimicry = mimicry, Dsym = Dsym,
             human_fit = human_fit, human_points = human_points, human_sig = human_sig,
             signatures = sig_long, patterns = patterns, flex = flex,
             n_points = map_int(clouds, \(c) length(c$point))),
        file.path(DIR, "stage3.rds"))
