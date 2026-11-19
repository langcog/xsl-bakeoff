# Which models, and which experiments, cluster together?
#
# Stage 1 (behaviour): each model is a vector of predictions at its group fit
#   - xsl_datasets: 924 test items in 47 conditions (fits/v05/adults
#     group_fits.Rdata `gfd`, plus the four yf2015 variants run from their
#     fits/v06/yf2015 group fits)
#   - Yurovsky & Frank (2015): 48 Exp 1-2 cells and 16 Exp 3 cells (target
#     and familiar-competitor choices), from score_yf_all.R's cache, without
#     the lapse (the model's own behaviour)
#   Accuracy is put on a chance-corrected scale, (p - 1/N) / (1 - 1/N), so
#   that 0 is chance and 1 is perfect whatever the number of alternatives N.
#   Distances are root-mean-square differences (RMSD) on that scale, with
#   humans included as one more "model". Average-linkage clustering and
#   classical MDS, on xsl_datasets, on YF, and on both (mean of the two
#   squared distances).
#
# Stage 2 (fit): residuals (model - human), on the same scale
#   - per condition x model mean residual, clustered both ways (YF with each
#     model's lapse fit to Exp 1, as scored)
#   - per condition, models ranked by RMSE; conditions clustered by those ranks
#   - xsl_datasets items: shared misfit (median residual over models) and the
#     principal components of the item-centred predictions (what separates
#     models), with humans projected onto them; both related to item and
#     condition design features
#
# Usage (from xsl-bakeoff/):
#   R_LIBS=$(pwd)/fits/v06/yf2015/Rlib Rscript fits/v06/clusters/cluster_models.R
# Output: fits/v06/clusters/*.png and clusters.rds

suppressPackageStartupMessages({
  library(XSLmodels)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
})

DIR <- "fits/v06/clusters"
YF <- "fits/v06/yf2015"
dir.create(DIR, showWarnings = FALSE)
HUMAN <- "HUMAN"
norm_acc <- \(p, n_alt) (p - 1 / n_alt) / (1 - 1 / n_alt)

# ---- a priori model families (for colouring only) ----
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

# ---- conditions and items of xsl_datasets ----
cond_family <- function(label) case_when(
  grepl("^2[0-2][0-9]$", label) ~ "2xx grid designs",
  label == "301" ~ "301 (2x2, 24AFC)",
  grepl("^filt", label) ~ "filtering",
  grepl("x8|freq369|cont_div", label) ~ "frequency / CD",
  TRUE ~ "contiguity / overlap")
items <- bind_rows(lapply(xsl_datasets, function(d) {
  tr <- d$train
  all_words <- sort(unique(unlist(tr$words)))
  all_objs <- unique(unlist(tr$objects))
  has_test <- length(d$test) > 0 && length(d$test$words) > 0
  item_word <- if (has_test) vapply(d$test$words, \(w) w[1], numeric(1)) else all_words[seq_along(d$accuracy)]
  n_alt <- if (has_test) lengths(d$test$objects) else rep(length(all_objs), length(d$accuracy))
  n_tr <- length(tr$words)
  feat <- lapply(item_word, function(w) {
    on <- which(vapply(tr$words, \(x) w %in% x, TRUE))
    co <- unique(unlist(tr$words[on]))
    c(freq = length(on), cd = length(setdiff(co, w)),
      recency = if (length(on)) n_tr - max(on) else NA, first = if (length(on)) min(on) else NA)
  })
  tibble(condnum = d$label, condition = d$condition, cond_family = cond_family(d$label),
         item = seq_along(d$accuracy), word = item_word, n_subj = d$n_subj, n_alt = n_alt,
         human = unname(d$accuracy), n_trials = n_tr,
         words_per_trial = mean(lengths(tr$words)), objs_per_trial = mean(lengths(tr$objects)),
         n_words = length(all_words), n_objs = length(all_objs)) |>
    bind_cols(as_tibble(do.call(rbind, feat)))
}))

# ---- predictions on xsl_datasets ----
e <- new.env()
load("fits/v05/adults/group_fits.Rdata", e)
gfd <- e$gfd |> group_by(Model, condnum) |> mutate(item = row_number()) |> ungroup() |>
  transmute(model = Model, condnum, item, pred = ModelPerf)
source(file.path(YF, "specs_yf2015.R"))
load(file.path(YF, "yf2015_fits.Rdata"))  # prog
yf_xsl <- bind_rows(lapply(names(prog), function(m) {
  mod <- specs_yf2015[[m]]$constructor(unname(prog[[m]]$group$best$optim$bestmem))
  fits <- xsl_run(mod, xsl_datasets)$fits
  bind_rows(lapply(seq_along(fits), \(i) tibble(model = m, condnum = xsl_datasets[[i]]$label,
                                                 item = seq_along(fits[[i]]$perf),
                                                 pred = unname(fits[[i]]$perf))))
}))
xsl <- bind_rows(gfd, yf_xsl) |>
  left_join(items, by = c("condnum", "item")) |>
  mutate(na_pred = norm_acc(pred, n_alt), na_human = norm_acc(human, n_alt), resid = na_pred - na_human)
stopifnot(!anyNA(xsl$na_pred))

# ---- predictions on Yurovsky & Frank ----
obs12 <- yurovsky_frank2015$exp12 |>
  mutate(trial_type = as.character(trial_type)) |>
  group_by(n_referents, interval, trial_type) |>
  summarise(k = sum(chose_repeated), n = n(), .groups = "drop") |>
  transmute(cell = paste0("E12 ", n_referents, "ref ", trial_type, " i", interval),
            block = paste0("YF E1-2: ", n_referents, " ref, ", trial_type),
            n_referents, trial_type, interval, choice = "target", human = k / n, n)
obs3 <- yurovsky_frank2015$exp3 |>
  mutate(trial_type = as.character(trial_type)) |>
  group_by(n_referents, trial_type) |>
  summarise(target = mean(correct), competitor = mean(familiar), n = n(), .groups = "drop") |>
  pivot_longer(c(target, competitor), names_to = "choice", values_to = "human") |>
  mutate(cell = paste0("E3 ", n_referents, "ref ", trial_type, " ", choice),
         block = paste0("YF E3: ", n_referents, " ref, ", choice), interval = NA)
yf_obs <- bind_rows(obs12, obs3)
scores <- readRDS(file.path(YF, "yf_scores_summary.rds"))
lapse <- scores |> group_by(model) |> summarise(eps = max(lapse))
yf_pred <- bind_rows(lapply(lapse$model, function(m) {
  x <- readRDS(file.path(YF, "yf_scores", paste0(m, ".rds")))$pred
  bind_rows(
    as.data.frame(x$e12) |> transmute(n_referents, interval, trial_type, choice = "target", pred = p_target),
    as.data.frame(x$e3) |> pivot_longer(c(p_target, p_competitor), names_to = "choice", values_to = "pred") |>
      transmute(n_referents, interval = NA, trial_type, choice = sub("p_", "", choice), pred)) |>
    mutate(model = m)
})) |>
  left_join(lapse, by = "model") |>
  mutate(pred_lapse = (1 - eps) * pred + eps / n_referents)
yf <- yf_pred |>
  inner_join(yf_obs, by = c("n_referents", "interval", "trial_type", "choice")) |>
  mutate(na_pred = norm_acc(pred, n_referents), na_lapse = norm_acc(pred_lapse, n_referents),
         na_human = norm_acc(human, n_referents), resid = na_lapse - na_human)
stopifnot(nrow(yf) == nrow(yf_obs) * nrow(lapse))

models <- sort(intersect(unique(xsl$model), unique(yf$model)))
cat(length(models), "models\n")

# =====================================================================
# Stage 1: behaviour
# =====================================================================
wide <- function(d, key, value, human_value) {
  m <- d |> filter(model %in% models) |> select(model, key = all_of(key), v = all_of(value)) |>
    pivot_wider(names_from = key, values_from = v)
  mat <- as.matrix(m[, -1]); rownames(mat) <- m$model
  h <- d |> distinct(key = .data[[key]], h = .data[[human_value]])
  rbind(mat[models, ], HUMAN = h$h[match(colnames(mat), h$key)])  # same row order in every matrix
}
xsl$key <- paste(xsl$condnum, xsl$item)
X_xsl <- wide(xsl, "key", "na_pred", "na_human")
X_yf <- wide(yf, "cell", "na_pred", "na_human")
rmsd <- \(X) as.matrix(dist(X)) / sqrt(ncol(X))
D <- list(xsl_datasets = rmsd(X_xsl), YF = rmsd(X_yf))
stopifnot(identical(rownames(D$xsl_datasets), rownames(D$YF)))
D$combined <- sqrt((D$xsl_datasets^2 + D$YF^2) / 2)

# human sampling noise: the RMSD a perfect model would have to the human means
floor_xsl <- with(distinct(xsl, key, human, n_subj, n_alt),
                  sqrt(mean(human * (1 - human) / n_subj / (1 - 1 / n_alt)^2)))
floor_yf <- with(yf_obs, sqrt(mean(human * (1 - human) / n / (1 - 1 / n_referents)^2)))
cat(sprintf("human noise floor (RMSD): xsl_datasets %.3f, YF %.3f\n", floor_xsl, floor_yf))

# dendrograms, drawn with ggplot from hclust
dendro_plot <- function(Dm, title) {
  hc <- hclust(as.dist(Dm), method = "average")
  dd <- dendextend::as.ggdend(as.dendrogram(hc))
  lab <- dd$labels |> mutate(family = family_of(label))
  ggplot() +
    geom_segment(data = dd$segments, aes(x, y, xend = xend, yend = yend), colour = "grey40") +
    geom_text(data = lab, aes(x, y - .01, label = label, colour = family), angle = 90, hjust = 1,
              size = 2.9, show.legend = FALSE) +
    geom_point(data = lab, aes(x, 0, colour = family), size = 0, alpha = 0) +
    scale_colour_manual(values = fam_cols, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(.9, .03))) +
    labs(title = title, x = NULL, y = "RMSD (chance-corrected accuracy)", colour = NULL) +
    guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1))) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank())
}
p_dendro <- dendro_plot(D$xsl_datasets, "xsl_datasets (924 items)") /
  dendro_plot(D$YF, "Yurovsky & Frank (64 cells, no lapse)") /
  dendro_plot(D$combined, "Both") +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(file.path(DIR, "stage1_dendrograms.png"), p_dendro, width = 11, height = 13, dpi = 150)

mds_df <- bind_rows(lapply(names(D), function(s) {
  xy <- cmdscale(D[[s]], k = 2, eig = TRUE)
  ve <- xy$eig[1:2] / sum(abs(xy$eig))
  tibble(set = sprintf("%s (dims %.0f%% + %.0f%%)", s, 100 * ve[1], 100 * ve[2]),
         model = rownames(xy$points), x = xy$points[, 1], y = xy$points[, 2])
})) |> mutate(family = family_of(model), set = factor(set, unique(set)))
p_mds <- ggplot(mds_df, aes(x, y, colour = family)) +
  geom_point(aes(shape = model == HUMAN), size = 2.6) +
  geom_text_repel(aes(label = model), size = 2.6, max.overlaps = 40, show.legend = FALSE,
                  segment.colour = "grey70") +
  facet_wrap(~set, scales = "free", ncol = 2) +
  scale_colour_manual(values = fam_cols) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 8), guide = "none") +
  labs(x = "MDS 1", y = "MDS 2", colour = NULL,
       title = "Models (and humans) by their predictions, classical MDS on RMSD") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
ggsave(file.path(DIR, "stage1_mds.png"), p_mds, width = 12, height = 11, dpi = 150)

# distance to humans, per data set, against the noise floor
to_human <- tibble(model = models,
                   xsl_datasets = D$xsl_datasets[models, HUMAN], YF = D$YF[models, HUMAN]) |>
  mutate(family = family_of(model))
print(arrange(to_human, xsl_datasets), n = Inf)
p_th <- ggplot(to_human, aes(xsl_datasets, YF, colour = family)) +
  geom_vline(xintercept = floor_xsl, linetype = "dotted") +
  geom_hline(yintercept = floor_yf, linetype = "dotted") +
  geom_point(size = 2.6) +
  geom_text_repel(aes(label = model), size = 2.8, max.overlaps = 40, show.legend = FALSE) +
  scale_colour_manual(values = fam_cols) +
  labs(x = "RMSD to humans, xsl_datasets items", y = "RMSD to humans, YF cells (no lapse)", colour = NULL,
       title = "How far each model's predictions are from human behaviour",
       subtitle = "Dotted: the RMSD a perfect model would have, from human sampling noise alone") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
ggsave(file.path(DIR, "stage1_distance_to_humans.png"), p_th, width = 9, height = 7.5, dpi = 150)

# =====================================================================
# Stage 2: misfit
# =====================================================================
cond_res <- bind_rows(
  xsl |> group_by(model, row = condnum, family = cond_family, label = condition) |>
    summarise(bias = mean(resid), rmse = sqrt(mean(resid^2)), .groups = "drop"),
  yf |> mutate(family = ifelse(grepl("^YF E3", block), "YF Exp 3", "YF Exps 1-2")) |>
    group_by(model, row = block, family, label = block) |>
    summarise(bias = weighted.mean(resid, n), rmse = sqrt(weighted.mean(resid^2, n)), .groups = "drop")
) |> filter(model %in% models)

cluster_order <- function(M, method = "euclidean") {
  d <- if (method == "spearman") as.dist(1 - cor(t(M), method = "spearman")) else dist(M)
  hclust(d, method = "average")$order
}
heat <- function(df, value, fill_scale, title, subtitle, file, row_method = "euclidean") {
  M <- df |> select(row, model, v = all_of(value)) |> pivot_wider(names_from = model, values_from = v)
  Mm <- as.matrix(M[, -1]); rownames(Mm) <- M$row
  ro <- rownames(Mm)[cluster_order(Mm, row_method)]
  co <- colnames(Mm)[cluster_order(t(Mm))]
  rl <- distinct(df, row, family, label) |> mutate(row = factor(row, ro))
  d <- df |> mutate(row = factor(row, ro), model = factor(model, co))
  g <- ggplot(d, aes(model, row, fill = .data[[value]])) +
    geom_tile() +
    geom_tile(data = rl, aes(x = -.2, y = row, colour = family), inherit.aes = FALSE, width = .8,
              fill = NA, linewidth = 2.2) +
    fill_scale +
    scale_y_discrete(labels = setNames(paste0(rl$label, ifelse(rl$label == rl$row, "", paste0(" [", rl$row, "]"))),
                                       rl$row)) +
    scale_colour_brewer(palette = "Set1", name = "Experiment family") +
    labs(x = NULL, y = NULL, title = title, subtitle = subtitle) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1,
                                     colour = fam_cols[family_of(co)]),
          legend.position = "right", panel.grid = element_blank())
  ggsave(file.path(DIR, file), g, width = 13, height = 14, dpi = 150)
  list(rows = ro, cols = co)
}
ord_bias <- heat(cond_res, "bias",
                 scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0,
                                      limits = c(-1, 1), oob = scales::squish, name = "Model - human"),
                 "Mean residual by condition (chance-corrected accuracy)",
                 "Red: model above humans; blue: below. Rows and columns clustered (average linkage). YF with each model's Exp 1 lapse.",
                 "stage2_bias_heatmap.png")
cond_rank <- cond_res |> group_by(row) |> mutate(rank = rank(rmse, ties.method = "average")) |> ungroup()
ord_rank <- heat(cond_rank, "rank",
                 scale_fill_viridis_c(direction = -1, name = "Rank (1 = best)"),
                 "Model rank by condition RMSE",
                 "Rows clustered by Spearman correlation of model ranks; columns by Euclidean distance of ranks.",
                 "stage2_rank_heatmap.png", row_method = "spearman")

# ---- shared misfit and discriminating components, xsl_datasets items ----
item_tab <- xsl |> filter(model %in% models) |>
  group_by(key, condnum, condition, cond_family, item, n_subj, n_alt, freq, cd, recency, first,
           words_per_trial, objs_per_trial, n_words, n_objs, n_trials, na_human) |>
  summarise(shared = median(resid), spread = sd(na_pred), .groups = "drop")
P <- X_xsl[models, ]
pca <- prcomp(P, center = TRUE, scale. = FALSE)
ve <- pca$sdev^2 / sum(pca$sdev^2)
cat("PCA of item-centred predictions, variance explained:", round(100 * ve[1:5], 1), "\n")
human_pc <- predict(pca, X_xsl[HUMAN, , drop = FALSE])
pc_models <- as_tibble(pca$x[, 1:3], rownames = "model") |>
  bind_rows(as_tibble(human_pc[, 1:3, drop = FALSE]) |> mutate(model = HUMAN)) |>
  mutate(family = family_of(model))
load_tab <- as_tibble(pca$rotation[, 1:3], rownames = "key") |>
  left_join(distinct(item_tab, key, condnum, cond_family), by = "key")
cond_load <- load_tab |>
  group_by(condnum, cond_family) |>
  summarise(across(PC1:PC3, \(v) sum(v^2)), n = n(), .groups = "drop") |>
  mutate(across(PC1:PC3, \(v) v / (n / sum(n))))  # > 1: more than its share of items
print(arrange(cond_load, desc(PC1)) |> head(10))
print(arrange(cond_load, desc(PC2)) |> head(10))

p_pc <- ggplot(pc_models, aes(PC1, PC2, colour = family)) +
  geom_hline(yintercept = 0, colour = "grey85") + geom_vline(xintercept = 0, colour = "grey85") +
  geom_point(aes(shape = model == HUMAN), size = 2.6) +
  geom_text_repel(aes(label = model), size = 2.6, max.overlaps = 40, show.legend = FALSE) +
  scale_colour_manual(values = fam_cols) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 8), guide = "none") +
  labs(title = "What separates the models on xsl_datasets",
       subtitle = sprintf("PCA of item-centred predictions (PC1 %.0f%%, PC2 %.0f%%); humans projected", 100 * ve[1], 100 * ve[2]),
       colour = NULL) +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
cl_long <- cond_load |>
  left_join(distinct(items, condnum, condition), by = "condnum") |>
  mutate(lab = reorder(paste0(condition, " [", condnum, "]"), PC1)) |>
  pivot_longer(PC1:PC2, names_to = "pc", values_to = "share")
p_cl <- ggplot(cl_long, aes(share, lab, fill = cond_family)) +
  geom_col() + facet_wrap(~pc, nrow = 1) +
  geom_vline(xintercept = 1, linetype = "dotted") +
  scale_fill_brewer(palette = "Set1", name = NULL) +
  labs(x = "Loading weight relative to the condition's share of items (1 = average)", y = NULL,
       title = "Which conditions carry each component") +
  theme_bw(base_size = 8) + theme(legend.position = "bottom")
ggsave(file.path(DIR, "stage2_pca.png"), p_pc / p_cl + plot_layout(heights = c(1, 1.3)),
       width = 11, height = 15, dpi = 150)

# ---- shared misfit by condition ----
shared_cond <- item_tab |> group_by(condnum, condition, cond_family) |>
  summarise(shared = mean(shared), human = mean(na_human), spread = mean(spread), .groups = "drop") |>
  mutate(lab = reorder(paste0(condition, " [", condnum, "]"), shared))
p_sh <- ggplot(shared_cond, aes(shared, lab, colour = cond_family)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_segment(aes(x = 0, xend = shared, yend = lab)) + geom_point(size = 2) +
  scale_colour_brewer(palette = "Set1", name = NULL) +
  labs(x = "Shared misfit: median over models of (model - human), mean over items", y = NULL,
       title = "Where all the models miss, xsl_datasets") +
  theme_bw(base_size = 8) + theme(legend.position = "bottom")
ggsave(file.path(DIR, "stage2_shared_misfit.png"), p_sh, width = 8, height = 10, dpi = 150)

# ---- design features ----
z <- \(v) as.numeric(scale(v))
feat_df <- item_tab |>
  left_join(load_tab |> select(key, PC1, PC2), by = "key") |>
  mutate(log_freq = log(freq), across(c(log_freq, cd, recency, n_alt, words_per_trial, objs_per_trial), z, .names = "z_{.col}"))
fml <- \(y) reformulate(c("z_log_freq", "z_cd", "z_recency", "z_n_alt", "z_words_per_trial", "z_objs_per_trial"), y)
feat_fits <- lapply(c(shared = "shared", spread = "spread", human = "na_human"), \(y) lm(fml(y), data = feat_df))
feat_coef <- bind_rows(lapply(names(feat_fits), function(y) {
  s <- summary(feat_fits[[y]])
  as_tibble(coef(s), rownames = "term") |> mutate(outcome = y, r2 = s$r.squared)
}))
print(feat_coef |> select(outcome, term, Estimate, `Std. Error`, `Pr(>|t|)`, r2), n = Inf)
fam_aov <- lapply(c(shared = "shared", spread = "spread"), \(y) summary(lm(reformulate("cond_family", y), feat_df))$r.squared)
cat("R2 of experiment family alone: shared", round(fam_aov$shared, 2), " spread", round(fam_aov$spread, 2), "\n")

saveRDS(list(D = D, floor = c(xsl = floor_xsl, yf = floor_yf), to_human = to_human, mds = mds_df,
             cond_res = cond_rank, heat_order = list(bias = ord_bias, rank = ord_rank),
             pca = pca, pc_models = pc_models, cond_load = cond_load, items = item_tab,
             feat_coef = feat_coef, shared_cond = shared_cond),
        file.path(DIR, "clusters.rds"))
