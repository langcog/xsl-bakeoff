# Figures for the model- and parameter-recovery appendix
# (paper/appendix_recovery.Rmd), drawn from fits/v05/extras/recovery/recovery.rds
# (made by fits/v05/extras/recovery.R).
#
# Sourced by the appendix, or run standalone to write the figures to
# paper/figs/recovery/ for review:
#   Rscript paper/recovery_figs.R

suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(here)
})

rec_path <- here("fits/v05/extras/recovery/recovery.rds")

# Paper abbreviations, in mechanism order (baseline, associative, episodic,
# reinforcement learning, hypothesis testing)
rec_models <- c(baseline = "Base", fazly = "PA", kachergis = "FU", kachergis_sampling = "FUs",
                Bayesian_decay = "BD", tilles = "TF", minerva2 = "M2", softmax_rl = "RL",
                trueswell2012 = "PbV")
# Parameter symbols in the paper's notation, in fits/v05/specs.R order (p1, p2, ...)
rec_params <- list(
  fazly              = c("lambda", "beta"),
  kachergis          = c("chi", "lambda", "alpha"),
  kachergis_sampling = c("chi", "lambda", "alpha"),
  Bayesian_decay     = c("alpha", "delta", "tau"),
  tilles             = c("x", "b", "alpha[0]"),
  minerva2           = c("X"),
  softmax_rl         = c("eta", "beta"),
  trueswell2012      = c("alpha", "epsilon"))

# Colors: one series (blue, slot 1); sequential blue ramp for the heatmap;
# text in ink tokens, never the series color
COL_POINT <- "#2a78d6"
COL_INK   <- "#2b2b2b"
COL_MUTED <- "#6b6b6b"
COL_GRID  <- "#e6e6e3"
SEQ_BLUE  <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")

theme_rec <- function(base_size = 8) {
  theme_minimal(base_size = base_size) +
    theme(text = element_text(colour = COL_INK),
          axis.text = element_text(colour = COL_MUTED),
          panel.grid.major = element_line(colour = COL_GRID, linewidth = .3),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = base_size, face = "bold", margin = margin(b = 2)),
          plot.margin = margin(3, 6, 3, 3))
}

load_recovery <- function(path = rec_path) readRDS(path)

# Axis labels that stay readable from 1e-10 to 20000
lab_num <- function(x) vapply(x, function(v) {
  if (is.na(v)) return(NA_character_)
  if (v != 0 && (abs(v) < 1e-3 || abs(v) >= 1e5)) return(format(v, scientific = TRUE, digits = 1))
  format(v, digits = 3, drop0trailing = TRUE, big.mark = ",", scientific = FALSE, trim = TRUE)
}, character(1))

## ---- Figure: parameter recovery (true vs recovered, one panel per parameter) ----
fig_param_recovery <- function(rec) {
  d <- rec$param$draws |>
    mutate(idx = as.integer(sub("^p", "", param)),
           sym = mapply(\(m, i) rec_params[[m]][i], model, idx),
           abbr = rec_models[model])
  s <- rec$param$summary |> mutate(idx = as.integer(sub("^p", "", param)))
  panels <- d |> distinct(model, idx, sym, abbr, logscale) |>
    mutate(model = factor(model, names(rec_models))) |> arrange(model, idx)

  plots <- lapply(seq_len(nrow(panels)), function(k) {
    pn <- panels[k, ]
    dd <- filter(d, model == as.character(pn$model), idx == pn$idx)
    r  <- filter(s, model == as.character(pn$model), idx == pn$idx)$r
    lim <- range(c(dd$true, dd$fit))
    p <- ggplot(dd, aes(true, fit)) +
      geom_abline(slope = 1, intercept = 0, colour = COL_MUTED, linetype = "dashed", linewidth = .3) +
      geom_point(colour = COL_POINT, size = 1.6) +
      # bottom-right corner in data units (an Inf position is dropped on log
      # axes); misses above the line (over-estimates) crowd the top-left
      annotate("text", x = lim[2], y = lim[1], hjust = 1, vjust = 0, size = 2.5, colour = COL_INK,
               label = sprintf("r = %.2f", r)) +
      labs(title = parse(text = paste0('"', pn$abbr, ': "~', pn$sym))[[1]], x = NULL, y = NULL) +
      theme_rec()
    if (pn$logscale) {
      p + scale_x_log10(limits = lim, labels = lab_num) + scale_y_log10(limits = lim, labels = lab_num)
    } else {
      p + scale_x_continuous(limits = lim) + scale_y_continuous(limits = lim)
    }
  })
  wrap_plots(plots, ncol = 4) +
    plot_annotation(caption = paste0("x: generating (true) value;  y: recovered value;  dashed: identity.  ",
                                     "Log axes where the fitting bounds span 100x or more."),
                    theme = theme(plot.caption = element_text(colour = COL_MUTED, size = 7, hjust = 0)))
}

## ---- Figure: model recovery (generator x fitter objective gap) ----------------
fig_model_recovery <- function(rec) {
  f <- rec$model$fits
  own <- f |> filter(fitter == gen) |> select(gen, rep, own = obj)
  gap <- f |> left_join(own, by = c("gen", "rep")) |>
    group_by(gen, fitter) |>
    summarise(gap = mean(obj - own), .groups = "drop") |>
    left_join(rec$model$confusion |> select(gen, fitter, wins = n), by = c("gen", "fitter")) |>
    mutate(wins = coalesce(wins, 0L),
           n_rep = n_distinct(f$rep),
           gen = factor(rec_models[gen], rev(rec_models)),
           fitter = factor(rec_models[fitter], rec_models),
           diag = as.character(gen) == as.character(fitter),
           label = ifelse(diag, sprintf("%d/%d", wins, n_rep), sprintf("%.2f", gap)),
           fill_val = ifelse(diag, NA, gap))
  ggplot(gap, aes(fitter, gen)) +
    geom_tile(aes(fill = fill_val), colour = "white", linewidth = 1) +
    geom_tile(data = filter(gap, diag), fill = "white", colour = COL_INK, linewidth = .4) +
    # white text once the fill is darker than the ramp's ~#3987e5 step
    geom_text(aes(label = label, colour = ifelse(!diag & fill_val > .15, "light", "dark")),
              size = 2.4, show.legend = FALSE) +
    scale_colour_manual(values = c(light = "white", dark = COL_INK)) +
    scale_fill_gradientn(colours = SEQ_BLUE, trans = "log10", na.value = "white",
                         name = "Mean objective of the fitting model\nminus the generating model's (log scale)",
                         labels = lab_num) +
    labs(x = "Model fit to the synthetic data", y = "Model that generated the data",
         caption = "Diagonal: number of synthetic datasets the generating model won.") +
    coord_equal() +
    theme_rec(9) +
    theme(panel.grid.major = element_blank(), legend.position = "bottom",
          legend.key.width = unit(1.4, "cm"), legend.title = element_text(size = 7),
          plot.caption = element_text(colour = COL_MUTED, size = 7, hjust = 0))
}

## ---- Figure: margin of the generator over its closest rival, per dataset ------
fig_recovery_margin <- function(rec) {
  m <- rec$model$margin |>
    mutate(margin = best_other - true_obj,
           gen = factor(rec_models[gen], rev(rec_models)),
           rival = rec_models[best_other_model])
  rivals <- m |> group_by(gen) |>
    summarise(rival = names(which.max(table(rival))), x = max(margin), .groups = "drop")
  ggplot(m, aes(margin, gen)) +
    geom_point(colour = COL_POINT, size = 2, alpha = .85) +
    geom_text(data = rivals, aes(x = x, label = paste0("closest rival: ", rival)),
              hjust = -.15, size = 2.5, colour = COL_MUTED) +
    scale_x_log10(labels = lab_num,
                  expand = expansion(mult = c(.05, .45))) +
    labs(x = "Closest rival's objective minus the generating model's (log scale; > 0 = generator wins)",
         y = NULL) +
    theme_rec(9)
}

if (sys.nframe() == 0) {
  rec <- load_recovery()
  out <- here("paper/figs/recovery"); dir.create(out, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(out, "param_recovery.pdf"), fig_param_recovery(rec), width = 7, height = 8.2)
  ggsave(file.path(out, "param_recovery.png"), fig_param_recovery(rec), width = 7, height = 8.2, dpi = 200, bg = "white")
  ggsave(file.path(out, "model_recovery.pdf"), fig_model_recovery(rec), width = 6, height = 6.4)
  ggsave(file.path(out, "model_recovery.png"), fig_model_recovery(rec), width = 6, height = 6.4, dpi = 200, bg = "white")
  ggsave(file.path(out, "recovery_margin.pdf"), fig_recovery_margin(rec), width = 6, height = 3.2)
  ggsave(file.path(out, "recovery_margin.png"), fig_recovery_margin(rec), width = 6, height = 3.2, dpi = 200, bg = "white")
  message("wrote figures to ", out)
}
