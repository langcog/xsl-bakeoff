# v05 generalization test: each model's v05 group-fit parameters
# (fits/v05/adults/group_fits.Rdata, fit to the 47 xsl_datasets conditions)
# are evaluated -- with NO refitting -- on data never seen during fitting.
# Supersedes fits/v04/generalize.R and the round-1
# fits/generalization_with_package.R. REVISION_PLAN R4.1 #6.
#
# Held-out data, by the level at which humans can be compared:
#   item level (per-item accuracy)
#     benitez2020                 4 child + 3 adult conditions, 8-item tests
#     kachergis_initial_accuracy  2 conditions, 18 items
#   condition means only (every item's stored accuracy is the condition mean)
#     suanda2014                  3 conditions, children aged 5-7
#     koehne2013                  4 conditions, adults
#     smith_yu2008                1 condition (infants), 2AFC; humans ~4/6 words
#     vlach_debrock2017           1 condition (children aged 2-5), 12 2AFC test
#                                 trials, each the target vs. one other learned
#                                 object (Vlach & DeBrock, 2017, Exp. 1; chance =
#                                 6/12). The foil assignment isn't reported, so the
#                                 model's value is its 2AFC accuracy averaged over
#                                 every possible foil, compared to the reported
#                                 mean (.5583).
#   trial-by-trial learning curves
#     medina2011                  4 conditions x 5 blocks of 12 trials; humans guess
#                                 a referent on every trial, and the reported
#                                 accuracy is the proportion of correct guesses
#                                 per block (Medina et al., 2011, Fig. 2). The
#                                 model's analogue is its probability of choosing
#                                 the correct referent among the objects on that
#                                 trial, given what it learned on earlier trials.
#
# Libraries: as for the other v05 analyses (fits/v05/extras/common.R), tilles
# is scored with its corrected model (fits/v05/tilles/Rlib, XSLmodels b44b9fa)
# and every other model with the v05 pin (fits/v05/Rlib, e70c10c). Run both:
#   bash fits/v05/run_generalize.sh
# or by hand, from xsl-bakeoff/:
#   R_LIBS=$(pwd)/fits/v05/Rlib        Rscript fits/v05/generalize.R   # all but tilles
#   R_LIBS=$(pwd)/fits/v05/tilles/Rlib Rscript fits/v05/generalize.R   # tilles
# Each run scores its models, caches them in fits/v05/generalization/<model>.rds
# (skipped next time unless the model's parameters change), then rebuilds the
# summary from every cached model.
#
# Output: fits/v05/generalization/generalization.Rdata --
#   gen_items   one row per model x scored item (or block, for Medina)
#   gen_cond    one row per model x held-out condition (mean model vs. human)
#   gen_summary one row per model: fit by holdout set, plus in-sample and CV
#   meta

source(here::here("fits/v05/extras/common.R"))  # specs_v04, RUN_ORDER, run_model(), XSL_LIB_USED, stamp()

N_SIM   <- as.integer(Sys.getenv("N_SIM", "200"))
OUT_DIR <- Sys.getenv("GEN_OUT_DIR", here("fits/v05/generalization"))
dir.create(OUT_DIR, showWarnings = FALSE)

group_pars <- load_group_pars()
models <- commandArgs(trailingOnly = TRUE)
if (!length(models)) {
  models <- intersect(RUN_ORDER, names(group_pars))
  models <- if (XSL_LIB_USED == "tilles") "tilles" else setdiff(models, "tilles")
}
stopifnot(all(models %in% names(group_pars)))
if ("tilles" %in% models && XSL_LIB_USED != "tilles") {
  stop("score tilles with its corrected model: R_LIBS=$(pwd)/fits/v05/tilles/Rlib")
}

# ---- held-out data ----------------------------------------------------------
item_sets <- list(
  benitez2020 = benitez2020,
  kachergis_initial_accuracy = kachergis_initial_accuracy
)
mean_sets <- list(suanda2014 = suanda2014, koehne2013 = koehne2013)

# Smith & Yu (2008): 6 pairs, 2 pairs per trial, 30 trials; each word tested
# twice in 2AFC against two different foils; infants learned ~4 of 6 words.
sy_ord <- as.matrix(read.table(here("other_exps/SmithYu2008-2x2-6w.txt"), header = FALSE, sep = "\t"))
sy_trials <- lapply(seq_len(nrow(sy_ord)), \(i) as.integer(sy_ord[i, ]))
mean_sets$smith_yu2008 <- list(`Smith & Yu 2008` = xslData(
  train = list(words = sy_trials, objects = sy_trials),
  test = list(words = as.list(c(1:6, 1:6)),
              objects = list(c(1, 2), c(2, 3), c(3, 4), c(4, 5), c(5, 6), c(6, 3),
                             c(1, 3), c(2, 4), c(3, 5), c(4, 6), c(5, 1), c(6, 2))),
  accuracy = rep(2 / 3, 12), n_subj = 28, label = "SmithYu2008", condition = "Smith & Yu 2008"))

# Vlach & DeBrock (2017, Exp. 1): every word against every other learned object
# as the 2AFC foil (132 trials); its mean is the model's expected accuracy under
# random foil assignment. Human: the reported mean, .5583 (sd .1975), n = 47.
vd <- vlach_debrock2017
vd_words <- sort(unique(unlist(vd$train$words)))
stopifnot(identical(as.numeric(vd_words), as.numeric(sort(unique(unlist(vd$train$objects))))))
vd_pairs <- expand.grid(foil = vd_words, word = vd_words) |> subset(foil != word)
mean_sets$vlach_debrock2017 <- list(`Vlach & DeBrock 2017` = xslData(
  train = vd$train,
  test = list(words = as.list(vd_pairs$word), objects = map2(vd_pairs$word, vd_pairs$foil, c)),
  accuracy = rep(.5583, nrow(vd_pairs)), n_subj = vd$n_subj,
  label = "VlachDeBrock2017", condition = "Vlach & DeBrock 2017"))

# Medina et al. (2011): 12 words, 60 trials, one word per trial with 2 (high
# informative) or 5 (low informative) referents; conditions differ in where the
# block of HI trials falls. Human accuracy per 12-trial block (their Fig. 2).
e <- new.env(); load(here("other_exps/Medina2013orders.RData"), envir = e)
medina <- map(e$Medina, function(cond) {
  w <- as.integer(cond$train$words[, 1])
  o <- lapply(seq_len(nrow(cond$train$objs)), \(i) { x <- cond$train$objs[i, ]; as.integer(x[!is.na(x)]) })
  list(words = w, objects = o, human = cond$HumBlockAcc)
})
MEDINA_BLOCK <- 12

# ---- scoring one model ------------------------------------------------------
ch_dec_of <- function(mod) mod$params[["ch_dec"]]

score_model <- function(m, par) {
  spec <- specs_v04[[m]]
  mod <- spec$constructor(par)
  d <- ch_dec_of(mod)
  set.seed(2026)
  # item- and mean-level holdouts: xsl_run()'s own scoring (mafc_test() when a
  # condition has a test, else get_perf())
  per_cond <- function(sets, level) imap(sets, \(set, set_name) imap(set, function(dat, cn) {
    f <- run_model(m, par, dat, n_sim = N_SIM)$fits[[1]]
    tibble(model = m, set = set_name, condition = cn, level = level, n_subj = dat$n_subj,
           item = seq_along(dat$accuracy), model_perf = as.vector(f$perf), human = dat$accuracy)
  }) |> list_rbind()) |> list_rbind()
  items <- bind_rows(per_cond(item_sets, "item"), per_cond(mean_sets, "mean"))
  # Medina: on trial t, P(correct guess | objects on t) from the matrix learned
  # on trials 1..t-1 (chance on trial 1), averaged within 12-trial blocks
  med <- imap(medina, function(cond, cn) {
    p <- map_dbl(seq_along(cond$words), function(t) {
      objs <- cond$objects[[t]]
      # a word not yet heard has no associations, so every model guesses at
      # chance (what predict_referent() returns for an unseen word); skipping
      # the run also avoids training on a single trial, which tilles() can't do
      if (!cond$words[t] %in% cond$words[seq_len(t - 1)]) return(1 / length(objs))
      dat <- xslData(train = list(words = as.list(cond$words[seq_len(t - 1)]),
                                  objects = cond$objects[seq_len(t - 1)]))
      mat <- run_model(m, par, dat, n_sim = N_SIM)$fits[[1]]$matrix
      probs <- predict_referent(mat, as.character(cond$words[t]), as.character(objs), d = d)
      probs[match(cond$words[t], objs)]
    })
    block <- ceiling(seq_along(p) / MEDINA_BLOCK)
    tibble(model = m, set = "medina2011", condition = cn, level = "trajectory", n_subj = NA_real_,
           item = seq_along(cond$human), model_perf = as.vector(tapply(p, block, mean)), human = cond$human)
  }) |> list_rbind()
  bind_rows(items, med) |> mutate(par = list(par), lib = XSL_LIB_USED)
}

for (m in models) {
  f <- file.path(OUT_DIR, paste0(m, ".rds"))
  par <- group_pars[[m]]
  if (file.exists(f) && identical(readRDS(f)$par[[1]], par)) next
  stamp("generalize  ", m, "  (", XSL_LIB_USED, " library)")
  saveRDS(score_model(m, par), f)
}

# ---- summary over every cached model ----------------------------------------
gen_items <- list.files(OUT_DIR, pattern = "^[^.]+\\.rds$", full.names = TRUE) |>
  setdiff(file.path(OUT_DIR, "generalization.rds")) |> map(readRDS) |> list_rbind()
stopifnot(!anyDuplicated(gen_items[c("model", "set", "condition", "item")]))
gen_items <- gen_items |>
  mutate(group = case_when(set == "benitez2020" & grepl("kids$", condition) ~ "benitez2020 children",
                           set == "benitez2020" ~ "benitez2020 adults",
                           TRUE ~ set))

gen_cond <- gen_items |>
  group_by(model, set, group, condition, level, n_subj) |>
  summarise(model_mean = mean(model_perf), human_mean = mean(human),
            item_mse = mean((model_perf - human)^2), .groups = "drop") |>
  mutate(mean_error = model_mean - human_mean)

item_fit <- gen_items |> filter(level == "item") |>
  group_by(model, group) |>
  summarise(mse = mean((model_perf - human)^2),
            r = if (sd(model_perf) > 0) cor(model_perf, human) else NA_real_, .groups = "drop") |>
  tidyr::pivot_wider(names_from = group, values_from = c(mse, r), names_glue = "{group}_{.value}")
mean_fit <- gen_cond |> filter(level == "mean") |>
  group_by(model, set) |>
  summarise(mae = mean(abs(mean_error)), .groups = "drop") |>
  tidyr::pivot_wider(names_from = set, values_from = mae, names_glue = "{set}_mean_abs_error")
medina_fit <- gen_items |> filter(set == "medina2011") |>
  group_by(model) |>
  summarise(medina2011_rmse = sqrt(mean((model_perf - human)^2)),
            medina2011_r = if (sd(model_perf) > 0) cor(model_perf, human) else NA_real_, .groups = "drop")

g <- new.env(); load(here("fits/v05/adults/group_fits.Rdata"), envir = g)
cvf <- new.env(); load(here("fits/v05/adults/cv_fits.Rdata"), envir = cvf)
in_sample <- g$gfd |> group_by(model = Model) |>
  summarise(in_sample_mse = mean((ModelPerf - HumanPerf)^2, na.rm = TRUE),
            in_sample_r = cor(ModelPerf, HumanPerf, use = "complete.obs"), .groups = "drop") |>
  mutate(in_sample_sse = map_dbl(model, \(m) g$group_fits[[m]]$optim$bestval))
cv <- imap(Filter(\(x) !is.null(x$testdf), cvf$cv_fits), \(x, m) {
  d <- x$testdf; tibble(model = m, cv_mse = mean((d$ModelPerf - d$HumanPerf)^2, na.rm = TRUE),
                        cv_r = cor(d$ModelPerf, d$HumanPerf, use = "complete.obs"))
}) |> list_rbind()

gen_summary <- in_sample |> left_join(cv, by = "model") |>
  inner_join(item_fit, by = "model") |> left_join(mean_fit, by = "model") |> left_join(medina_fit, by = "model") |>
  mutate(n_params = map_int(model, \(m) length(group_pars[[m]])), .after = model) |>
  arrange(`benitez2020 children_mse`)

meta <- list(group_fits_meta = g$meta, n_sim = N_SIM, libs = distinct(gen_items, model, lib),
             medina_block = MEDINA_BLOCK, updated = format(Sys.time()))
save(gen_items, gen_cond, gen_summary, meta, file = file.path(OUT_DIR, "generalization.Rdata"))
print(as.data.frame(gen_summary), digits = 3, row.names = FALSE)
