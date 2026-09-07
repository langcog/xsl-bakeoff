# Generalization to held-out experiments, using the XSLmodels package.
#
# Replaces the old research-code generalization analysis in
# xsl-bake-off.Rmd (which produced fits/pre-package/generalization_exp_results.RData).
#
# Each model is run at the parameters it was given by the joint GROUP fit to
# all 53 conditions in xsl_datasets (fits/group_fits_all_experiments.Rdata +
# fits/group_fits_new_models.Rdata) and its predictions are compared against
# published group-level human performance for experiments that were NOT in
# that fit.
#
# Two tiers:
#
#   A. Truly held out -- never in xsl_datasets, so never fit:
#        * Medina et al. (2011)   -- 4 conditions x 5 training "vignette"
#          blocks; human block accuracy read from their Figure 2.
#        * Smith & Yu (2008) / Yu & Smith (2011) -- one infant condition,
#          6 words, 2AFC; humans learn ~4/6.
#
#   B. Formerly held out in the CogSci paper, but added to xsl_datasets
#      afterwards (XSLmodels 0.2.0) and therefore IN the group fit now:
#        * Suanda et al. (2014)   -- 3 contextual-diversity conditions, 6yo
#        * Koehne et al. (2013)   -- 4 meaning-order conditions, adults
#      Reported so the paper can say "these were held out before, and here
#      is how they look now that they are in the fit set" -- their numbers
#      come straight from the group fit, no re-simulation.
#
# Output: fits/generalization_with_package.RData with
#   gen_exps       tibble(exp, model, MSE, RMSE, tier)      -- one row per (exp, model)
#   gen_exps_perf  tibble(exp, Condition, model, Human, Model, Model_sd, tier)
# MSE is mean squared error over that experiment's condition/block points
# (matching the old artifact's column name); RMSE = sqrt(MSE).

library(XSLmodels)
library(dplyr)
library(purrr)
library(tibble)
library(here)

source(here("fits/model_specs.R"))       # model_specs, fazlyt_spec
source(here("fits/new_model_specs.R"))   # new_model_specs

N_SIM <- 200          # simulated learners per stochastic model (matches the fit scripts)
BLOCK_SIZE_MEDINA <- 12

set.seed(123)

## ---------------------------------------------------------------------------
## Model roster: constructor + group-fit parameters, keyed by package name
## ---------------------------------------------------------------------------

group_fits <- local({
  e1 <- new.env(); load(here("fits/group_fits_all_experiments.Rdata"), envir = e1)
  e2 <- new.env(); load(here("fits/group_fits_new_models.Rdata"),       envir = e2)
  c(e1$group_fits_all, e2$group_fits_new)
})

specs <- c(
  model_specs,
  list(
    baseline = list(constructor = function(p) baseline(), stochastic = FALSE),
    fazlyt   = fazlyt_spec
  ),
  new_model_specs
)

model_names <- intersect(names(specs), names(group_fits))
message("Models: ", paste(model_names, collapse = ", "))

build_model <- function(name) {
  bestmem <- group_fits[[name]]$optim$bestmem
  specs[[name]]$constructor(if (is.null(bestmem)) numeric(0) else as.numeric(bestmem))
}
is_stochastic <- function(name) isTRUE(specs[[name]]$stochastic)
control_for <- function(name, ...) {
  if (is_stochastic(name)) xslControl(n_sim = N_SIM, ...) else xslControl(...)
}

# proportion-correct (Luce) per word from a word-by-object matrix. The
# correct object for word `w` shares its label (`m[w, w]`); matched by
# dimname rather than position, since a truncated run need not contain
# every object. Unseen / all-zero-row words score 0, matching the old
# analysis.
word_perf <- function(m) {
  wn <- rownames(m); on <- colnames(m)
  vapply(wn, function(w) {
    if (w %in% on && sum(m[w, ]) > 0) m[w, w] / sum(m[w, ]) else 0
  }, numeric(1))
}

## ---------------------------------------------------------------------------
## Tier A.1 -- Medina et al. (2011): block-wise learning trajectory
## ---------------------------------------------------------------------------
# 12 words, 60 trials, one word per trial. High-Informative trials show 2
# referents, Low-Informative trials show 5. Four between-subjects conditions
# differ in where (if anywhere) the block of HI trials falls. Human accuracy
# is per 12-trial block, read from Medina et al. (2011, Fig. 2).

load(here("other_exps/Medina2013orders.RData")) # `Medina`: list of 4 conditions

medina_human <- list(
  HIfirst  = c(.66, .33, .39, .46, .41),
  HImid    = c(.14, .14, .39, .30, .16),
  HIlast   = c(.07, .09, .14, .08, .21),
  HIabsent = c(.16, .20, .21, .18, .21)
)

# build a truncated xslData: the first `n_trials` training trials only
medina_xsldata <- function(cond, n_trials) {
  ord <- Medina[[cond]]
  w <- ord$train$words[seq_len(n_trials), , drop = TRUE]
  o <- ord$train$objs[seq_len(n_trials), , drop = FALSE]
  xslData(
    train = list(
      words   = lapply(w, function(x) as.integer(x)),
      objects = lapply(seq_len(nrow(o)), function(i) as.integer(o[i, !is.na(o[i, ])]))
    ),
    label = cond, condition = paste("Medina 2011", cond)
  )
}

# block perf for one model: run the model on the training truncated at each
# 12-trial block boundary and take mean word_perf() over the 12 words.
# xsl_run() already averages over n_sim internally for a stochastic model
# (fits[[1]]$matrix is the sum over sims), so divide by n_sim to recover the
# mean association matrix.
medina_block_perf <- function(name) {
  boundaries <- seq(BLOCK_SIZE_MEDINA, 60, by = BLOCK_SIZE_MEDINA)
  n_sim <- if (is_stochastic(name)) N_SIM else 1L
  map_dfr(names(medina_human), function(cond) {
    per_block <- map_dbl(boundaries, function(nb) {
      res <- xsl_run(build_model(name), list(medina_xsldata(cond, nb)),
                     control = control_for(name))
      mean(word_perf(res$fits[[1]]$matrix / n_sim))
    })
    tibble(exp = "Medina et al.", Condition = cond,
           block = seq_along(boundaries),
           Model = per_block, Model_sd = NA_real_,
           Human = medina_human[[cond]])
  })
}

## ---------------------------------------------------------------------------
## Tier A.2 -- Smith & Yu (2008) / Yu & Smith (2011): infant 2AFC
## ---------------------------------------------------------------------------
# 6 word-object pairs, 2 pairs per trial, 30 trials; each word tested twice
# in 2AFC. Infants learned ~4 of 6 words (accuracy 2/3).

sy_ord <- as.matrix(read.table(here("other_exps/SmithYu2008-2x2-6w.txt"),
                               header = FALSE, sep = "\t"))
sy_train_trials <- lapply(seq_len(nrow(sy_ord)), function(i) as.integer(sy_ord[i, ]))
# 12 test trials: each word vs. two different foils (Smith & Yu 2008 design)
sy_test <- list(
  words   = as.list(c(1:6, 1:6)),
  objects = list(c(1,2), c(2,3), c(3,4), c(4,5), c(5,6), c(6,3),
                 c(1,3), c(2,4), c(3,5), c(4,6), c(5,1), c(6,2))
)
smith_yu_data <- xslData(
  train = list(words = sy_train_trials, objects = sy_train_trials),
  test  = sy_test,
  accuracy = rep(2/3, 6), n_subj = 28,
  label = "SmithYu", condition = "Smith & Yu 2008"
)

smith_yu_perf <- function(name) {
  res <- xsl_run(build_model(name), list(smith_yu_data), control = control_for(name))
  p <- res$fits[[1]]$perf              # P(correct) per test trial (mafc_test)
  tibble(exp = "Smith & Yu", Condition = "Smith & Yu 2008",
         Model = mean(p, na.rm = TRUE),
         Model_sd = sd(p, na.rm = TRUE),
         Human = 2/3)
}

## ---------------------------------------------------------------------------
## Tier B -- Suanda 2014 & Koehne 2013: now in xsl_datasets / the group fit
## ---------------------------------------------------------------------------
# Pull their per-condition model-vs-human straight from the group fit
# dataframe (gfd), rather than re-simulating: these conditions were fit
# jointly with everything else.

gfd_group <- local({
  e1 <- new.env(); load(here("fits/group_fits_all_experiments.Rdata"), envir = e1)
  e2 <- new.env(); load(here("fits/group_fits_new_models.Rdata"),       envir = e2)
  bind_rows(e1$gfd_all, e2$gfd_new)
})

tierB_from_gfd <- function(pattern, exp_label) {
  gfd_group %>%
    filter(grepl(pattern, condnum)) %>%
    group_by(model = Model, exp = exp_label, Condition = condnum) %>%
    summarise(Model = mean(ModelPerf), Model_sd = sd(ModelPerf),
              Human = mean(HumanPerf), .groups = "drop") %>%
    select(exp, Condition, model, Human, Model, Model_sd)
}

## ---------------------------------------------------------------------------
## Run everything
## ---------------------------------------------------------------------------

message("Tier A.1: Medina et al. (2011) ...")
medina_perf <- map_dfr(model_names, function(m) {
  message("  ", m); medina_block_perf(m) %>% mutate(model = m)
})

message("Tier A.2: Smith & Yu (2008) ...")
smithyu_perf <- map_dfr(model_names, function(m) {
  message("  ", m); smith_yu_perf(m) %>% mutate(model = m)
})

message("Tier B: Suanda et al. (2014) & Koehne et al. (2013) from the group fit ...")
suanda_perf <- tierB_from_gfd("^Suanda2014", "Suanda et al.")
koehne_perf <- tierB_from_gfd("^Koehne2013", "Koehne et al.")

gen_exps_perf <- bind_rows(
  medina_perf  %>% mutate(tier = "held out"),
  smithyu_perf %>% mutate(tier = "held out"),
  suanda_perf  %>% mutate(tier = "now in fit set"),
  koehne_perf  %>% mutate(tier = "now in fit set")
) %>%
  select(exp, Condition, model, Human, Model, Model_sd, tier, everything())

gen_exps <- gen_exps_perf %>%
  group_by(exp, model, tier) %>%
  summarise(MSE = mean((Human - Model)^2), .groups = "drop") %>%
  mutate(RMSE = sqrt(MSE)) %>%
  arrange(exp, MSE)

save(gen_exps, gen_exps_perf,
     file = here("fits/generalization_with_package.RData"))
message("Saved fits/generalization_with_package.RData")

print(gen_exps %>% arrange(exp, MSE), n = Inf)
