# Round-2 generalization test: each model's group-fit parameters from
# fits/v04/group_fits.Rdata (fit to all 56 xsl_datasets conditions) are
# evaluated -- with NO refitting -- on XSLmodels' scorable holdout datasets
# (xsl_holdout_datasets with scorable = TRUE: benitez2020,
# kachergis_initial_accuracy), which were never seen during fitting.
# Supersedes fits/generalization_test.R (round-1 params) for round 2.
#
# Usage (from xsl-bakeoff/):
#   R_LIBS=$(pwd)/fits/v04/Rlib Rscript fits/v04/generalize.R
#
# Resumable: models already scored at their current group-fit parameters are
# skipped, so it can be re-run while refit.R is still adding models.
#
# Two scales are reported, each identical in-sample and on the holdout:
#  - sse: xsl_run()'s objective, the n_subj-weighted mean over conditions of
#    each condition's SSE (in-sample this is the group fit's bestval);
#  - mse / r: per-item squared error and model-human correlation, pooled
#    over every scored item (in-sample from refit.R's gfd).
#
# Output: fits/v04/generalization.Rdata -- gen (one row per model x holdout
# condition), gen_summary (one row per model), meta

suppressMessages({
  library(XSLmodels)
  library(purrr)
  library(dplyr)
  library(here)
})

if (as.character(packageVersion("XSLmodels")) != "0.4.0" ||
    !startsWith(find.package("XSLmodels"), here("fits/v04/Rlib"))) {
  stop("run with R_LIBS=$(pwd)/fits/v04/Rlib (found XSLmodels ",
       packageVersion("XSLmodels"), " at ", find.package("XSLmodels"), ")")
}
source(here("fits/v04/specs.R"))  # specs_v04, RUN_ORDER

N_SIM <- as.integer(Sys.getenv("N_SIM", "200"))
OUT   <- here("fits/v04/generalization.Rdata")

load(here("fits/v04/group_fits.Rdata"))  # group_fits, gfd, meta
fit_meta <- meta

# the registry holds metadata; each dataset is the exported object of that name
holdout_sets <- names(keep(xsl_holdout_datasets, "scorable"))
conditions <- map(holdout_sets, \(set) {
  d <- getExportedValue("XSLmodels", set)
  set_names(d, paste0(set, "/", names(d)))
}) |> list_flatten()

gen <- tibble()
if (file.exists(OUT)) load(OUT)

stamp <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)

for (m in intersect(RUN_ORDER, names(group_fits))) {
  par <- unname(group_fits[[m]]$optim$bestmem)
  done <- if (nrow(gen)) filter(gen, model == m) else gen
  if (nrow(done) && identical(done$par[[1]], par)) next
  spec <- specs_v04[[m]]
  stamp("holdout  ", m)
  mod <- spec$constructor(par)
  ctrl <- if (spec$stochastic) xslControl(n_sim = N_SIM) else xslControl()
  set.seed(2026)
  rows <- imap(conditions, \(dat, cn) {
    f <- xsl_run(mod, dat, control = ctrl)$fits[[1]]
    tibble(model = m, condition = cn, n_items = length(dat$accuracy), n_subj = dat$n_subj,
           sse = f$sse, model_perf = list(as.vector(f$perf)), human = list(dat$accuracy))
  }) |> list_rbind()
  rows$par <- list(par)
  gen <- bind_rows(if (nrow(gen)) filter(gen, model != m), rows)
  save(gen, file = OUT)
}

gen_summary <- gen |>
  group_by(model) |>
  summarise(n_params = length(par[[1]]),
            holdout_sse = sum(sse * n_subj) / sum(n_subj),
            holdout_mse = sum(sse) / sum(n_items),
            holdout_r = cor(unlist(model_perf), unlist(human), use = "complete.obs"),
            benitez_mse = sum(sse[startsWith(condition, "benitez2020")]) /
              sum(n_items[startsWith(condition, "benitez2020")]),
            initacc_mse = sum(sse[startsWith(condition, "kachergis_initial")]) /
              sum(n_items[startsWith(condition, "kachergis_initial")]),
            .groups = "drop") |>
  left_join(gfd |> group_by(model = Model) |>
              summarise(in_sample_mse = mean((ModelPerf - HumanPerf)^2, na.rm = TRUE),
                        in_sample_r = cor(ModelPerf, HumanPerf, use = "complete.obs")),
            by = "model") |>
  mutate(in_sample_sse = map_dbl(model, \(m) group_fits[[m]]$optim$bestval), .after = n_params) |>
  relocate(in_sample_mse, in_sample_r, .after = in_sample_sse) |>
  arrange(holdout_sse)

meta <- list(group_fits_meta = fit_meta, n_sim = N_SIM, holdout = names(conditions),
             updated = format(Sys.time()))
save(gen, gen_summary, meta, file = OUT)
print(as.data.frame(gen_summary), digits = 3, row.names = FALSE)
