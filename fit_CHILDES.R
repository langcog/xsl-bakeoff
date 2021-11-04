source("load_corpus_data.R")
source("ROC.R")
source("optimize_corpus_fscores.R")

#load in group_fits before running/sourcing
load(here("fits","group_fits.Rdata"))

determ_models = c("Bayesian_decay", "fazly", "kachergis", 
                  "novelty", "rescorla-wagner", "strength", "uncertainty")
stochastic_models = c("guess-and-test","pursuit","trueswell2012","kachergis_sampling")

#corpus_fits = list(FGT = list(), FM = list())
load(here("fits","FGT_FM_fits.Rdata"))

for (model_name in determ_models) {
  fit = optimize_corpus_fscore(fgt_ord, model_name)
  corpus_fits[["FGT"]][model_name] = fit
  save(corpus_fits, file=here("fits","FGT_FM_fits.Rdata"))
}

determ_models = c("kachergis", "uncertainty") # re-fit for FM
group_fits$kachergis$member$upper = c(.5, 11, 1)
group_fits$uncertainty$member$upper = c(.5, 11, 1)

for (model_name in determ_models) {
  fit = optimize_corpus_fscore(fm_ord, model_name)
  corpus_fits[["FM"]][model_name] = fit
  save(corpus_fits, file=here("fits","FGT_FM_fits.Rdata"))
}
# problem for FM with kachergis, and with uncertainty: 
#Error in if (min(p) < 0 || sum(p) <= 0) return(NA) : 
#missing value where TRUE/FALSE needed

# ToDo: fit stochastic models

for (model_name in stochastic_models) {
  fit = optimize_corpus_fscore(fgt_ord, model_name)
  corpus_fits[["FGT"]][model_name] = fit
  save(fit, file=here("fits","FGT_FM_stoch_fits.Rdata"))
}

for (model_name in stochastic_models) {
  fit = optimize_corpus_fscore(fm_ord, model_name)
  corpus_fits[["FM"]][model_name] = fit
  save(fit, file=here("fits","FGT_FM_stoch_fits.Rdata"))
}


df <- tibble()
for(corpus_name in names(corpus_fits)) {
  for(model_name in names(corpus_fits[[corpus_name]])) {
    df <- bind_rows(df, 
                    tibble(corpus = corpus_name, 
                           model = model_name, 
                           fscore = corpus_fits[[corpus_name]][[model_name]]$bestval))
  }
}

df %>% ggplot(aes(y=model, x=fscore)) +
  facet_wrap(. ~ corpus) + geom_point() +
  theme_bw() + xlim(0,1)
ggsave("corpus_fits.pdf", width=7, height=3.5)