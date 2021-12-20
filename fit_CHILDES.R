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

#source("models/kachergis.R")
#run_corpus_model(group_fits$kachergis$optim$bestmem, FGT_corpus$train) # .83
#run_corpus_model(group_fits$kachergis$optim$bestmem, FGT_corpus$train, gold_lexicon = FGT_corpus$gold_lexicon) # 0.61
#run_corpus_model(group_fits$kachergis$optim$bestmem, FM_corpus$train) # .82

#optimize_corpus_fscore(FGT_corpus$train, "Bayesian_decay", gold_lexicon = FGT_corpus$gold_lexicon)

for (model_name in determ_models) {
  fit = optimize_corpus_fscore(FGT_corpus$train, model_name, load_fits = T, 
                               gold_lexicon=FGT_corpus$gold_lexicon)
  corpus_fits[["FGT"]][model_name] = fit
  save(corpus_fits, file=here("fits","FGT_FM_fits.Rdata"))
}

#determ_models = c("kachergis", "uncertainty") # re-fit for FM
group_fits$kachergis$member$upper = c(.5, 11, 1)
group_fits$uncertainty$member$upper = c(.5, 11, 1)

for (model_name in determ_models) {
  fit = optimize_corpus_fscore(FM_corpus$train, model_name, load_fits = F,
                               gold_lexicon=FM_corpus$gold_lexicon)
  corpus_fits[["FM"]][model_name] = fit
  save(corpus_fits, file=here("fits","FGT_FM_fits.Rdata"))
}





# fit stochastic models
load(here("fits","FGT_FM_stoch_fits.Rdata"))
# kachergis_sampling 
# Iteration: 26 bestvalit: 0.696970 bestmemit:    0.107670    0.238341    0.071873
# Iteration: 59 bestvalit: 0.677419 bestmemit:    0.136058    0.184348    0.123132

for (model_name in stochastic_models) {
  fit = optimize_corpus_fscore(FGT_corpus$train, model_name, 
                               gold_lexicon = FGT_corpus$gold_lexicon)
  corpus_fits[["FGT"]][model_name] = fit
  save(corpus_fits, file=here("fits","FGT_FM_stoch_fits.Rdata"))
}

for (model_name in stochastic_models) {
  fit = optimize_corpus_fscore(FM_corpus$train, model_name) # ToDo: get gold_lexicon
  corpus_fits[["FM"]][model_name] = fit
  save(corpus_fits, file=here("fits","FGT_FM_stoch_fits.Rdata"))
}

get_corpus_fits_df <- function(corpus_fits) {
  df <- tibble()
  for(corpus_name in names(corpus_fits)) {
    for(model_name in names(corpus_fits[[corpus_name]])) {
      df <- bind_rows(df, 
                      tibble(corpus = corpus_name, 
                             model = model_name, 
                             fscore = 1-corpus_fits[[corpus_name]][[model_name]]$bestval))
    }
  }
  return(df)
}

df <- get_corpus_fits_df(corpus_fits)
df %>% ggplot(aes(y=model, x=fscore)) +
  facet_wrap(. ~ corpus) + geom_point() +
  theme_bw() + xlim(0,1)
ggsave("corpus_fits.pdf", width=7, height=3.5)