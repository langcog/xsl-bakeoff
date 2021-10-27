source("load_corpus_data.R")
source("ROC.R")
source("optimize_corpus_fscores.R")

#load in group_fits before running/sourcing

determ_models = c("Bayesian_decay", "fazly", "kachergis", "novelty",
                       "rescorla-wagner", "strength", "uncertainty")
stochastic_models = c("guess-and-test","pursuit","trueswell2012","kachergis_sampling")

for (model_name in determ_models) {
  assign(paste0("fgt_fit_", model_name), 
         optimize_corpus_fscore(fgt_ord, model_name))
  save(fit, file=paste0("CHILDES_fits/fgt_", model_name, ".Rdata"))
}

for (model_name in determ_models) {
  assign(paste0("fm_fit_", model_name), 
         optimize_corpus_fscore(fm_ord, model_name))
  save(fit, file=paste0("CHILDES_fits/fm_", model_name, ".Rdata"))
}