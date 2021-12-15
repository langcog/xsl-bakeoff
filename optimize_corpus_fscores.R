require(DEoptim)
require(here)

model_dir = "models"
model_dir_stoch = here("models","stochastic")
fits_dir = here("fits","group_fits.Rdata")

# refer as needed to run_model
run_corpus_model <- function(parms, corpus, Fscore_only=T, gold_lexicon=c()) {
  model_out = model(parms, corpus)
  
  # row-normalize matrix?
  fscore = get_roc_max(model_out$matrix, gold_lexicon)
  if(Fscore_only) { 
    return(1-fscore) 
  } else {
    roc <- get_roc(model_out$matrix, fscores_only = F, gold_lexicon = gold_lexicon)
    return(roc)
  }
}

run_corpus_model_stochastic <- function(parms, corpus, Fscore_only=T, Nsim = 500, gold_lexicon=c()) {
  model_matrix = model(parms, corpus)$matrix
  
  for (i in 1:Nsim) {
    model_matrix = model_matrix + model(parms, corpus)$matrix
  }
  fscore = get_roc_max(model_matrix, gold_lexicon)
  if(Fscore_only) { 
    return(1-fscore) 
  } else {
    #return(get_fscore(model_matrix, fscore_only = F))
    roc <- get_roc(model_matrix, fscores_only = F, gold_lexicon = gold_lexicon)
    return(roc)
  }
}


# corpora = list(FM, FGT)
optimize_corpus_fscore <- function(corpus, model_name, load_fits = F, gold_lexicon = c()) {
  stochastic_models = c("guess-and-test","pursuit","trueswell2012","kachergis_sampling")
  if (model_name %in% stochastic_models) {
    source(here(model_dir_stoch, paste0(model_name,".R")))
  } else {
    source(here(model_dir, paste0(model_name,".R")))
  }
  if(load_fits) load(fits_dir)
  lower = group_fits[[model_name]]$member$lower
  upper = group_fits[[model_name]]$member$upper
  fit = DEoptim(run_corpus_model, lower=lower, upper=upper, DEoptim.control(reltol=.001, NP=100, itermax=100), 
                corpus=corpus, Fscore_only=T, gold_lexicon=gold_lexicon) # maximize Fscore (or minimize 1-F)
  if (model_name %in% stochastic_models) {
    fit = DEoptim(run_corpus_model_stochastic, lower=lower, upper=upper, DEoptim.control(reltol=.001, NP=100, itermax=100), 
                  corpus=corpus, Fscore_only=T, gold_lexicon=gold_lexicon) # maximize Fscore (or minimize 1-F)
  } else {
    fit = DEoptim(run_corpus_model, lower=lower, upper=upper, DEoptim.control(reltol=.001, NP=100, itermax=100), 
                  corpus=corpus, Fscore_only=T, gold_lexicon=gold_lexicon) # maximize Fscore (or minimize 1-F)
  }
  return(fit)
}