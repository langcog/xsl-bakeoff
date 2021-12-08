require(DEoptim)
require(here)

model_dir = "models/"
model_dir_stoch = "models/stochastic/"
fits_dir = "fits/group_fits.Rdata"

# refer as needed to run_model
run_corpus_model <- function(parms, corpus, Fscore_only=T, gold_lexicon=c()) {
  model_out = model(parms, corpus)
  
  #if(length(gold_lexicon)==0) {
  #  gold_lexicon = diag(1, nrow=length(corpus$words), ncol=length(corpus$objs))
  #}
  
  # row-normalize matrix?
  fscore = get_roc_max(model_out$matrix, gold_lexicon)
  if(Fscore_only) { 
    return(1-fscore) 
  } else {
    roc <- get_roc(model_out$matrix, gold_lexicon)
    return(list(fscore=fscore, precision=precision, recall=recall))
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
    roc <- get_roc(model_out$matrix, gold_lexicon)
    return(list(fscore=fscore, precision=precision, recall=recall))
  }
}


# corpora = list(FM, FGT)
optimize_corpus_fscore <- function(corpora, model_name, load_fits = F) {
  stochastic_models = c("guess-and-test","pursuit","trueswell2012","kachergis_sampling")
  if (model_name %in% stochastic_models) {
    source(here(paste0(model_dir_stoch, model_name,".R")))
  } else {
    source(here(paste0(model_dir, model_name,".R")))
  }
  if(load_fits) source(here(fits_dir))
  lower = group_fits[[model_name]]$member$lower
  upper = group_fits[[model_name]]$member$upper
  fit = DEoptim(run_corpus_model, lower=lower, upper=upper, DEoptim.control(reltol=.001, NP=100, itermax=100), 
                corpus=corpus, Fscore_only=T) # maximize Fscore (or minimize 1-F)
  if (model_name %in% stochastic_models) {
    fit = DEoptim(run_corpus_model_stochastic, lower=lower, upper=upper, DEoptim.control(reltol=.001, NP=100, itermax=100), 
                  corpus=corpus, Fscore_only=T) # maximize Fscore (or minimize 1-F)
  } else {
    fit = DEoptim(run_corpus_model, lower=lower, upper=upper, DEoptim.control(reltol=.001, NP=100, itermax=100), 
                  corpus=corpus, Fscore_only=T) # maximize Fscore (or minimize 1-F)
  }
  return(fit)
}