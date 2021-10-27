require(DEoptim)
require(here)

model_dir = "models/"
fits_dir = "fits/group_fits.Rdata"

# refer as needed to run_model
run_corpus_model <- function(parms, corpora, Fscore_only=T) {
  model_out = model(parms, corpora)
  fscore = get_roc_max(model_out)
  if(Fscore_only) { 
    return(1-fscore) 
  } else {
    return(list(fscore=fscore, precision=precision, recall=recall))
  }
}


# corpora = list(FM, FGT)
optimize_corpus_fscore <- function(corpora, model_name, load_fits = F) {
  source(here(paste0(model_dir, model_name,".R")))
  if(load_fits) source(here(fits_dir))
  lower = group_fits[[model_name]]$member$lower
  upper = group_fits[[model_name]]$member$upper
  fit = DEoptim(run_corpus_model, lower=lower, upper=upper, DEoptim.control(reltol=.001, NP=100, itermax=100), 
                corpora=corpora, Fscore_only=T) # maximize Fscore (or minimize 1-F)
  return(fit)
}
