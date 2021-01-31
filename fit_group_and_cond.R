source("fitting_functions.R")

group_fits <- function() {
  group_fits = list()
  group_fits[["kachergis"]] = fit_model("kachergis", combined_data, c(.001,.1,.5), c(5,15,1)) 
  group_fits[["fazly"]] = fit_model("fazly", combined_data, c(1e-10,2), c(2,20000))
  group_fits[["novelty"]] = fit_model("novelty", combined_data, c(.001,.1,.5), c(5,15,1)) # NaN value of objective function!
  group_fits[["Bayesian_decay"]] = fit_model("Bayesian_decay", combined_data, c(1e-5,1e-5,1e-5), c(10,10,10)) 
  group_fits[["strength"]] = fit_model("strength", combined_data, c(.001,.1), c(5,1))
  group_fits[["uncertainty"]] = fit_model("uncertainty", combined_data, c(.001,.1,.5), c(5,15,1))
  group_fits[["rescorla-wagner"]] = fit_model("rescorla-wagner", combined_data, c(1e-5,1e-5,1e-5), c(1,1,1))
  #group_fits[["decay"]] = fit_model("decay", combined_data, .01, 1) # barely better than baseline
  
  # Fazly with threshold parameter
  group_fits[["fazlyt"]] = fit_model("fazlyt", combined_data, c(.01,2,.01), c(2,1000,1)) # .738
  
  # ToDo: re-fit these with Nsim=500 (instead of 200), and higher itermax
  # load("fits/group_stochastic_fits.Rdata")
  group_fits[["trueswell2012"]] = fit_stochastic_model("trueswell2012", combined_data, c(.0001,.0001), c(1,1))
  # bestvalit: 0.779708 bestmemit:    0.061586    0.339723
  group_fits[["guess-and-test"]] = fit_stochastic_model("guess-and-test", combined_data, c(.0001,.0001), c(1,1))
  # bestvalit: 0.787930 bestmemit:    0.843235    0.993573
  group_fits[["pursuit"]] = fit_stochastic_model("pursuit", combined_data, c(1e-5, 1e-5, 1e-5), c(1,1,1))
  # bestvalit: 1.009069 bestmemit:    0.167124    0.842535    0.001001
  group_fits[["kachergis_sampling"]] = fit_stochastic_model("kachergis_sampling", combined_data, c(.001,.1,.5), c(5,15,1))
  # bestvalit: 0.356301 bestmemit:    0.152348   11.209949    0.999401
  # Error in m[need_hypoths[w], new_hyps[w]] <- alpha : number of items to replace is not a multiple of replacement length
  
  # look into this warning: In if (!is.element(hypo, tr_o)) { ... :
  #  the condition has length > 1 and only the first element will be used
  gfd = get_model_dataframe(group_fits, combined_data)
  save(group_fits, gfd, file="fits/group_fits.Rdata")
}


cond_fits <- function() {
  cond_fits = list()
  cond_fits[["kachergis"]] = fit_by_cond(c("kachergis"), combined_data, c(.001,.1,.5), c(5,15,1))
  cond_fits[["uncertainty"]] = fit_by_cond(c("uncertainty"), combined_data, c(.001,.1,.5), c(5,15,1))
  cond_fits[["strength"]] = fit_by_cond("strength", combined_data, c(.001,.1), c(5,1))
  cond_fits[["novelty"]] = fit_by_cond(c("novelty"), combined_data, c(.001,.1,.5), c(5,15,1))
  cond_fits[["fazly"]] = fit_by_cond("fazly", combined_data, c(1e-10,2), c(2,20000)) 
  cond_fits[["Bayesian_decay"]] = fit_by_cond("Bayesian_decay", combined_data, c(1e-5,1e-5,1e-5), c(10,10,10)) 
  cond_fits[["rescorla-wagner"]] = fit_by_cond("rescorla-wagner", combined_data, c(1e-5,1e-5,1e-5), c(1,1,1))
  # something's gotta be wrong with R-W
  cfd <- get_model_dataframe_cond_fits(cond_fits, combined_data)
  save(cond_fits, cfd, file="fits/cond_fits.Rdata")
  
  # stochastic models so far fitted with 20-30 iterations per cond -- should increase this (to 100, and perhaps refit a few times?)
  cond_fits[["trueswell2012"]] = fit_stochastic_by_cond("trueswell2012", combined_data, c(.0001,.0001), c(1,1))
  cond_fits[["guess-and-test"]] = fit_stochastic_by_cond("guess-and-test", combined_data, c(.0001,.0001), c(1,1))
  cond_fits[["pursuit"]] = fit_stochastic_by_cond("pursuit", combined_data, c(1e-5, 1e-5, 1e-5), c(1,1,1))
  cond_fits[["kachergis_sampling"]] = fit_stochastic_by_cond("kachergis_sampling", combined_data, c(.001,.1,.5), c(5,15,1))
  
  cfd <- get_model_dataframe_cond_fits(cond_fits, combined_data)
  save(cond_fits, cfd, file="fits/cond_fits.Rdata")
}

cond_fits()