source("fitting_functions.R")
require(tibble)

cv_group_fits <- function() {
  cv_group_fits = list()
  cv_group_fits[["kachergis"]] = cross_validated_group_fits("kachergis", combined_data, c(.001,.1,.5), c(5,15,1))
  cv_group_fits[["novelty"]] = cross_validated_group_fits("novelty", combined_data, c(.001,.1,.5), c(5,15,1))
  cv_group_fits[["fazly"]] = cross_validated_group_fits("fazly", combined_data, c(1e-10,2), c(2,20000)) 
  
  cv_group_fits[["Bayesian_decay"]] = cross_validated_group_fits("Bayesian_decay", combined_data, c(1e-5,1e-5,1e-5), c(10,10,10)) 
  cv_group_fits[["strength"]] = cross_validated_group_fits("strength", combined_data, c(.001,.1), c(5,1))
  cv_group_fits[["uncertainty"]] = cross_validated_group_fits("uncertainty", combined_data, c(.001,.1,.5), c(5,15,1))
  cv_group_fits[["rescorla-wagner"]] = cross_validated_group_fits("rescorla-wagner", combined_data, c(1e-5,1e-5,1e-5), c(1,1,1))
  
  cv_group_fits[["guess-and-test"]] = cross_validated_group_fits("guess-and-test", combined_data, c(.0001,.0001), c(1,1))
  cv_group_fits[["trueswell2012"]] = cross_validated_group_fits("trueswell2012", combined_data, c(.0001,.0001), c(1,1))
  # In if (!is.element(hypo, tr_o)) { ... :
  # the condition has length > 1 and only the first element will be used
  cv_group_fits[["pursuit_detailed"]] = cross_validated_group_fits("pursuit_detailed", combined_data, c(1e-5, 1e-5, 1e-5), c(1,1,1))
  # Error in m[novel[w], min_ref] <- gamma : 
  # number of items to replace is not a multiple of replacement length
  #cv_group_fits[["kachergis_sampling"]] = cross_validated_group_fits("kachergis_sampling", combined_data, c(.001,.1,.5), c(5,15,1))
  
  save(cv_group_fits, file="fits/cv_group_fits.Rdata")
}

#cv_group_fits()
load("fits/cv_group_fits.Rdata")
#cv_group_fits[["guess-and-test"]] = cross_validated_group_fits("guess-and-test", combined_data, c(.0001,.0001), c(1,1))
cv_group_fits[["kachergis_sampling"]] = cross_validated_group_fits("kachergis_sampling", combined_data, c(.001,.1,.5), c(5,15,1))
# Error in temp_wts * nent : non-conformable arrays

save(cv_group_fits, file="fits/cv_group_fits.Rdata")
#for(m in names(cv_group_fits) print(paste(cv_group_fits[[m]]$
                                          