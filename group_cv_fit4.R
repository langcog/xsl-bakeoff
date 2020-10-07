source("fitting_functions.R")

load("fits/cv_group_fits.Rdata")
cv_group_fits[["kachergis_sampling"]] = cross_validated_group_fits("kachergis_sampling", combined_data, c(.001,.1,.5), c(5,15,1))
save(cv_group_fits, file="fits/cv_group_fits.Rdata")