source("fitting_functions.R")

load("fits/cv_group_fits.Rdata")
cv_group_fits[["pursuit_detailed"]] = cross_validated_group_fits("pursuit_detailed", combined_data, c(1e-5, 1e-5, 1e-5), c(1,1,1))
save(cv_group_fits, file="fits/cv_group_fits.Rdata")

cv_group_fits[["kachergis_sampling"]] = cross_validated_group_fits("kachergis_sampling", combined_data, c(.001,.1,.5), c(5,15,1))
