source("fitting_functions.R")

load("fits/cv_group_fits.Rdata")
cv_group_fits[["guess-and-test"]] = cross_validated_group_fits("guess-and-test", combined_data, c(.0001,.0001), c(1,1))
save(cv_group_fits, file="fits/cv_group_fits.Rdata")

