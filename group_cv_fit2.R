source("fitting_functions.R")

load("fits/cv_group_fits.Rdata")
cv_group_fits[["trueswell2012"]] = cross_validated_group_fits("trueswell2012", combined_data, c(.0001,.0001), c(1,1))
save(cv_group_fits, file="fits/cv_group_fits.Rdata")

#tcvfit <- cv_group_fits[["trueswell2012"]]
#save(tcvfit, file="new_trueswell2012_CV_fit.Rdata")

# need to reload cv_group_fits and save tcvfit in trueswell2012
cv_group_fits[["trueswell2012"]] = tcvfit

cv_group_fits$`guess-and-test`$pars
# old pars:
#      par1      par2
# [1,] 0.5374030 0.9610217
# [2,] 0.6119614 0.9942667
# [3,] 0.5291767 0.9496118
# [4,] 0.5877618 0.9760411
# [5,] 0.5302537 0.9435964

# new pars:
#     par1      par2
#[1,] 0.5446026 0.9958691
#[2,] 0.5090271 0.9515360
#[3,] 0.6398130 0.9991075
#[4,] 0.5605571 0.9842598
#[5,] 0.6023180 0.9816076

#calc old g-n-t CV acc before we throw it out
old_gnt = cv_group_fits$`guess-and-test`

df <- cv_group_fits$`guess-and-test`$testdf
df$SSE = (df$ModelPerf - df$HumanPerf)^2
sum(df$SSE) # 31.49 old, 31.34 new



# old trueswell
df <- cv_group_fits$`trueswell2012`$testdf
df$SSE = (df$ModelPerf - df$HumanPerf)^2
sum(df$SSE) # 42.92 old, 31.03 new