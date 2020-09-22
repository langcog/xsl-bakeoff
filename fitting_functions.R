require(DEoptim) # pso

#source("fit.R")

order_dir = "orders/"
model_dir = "models/"
data_dir = "data/"

# load lists of many trial orderings (and some means of human performance):
load("data/master_orders.RData") # orders
load("data/asymmetric_conditions.RData") # conds
print(names(orders)) # e.g., orders[["filt0E_3L"]]
print(names(conds))

load("data/filtering_item_acc.RData")
fd <- subset(acc_s, AtoA==T)
for(ord in unique(fd$order)) {
  css = subset(fd, order==ord)
  orders[[ord]]$Nsubj = css$n[1]
  orders[[ord]]$HumanItemAcc = css$Correct
}

totSs = 0
for(ord in names(orders)) {
  totSs = totSs + orders[[ord]]$Nsubj
  orders[[ord]]$Condition = ord
}
# 782 subjects

#load(paste(data_dir,"asym_master.RData",sep='')) # raw


#conds[["211"]]$Nsubj = 43 # 4AFC - should change model evaluation to use conds[["211"]]$test
#conds[["212"]]$Nsubj = 38 # 4AFC
#conds[["213"]]$Nsubj = 31 # 4AFC
#conds[["214"]]$Nsubj = 36 # 4AFC


for(ord in names(conds)) {
  if(!is.na(conds[[ord]]$Nsubj)) {
    totSs = totSs + conds[[ord]]$Nsubj
  } else print(ord) 
} 
# 1532 subjects



# run given model on 1 or more condition 
run_model <- function(conds, model_name, parameters, SSE_only=F, print_perf=F) {
	source(paste0(model_dir,model_name,".R"))
  if(!is.null(conds$train)) {
    mod = model(parameters, ord=conds$train)$perf
    SSE = sum( (mod - conds$HumanItemAcc)^2 )
  } else {
    mod = list()
    SSE = 0
    totSs = 0
    for(i in 1:length(conds)) {
      #print(conds[[i]]$Condition)
      mp = model(parameters, conds[[i]]$train)
      mod[[names(conds)[i]]] = mp$perf
      SSE = SSE + conds[[i]]$Nsubj * sum( (mp$perf - conds[[i]]$HumanItemAcc)^2 )
      totSs = totSs + conds[[i]]$Nsubj
    }
    SSE = SSE / totSs
  }
	if(print_perf) {
	  print(mod)
    print(paste0("SSE: ",SSE))
	}
  mod$SSE = SSE
  
  if(SSE_only) return(SSE)
	return(mod)
}

fit_model <- function(model_name, conds, lower, upper) {
  fit = DEoptim(run_model, lower=lower, upper=upper, DEoptim.control(reltol=.001, NP=100, itermax=100), 
                model_name=model_name, conds=conds, SSE_only=T)
  return(fit)
}

stochastic_dummy <- function(n, parameters, ord) {
  return(model(parameters, ord)$perf)
}

run_stochastic_model <- function(conds, model_name, parameters, SSE_only=F, print_perf=F, Nsim=200) {
  source(paste0(model_dir,"stochastic/",model_name,".R"))
  if(!is.null(conds$train)) {
    mp = sapply(1:Nsim, stochastic_dummy, parameters=parameters, ord=conds$train)
    mod = rowSums(mp) / ncol(mp)
    SSE = sum( (mod - conds$HumanItemAcc)^2 )
  } else {
    mod = list()
    SSE = 0
    totSs = 0
    for(i in 1:length(conds)) {
      #print(conds[[i]]$Condition)
      mp = sapply(1:Nsim, stochastic_dummy, parameters=parameters, ord=conds[[i]]$train)
      mperf = rowSums(mp) / ncol(mp)
      mod[[names(conds)[i]]] = mperf
      SSE = SSE + conds[[i]]$Nsubj * sum( (mperf - conds[[i]]$HumanItemAcc)^2 )
      totSs = totSs + conds[[i]]$Nsubj
    }
    SSE = SSE / totSs
  }
  if(print_perf) {
    print(mod)
    print(paste0("SSE: ",SSE))
  }
  mod$SSE = SSE
  
  if(SSE_only) return(SSE)
  return(mod)
}

fit_stochastic_model <- function(model_name, conds, lower, upper) {
  fit = DEoptim(run_stochastic_model, lower=lower, upper=upper, DEoptim.control(reltol=.001, NP=100, itermax=100), 
                model_name=model_name, conds=conds, SSE_only=T)
  return(fit)
}


get_model_dataframe <- function(fits, conds) {
  mdf = tibble()
  for(model_name in names(fits)) {
    pars = fits[[model_name]]$optim$bestmem
    mdat = run_model(conds, model_name, pars)
    for(c in names(mdat)) {
      if(c!="SSE") {
        Nitems = length(mdat[[c]])
        HumanPerf = conds[[c]]$HumanItemAcc
        if(length(HumanPerf)==0) HumanPerf = rep(NA, Nitems)
        tmp = tibble(Model=rep(model_name, Nitems), 
                     condnum=rep(c, Nitems), 
                     Condition=rep(conds[[c]]$Condition, Nitems),
                     ModelPerf=as.vector(mdat[[c]]), 
                     HumanPerf=HumanPerf,
                     Nsubj=rep(conds[[c]]$Nsubj, Nitems))
        mdf = rbind(mdf, tmp)
      }
    }
  }
  return(mdf)
}

combined_data = c(conds, orders)

group_fits = list()

already_run <- function() {
  group_fits[["kachergis"]] = fit_model("kachergis", combined_data, c(.001,.1,.5), c(5,15,1)) 
  group_fits[["fazly"]] = fit_model("fazly", combined_data, c(1e-10,2), c(2,20000)) 
  group_fits[["Bayesian_decay"]] = fit_model("Bayesian_decay", combined_data, c(1e-5,1e-5,1e-5), c(10,10,10)) 
  group_fits[["strength"]] = fit_model("strength", combined_data, c(.001,.1), c(5,1))
  group_fits[["uncertainty"]] = fit_model("uncertainty", combined_data, c(.001,.1,.5), c(5,15,1))
  gfd = get_model_dataframe(group_fits, combined_data)
  save(group_fits, gfd, file="fits/group_fits.Rdata")
}


## TESTING

#gt = run_stochastic_model(conds, "guess-and-test", c(.1, .5)) # SSE=1.14
#pt = run_stochastic_model(conds, "pursuit_detailed", c(.2, .3, .05)) # SSE=4.06
#st = fit_model("strength", conds[[1]], c(.001,.1), c(5,1)) # run_model

##### NOT RUN YET
load("fits/group_fits.Rdata")

group_fits[["guess-and-test"]] = fit_stochastic_model("guess-and-test", combined_data, c(.0001,.0001), c(1,1))
group_fits[["pursuit_detailed"]] = fit_stochastic_model("pursuit_detailed", combined_data, c(1e-5, 1e-5, 1e-5), c(1,1,1))

gfd = get_model_dataframe(group_fits, combined_data)

save(group_fits, gfd, file="fits/group_fits.Rdata")


# use for fitting each model to each condition
fit_by_cond <- function(models, conds) {
  fits = list()
  for(mname in models) {
    mod_fits = list()
    for(cname in names(conds)) {
      mod_fits[[cname]] = fit_model(mname, conds[[cname]], c(.001,.1,.5), c(5,15,1)) 
    }
    fits[[mname]] = mod_fits
  }
  return(fits)
}

already_run2 <- function() {
  cond_fits = list()
  cond_fits[["kachergis"]] = fit_by_cond(c("kachergis"), combined_data)
  cond_fits[["uncertainty"]] = fit_by_cond(c("uncertainty"), combined_data)
  # rewrite get_model_dataframe for 
  save(cond_fits, file="fits/cond_fits.Rdata")
}

gfd %>% ggplot(aes(x=ModelPerf, y=HumanPerf, group=Condition, color=Condition)) + 
  geom_point() + facet_wrap(vars(Model)) + theme_bw() 

#gfd %>% ggplot(aes(x=ModelPerf, y=HumanPerf, group=Model, color=Model)) + 
#  geom_point() + facet_wrap(vars(Condition)) + theme_bw() + geom_smooth(method = "lm")

require(tidyverse)
gfd %>% filter(!is.na(HumanPerf)) %>%
  group_by(Model) %>% 
  summarise(SSE = sum((ModelPerf-HumanPerf)^2),
            r = cor(ModelPerf, HumanPerf))
# Model            SSE     r
# Bayesian_decay  25.3 0.574
# fazly           23.7 0.622
# kachergis       22.6 0.643
# strength        46.3 0.275
# uncertainty     35.2 0.550

# fix the below, (and tilles)

# group_fits[["novelty"]] = fit_model("novelty", conds, c(.001,.1,.5), c(5,15,1))
# NaN value of objective function

#group_fits[["rescorla-wagner"]] = fit_model("rescorla-wagner", conds, c(.0001,.1, .1), c(10,1,10))


#models = c("kachergis", "fazly", "strength", "uncertainty", "novelty", "Bayesian_decay", "rescorla_wagner")

