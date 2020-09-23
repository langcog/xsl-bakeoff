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


mafc_test <- function(mperf, test) {
  perf = rep(0, length(test$trials))
  for(i in 1:length(test$trials)) {
    w = test$trials[[i]]$word
    denom = sum(mperf[w, test$trials[[i]]$objs])
    perf[i] = mperf[w,w] / denom
  }
  return(perf)
}


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
      if(!is.null(conds[[i]]$test)) {
        mperf = mafc_test(mp$matrix, conds[[i]]$test)
        mod[[names(conds)[i]]] = mperf
      } else {
        mperf = mp$perf
        mod[[names(conds)[i]]] = mperf
      }
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

fit_model <- function(model_name, conds, lower, upper) {
  fit = DEoptim(run_model, lower=lower, upper=upper, DEoptim.control(reltol=.001, NP=100, itermax=100), 
                model_name=model_name, conds=conds, SSE_only=T)
  return(fit)
}

stochastic_dummy <- function(n, parameters, ord) {
  return(model(parameters, ord)$perf)
}

stochastic_matrix_dummy <- function(n, parameters, ord) {
  return(model(parameters, ord)$matrix)
}


run_stochastic_model <- function(conds, model_name, parameters, SSE_only=F, print_perf=F, Nsim=200) {
  source(paste0(model_dir,"stochastic/",model_name,".R"))
  # fitting a single condition
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
      # 4AFC conditions use different testing
      if(!is.null(conds[[i]]$test)) {
        mp = lapply(1:Nsim, stochastic_matrix_dummy, parameters=parameters, 
                    ord=conds[[i]]$train)
        mperf = Reduce('+', mp)
        mperf = mafc_test(mperf, conds[[i]]$test)
      } else {
        mp = sapply(1:Nsim, stochastic_dummy, parameters=parameters, ord=conds[[i]]$train)
        mperf = rowSums(mp) / ncol(mp)
      }
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
  fit = DEoptim(run_stochastic_model, lower=lower, upper=upper, DEoptim.control(reltol=.001, NP=100, itermax=20), 
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



do_group_fits <- function() {
  group_fits = list()
  group_fits[["kachergis"]] = fit_model("kachergis", combined_data, c(.001,.1,.5), c(5,15,1)) 
  # group_fits[["novelty"]] = fit_model("novelty", combined_data, c(.001,.1,.5), c(5,15,1)) # NaN value of objective function!
  group_fits[["fazly"]] = fit_model("fazly", combined_data, c(1e-10,2), c(2,20000)) 
  group_fits[["Bayesian_decay"]] = fit_model("Bayesian_decay", combined_data, c(1e-5,1e-5,1e-5), c(10,10,10)) 
  group_fits[["strength"]] = fit_model("strength", combined_data, c(.001,.1), c(5,1))
  group_fits[["uncertainty"]] = fit_model("uncertainty", combined_data, c(.001,.1,.5), c(5,15,1))
  group_fits[["rescorla-wagner"]] = fit_model("rescorla-wagner", conds, c(1e-5,1e-5,1e-5), c(1,1,1))
  gfd = get_model_dataframe(group_fits, combined_data)
  save(group_fits, gfd, file="fits/group_fits.Rdata")
}


## TESTING

#pv = run_stochastic_model(conds, "trueswell2012", c(.1, .5)) # SSE=1.18

#gt = run_stochastic_model(conds, "guess-and-test", c(.1, .5)) # SSE=1.14
# so far: Iteration: 10 bestvalit: 0.932846 bestmemit:    0.685980    0.994259

#pt = run_stochastic_model(conds, "pursuit_detailed", c(.2, .3, .05)) # SSE=4.06


nv = fit_model("novelty", conds[[1]], c(.001,.1,.5), c(5,15,1)) # run_model

##### NOT RUN YET
load("fits/group_fits.Rdata")

group_fits[["trueswell2012"]] = fit_stochastic_model("trueswell2012", combined_data, c(.0001,.0001), c(1,1))
# trueswell2012 SSE = 0.878519  c(0.113666, 0.266792)
group_fits[["guess-and-test"]] = fit_stochastic_model("guess-and-test", combined_data, c(.0001,.0001), c(1,1))
group_fits[["pursuit_detailed"]] = fit_stochastic_model("pursuit_detailed", combined_data, c(1e-5, 1e-5, 1e-5), c(1,1,1))

gfd = get_model_dataframe(group_fits, combined_data)

save(group_fits, gfd, file="fits/group_fits.Rdata")


# use for fitting given model to each condition
fit_by_cond <- function(mname, conds, lower, upper) {
  mod_fits = list()
  for(cname in names(conds)) {
    mod_fits[[cname]] = fit_model(mname, conds[[cname]], lower, upper) 
  }
  return(mod_fits)
}

do_cond_fits <- function() {
  cond_fits = list()
  cond_fits[["kachergis"]] = fit_by_cond(c("kachergis"), combined_data, c(.001,.1,.5), c(5,15,1))
  cond_fits[["uncertainty"]] = fit_by_cond(c("uncertainty"), combined_data, c(.001,.1,.5), c(5,15,1))
  cond_fits[["fazly"]] = fit_by_cond("fazly", combined_data, c(1e-10,2), c(2,20000)) 
  cond_fits[["Bayesian_decay"]] = fit_by_cond("Bayesian_decay", combined_data, c(1e-5,1e-5,1e-5), c(10,10,10)) 
  cond_fits[["strength"]] = fit_model("strength", combined_data, c(.001,.1), c(5,1))
  cond_fits[["rescorla-wagner"]] = fit_model("rescorla-wagner", conds, c(1e-5,1e-5,1e-5), c(1,1,1))
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
# rescorla-wagner 42.9 0.593
# strength        46.3 0.275
# uncertainty     35.2 0.550

# fix the below, (and tilles)

# group_fits[["novelty"]] = fit_model("novelty", conds, c(.001,.1,.5), c(5,15,1))
# NaN value of objective function



#models = c("kachergis", "fazly", "strength", "uncertainty", "novelty", "Bayesian_decay", "rescorla_wagner")

