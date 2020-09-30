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
    mod = list(perf = model(parameters, ord=conds$train)$perf)
    SSE = sum( (mod$perf - conds$HumanItemAcc)^2 )
  } else {
    mod = list()
    SSE = 0
    totSs = 0
    for(i in 1:length(conds)) {
      #print(conds[[i]]$Condition)
      #print(parameters)
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
  fit = DEoptim(run_stochastic_model, lower=lower, upper=upper, DEoptim.control(reltol=.001, NP=100, itermax=30), 
                model_name=model_name, conds=conds, SSE_only=T)
  return(fit)
}

stochastic_models = c("guess-and-test","pursuit_detailed","trueswell2012","kachergis_sampling")

# for group fits (all conditions per model)
get_model_dataframe <- function(fits, conds) {
  mdf = tibble()
  for(model_name in names(fits)) {
    if(is.element(model_name, stochastic_models)) {
      pars = fits[[model_name]]$pars # change once we have DEoptim objs
      mdat = run_stochastic_model(conds, model_name, pars)
    } else {
      pars = fits[[model_name]]$optim$bestmem
      mdat = run_model(conds, model_name, pars)
    }
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

get_model_dataframe_cond_fits <- function(fits, conds) {
  mdf = tibble()
  SSE = 0
  for(model_name in names(fits)) {
    for(c in names(fits[[model_name]])) {
      pars = fits[[model_name]][[c]]$optim$bestmem
      SSE = fits[[model_name]][[c]]$optim$bestval
      
      if(is.element(model_name, stochastic_models)) {
        mdat = run_stochastic_model(conds, model_name, pars)
      } else {
        mdat = run_model(conds[[c]], model_name, pars)
      }
      Nitems = length(mdat$perf)
      HumanPerf = conds[[c]]$HumanItemAcc
      if(length(HumanPerf)==0) HumanPerf = rep(NA, Nitems)
      tmp = tibble(Model=rep(model_name, Nitems), 
                   condnum=rep(c, Nitems), 
                   Condition=rep(conds[[c]]$Condition, Nitems),
                   ModelPerf=as.vector(mdat$perf), 
                   HumanPerf=HumanPerf,
                   Nsubj=rep(conds[[c]]$Nsubj, Nitems))
      mdf = rbind(mdf, tmp)
    }
  }
  return(mdf)
}

combined_data = c(conds, orders)


do_group_fits <- function() {
  group_fits = list()
  group_fits[["kachergis"]] = fit_model("kachergis", combined_data, c(.001,.1,.5), c(5,15,1)) 
  group_fits[["novelty"]] = fit_model("novelty", combined_data, c(.001,.1,.5), c(5,15,1)) # NaN value of objective function!
  group_fits[["fazly"]] = fit_model("fazly", combined_data, c(1e-10,2), c(2,20000)) 
  group_fits[["Bayesian_decay"]] = fit_model("Bayesian_decay", combined_data, c(1e-5,1e-5,1e-5), c(10,10,10)) 
  group_fits[["strength"]] = fit_model("strength", combined_data, c(.001,.1), c(5,1))
  group_fits[["uncertainty"]] = fit_model("uncertainty", combined_data, c(.001,.1,.5), c(5,15,1))
  group_fits[["rescorla-wagner"]] = fit_model("rescorla-wagner", conds, c(1e-5,1e-5,1e-5), c(1,1,1))
  
  # ToDo: merge these in from group_stochastic_fits.Rdata
  # load("fits/group_stochastic_fits.Rdata")
  group_fits[["trueswell2012"]] = fit_stochastic_model("trueswell2012", combined_data, c(.0001,.0001), c(1,1))
  group_fits[["guess-and-test"]] = fit_stochastic_model("guess-and-test", combined_data, c(.0001,.0001), c(1,1))
  group_fits[["pursuit_detailed"]] = fit_stochastic_model("pursuit_detailed", combined_data, c(1e-5, 1e-5, 1e-5), c(1,1,1))
  group_fits[["kachergis_sampling"]] = fit_stochastic_model("kachergis_sampling", combined_data, c(.001,.1,.5), c(5,15,1))
  
  gfd = get_model_dataframe(group_fits, combined_data)
  save(group_fits, gfd, file="fits/group_fits.Rdata")
}


## TESTING
#pv = run_stochastic_model(conds, "trueswell2012", c(.1, .5)) # SSE=1.18
#gt = run_stochastic_model(conds, "guess-and-test", c(.1, .5)) # SSE=1.14
#pt = run_stochastic_model(conds, "pursuit_detailed", c(.2, .3, .05)) # SSE=4.06

#tl = fit_model("tilles", conds[[1]], c(1e-5,1e-5,1e-5), c(1,1,1)) # run_model



load("fits/group_fits.Rdata")
for(m in names(group_fits)) { 
  print(paste(m, round(group_fits[[m]]$optim$bestval, 2)))
}

# temporary (until we finish full fits)
#group_fits[["trueswell2012"]] = list(pars = c(0.113666, 0.266792)) # SSE=.879
#group_fits[["guess-and-test"]] = list(pars = c(0.691312, 0.991726)) # SSE=.884
#group_fits[["pursuit_detailed"]] = list(pars = c(0.088324, 0.409523, 0.000059)) # SSE = 0.996
#group_fits[["kachergis_sampling"]] = list(pars = c(0.15553, 11.04866, 0.99260)) # SSE = 0.407
#gfd = get_model_dataframe(group_fits, combined_data) # 726 items per model
#save(group_fits, gfd, file="fits/group_fits.Rdata")


# use for fitting given model to each condition
fit_by_cond <- function(mname, conds, lower, upper) {
  mod_fits = list()
  for(cname in names(conds)) {
    print(cname)
    mod_fits[[cname]] = fit_model(mname, conds[[cname]], lower, upper) 
  }
  return(mod_fits)
}

fit_stochastic_by_cond <- function(mname, conds, lower, upper) {
  mod_fits = list()
  for(cname in names(conds)) {
    print(cname)
    mod_fits[[cname]] = fit_stochastic_model(mname, conds[[cname]], lower, upper) 
  }
  return(mod_fits)
}


completed_cond_fits <- function() {
  cond_fits = list()
  cond_fits[["kachergis"]] = fit_by_cond(c("kachergis"), combined_data, c(.001,.1,.5), c(5,15,1))
  cond_fits[["uncertainty"]] = fit_by_cond(c("uncertainty"), combined_data, c(.001,.1,.5), c(5,15,1))
  cond_fits[["strength"]] = fit_by_cond("strength", combined_data, c(.001,.1), c(5,1))
  cond_fits[["novelty"]] = fit_by_cond(c("novelty"), combined_data, c(.001,.1,.5), c(5,15,1))
  cond_fits[["fazly"]] = fit_by_cond("fazly", combined_data, c(1e-10,2), c(2,20000)) 
  cond_fits[["Bayesian_decay"]] = fit_by_cond("Bayesian_decay", combined_data, c(1e-5,1e-5,1e-5), c(10,10,10)) 
  cond_fits[["rescorla-wagner"]] = fit_by_cond("rescorla-wagner", combined_data, c(1e-5,1e-5,1e-5), c(1,1,1))
  
  # stochastic models so far fitted with 20-30 iterations per cond -- should increase this (to 100, and perhaps refit a few times?)
  cond_fits[["trueswell2012"]] = fit_stochastic_by_cond("trueswell2012", combined_data, c(.0001,.0001), c(1,1))
  cond_fits[["guess-and-test"]] = fit_stochastic_by_cond("guess-and-test", combined_data, c(.0001,.0001), c(1,1))
  cond_fits[["pursuit_detailed"]] = fit_stochastic_by_cond("pursuit_detailed", combined_data, c(1e-5, 1e-5, 1e-5), c(1,1,1))
  cond_fits[["kachergis_sampling"]] = fit_stochastic_by_cond("kachergis_sampling", combined_data, c(.001,.1,.5), c(5,15,1))
  
  cfd <- get_model_dataframe_cond_fits(cond_fits, combined_data)
  save(cond_fits, cfd, file="fits/cond_fits.Rdata")
}



#models = c("kachergis", "fazly", "strength", "uncertainty", "novelty", "Bayesian_decay", "rescorla_wagner")

