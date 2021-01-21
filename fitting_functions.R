require(DEoptim) # pso
require(caret)

order_dir = "orders/"
model_dir = "models/"
data_dir = "data/"

#source("fit.R")

stochastic_models = c("guess-and-test","pursuit_detailed","trueswell2012","kachergis_sampling")

load("data/combined_data.RData")

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
    unweighted_SSE = 0
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
      unweighted_SSE = unweighted_SSE + sum( (mperf - conds[[i]]$HumanItemAcc)^2 )
      totSs = totSs + conds[[i]]$Nsubj
    }
    SSE = SSE / totSs
  }
	if(print_perf) {
	  print(mod)
    print(paste0("SSE: ",SSE))
    #print(paste0("unweighted SSE: ", unweighted_SSE)) # baseline: 31.84
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


run_stochastic_model <- function(conds, model_name, parameters, SSE_only=F, print_perf=F, get_resp_matrix=F, Nsim=200) {
  source(paste0(model_dir,"stochastic/",model_name,".R"))
  # fitting a single condition
  if(!is.null(conds$train)) {
    if(get_resp_matrix) {
      mp = lapply(1:Nsim, stochastic_matrix_dummy, parameters=parameters, 
                  ord=conds$train)
      mod = Reduce('+', mp)
    } else {
      mp = sapply(1:Nsim, stochastic_dummy, parameters=parameters, ord=conds$train)
      mod = rowSums(mp) / ncol(mp)
      SSE = sum( (mod - conds$HumanItemAcc)^2 )
    }
  } else {
    mod = list()
    SSE = 0
    totSs = 0
    for(i in 1:length(conds)) {
      # just want response matrix, not AFC performance / SSE
      if(get_resp_matrix) {
        mp = lapply(1:Nsim, stochastic_matrix_dummy, parameters=parameters, 
                    ord=conds[[i]]$train)
        mod[[names(conds)[i]]] = Reduce('+', mp)
      } else {
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
        SSE = SSE / totSs
      }
    
  }
  if(print_perf) {
    print(mod)
    print(paste0("SSE: ",SSE))
  }
  mod$SSE = SSE
  
  if(SSE_only) return(SSE)
  return(mod)
  }
}
  
fit_stochastic_model <- function(model_name, conds, lower, upper) {
  fit = DEoptim(run_stochastic_model, lower=lower, upper=upper, DEoptim.control(reltol=.001, NP=100, itermax=30), 
                model_name=model_name, conds=conds, SSE_only=T)
  return(fit)
}


# for group fits (all conditions per model)
get_model_dataframe <- function(fits, conds, cvdat=c()) {
  mdf = tibble()
  for(model_name in names(fits)) {
    if(length(cvdat)!=0) {
      mdat = cvdat
    } else {
      if(is.element(model_name, stochastic_models)) {
        pars = fits[[model_name]]$optim$bestmem 
        mdat = run_stochastic_model(conds, model_name, pars)
      } else {
        pars = fits[[model_name]]$optim$bestmem
        mdat = run_model(conds, model_name, pars)
      }
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
    print(model_name)
    for(c in names(fits[[model_name]])) {
      print(c)
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



## TESTING
#pv = run_stochastic_model(conds, "trueswell2012", c(.1, .5)) # SSE=1.18
#gt = run_stochastic_model(conds, "guess-and-test", c(.1, .5)) # SSE=1.14
#pt = run_stochastic_model(conds, "pursuit_detailed", c(.2, .3, .05)) # SSE=4.06

#tl = fit_model("tilles", conds[[1]], c(1e-5,1e-5,1e-5), c(1,1,1)) # run_model



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



# given a vector of indices and a list, remove all indexed items
get_train_test_split <- function(test_inds, conds) {
  dat = list(train = conds, test = list())
  for(i in test_inds) { 
    dat$train[[i]] = NULL 
    dat$test[[names(conds)[i]]] = conds[[i]]
  }
  return(dat)
}


# run fit_model / fit_stochastic_model for each of 5 subsets of combined_data conditions
# save the parameters...and whole dataframe?
cross_validated_group_fits <- function(model_name, combined_data, lower, upper) {
  #model_name = "kachergis"
  dat = list()
  test = list()
  testdf = tibble()
  set.seed(123)
  folds <- createFolds(names(combined_data), k = 5, list = T)
  cv_group_fits = list()
  for(i in 1:length(folds)) {
    conds = get_train_test_split(folds[[i]], combined_data)
    if(is.element(model_name, stochastic_models)) {
      opt = fit_stochastic_model(model_name, conds$train, lower, upper)
      test[[i]] = run_stochastic_model(conds$test, model_name, opt$optim$bestmem)
    } else {
      opt = fit_model(model_name, conds$train, lower, upper)
      test[[i]] = run_model(conds$test, model_name, opt$optim$bestmem)
    }
    dat[["pars"]] = rbind(dat[["pars"]], opt$optim$bestmem)
    dat[["train_acc"]] = c(dat[["train_acc"]], opt$optim$bestval)
    
    # add the data frame of test data? much more convenient..
    tmp = list()
    tmp[[model_name]] = opt
    testdf = rbind(testdf, get_model_dataframe(tmp, conds$test, cvdat=test[[i]]))
    print(paste("fold",i,"train SSE:",round(opt$optim$bestval,3),"test SSE:",round(test[[i]]$SSE,3)))
  }
  # add the folds ??
  dat[["test"]] = test
  dat[["testdf"]] = testdf # get_cv_test_df(test)
  return(dat)
}


# do for each model and save



# now we're going to fit each model to 80% of the items per condition (5-fold CV)
cross_validated_cond_fits <- function(combined_data) {
  # for each condition, select 80% of items for training SSE, fit to those, then test on remaining 20%
  for(c in names(combined_data)) {
    # modify run_model to check in each cond for "train_items" ?
  }
}


#models = c("kachergis", "fazly", "strength", "uncertainty", "novelty", "Bayesian_decay", "rescorla_wagner")

