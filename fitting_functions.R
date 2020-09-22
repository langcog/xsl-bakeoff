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
      print(conds[[i]]$Condition)
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

gt = run_stochastic_model(conds, "guess-and-test", c(.1, .5)) # perf[rep,] = diag(m) size mismatch "3x4 +6o"

pt = run_stochastic_model(conds, "pursuit_detailed", c(.2, .3, .05))

st = fit_model("strength", conds[[1]], c(.001,.1), c(5,1)) # run_model

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
    for(cname in conds) {
      mod_fits[[cname]] = fit_model(mname, conds[[cname]], c(.001,.1,.5), c(5,15,1)) 
    }
    fits[[mname]] = mod_fits
  }
  return(fits)
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
# Bayesian_decay  24.5 0.588
# fazly           66.3 0.134
# kachergis       22.2 0.647
# strength        45.8 0.276
# uncertainty     34.9 0.546

# fix the below, (and tilles)

# group_fits[["novelty"]] = fit_model("novelty", conds, c(.001,.1,.5), c(5,15,1))
# NaN value of objective function

group_fits[["rescorla-wagner"]] =  fit_model("rescorla-wagner", conds, c(.0001,.1, .1), c(10,1,10))


#models = c("kachergis", "fazly", "strength", "uncertainty", "novelty", "Bayesian_decay", "rescorla_wagner")





#mr = run_model(conds, "fazly", c(.0001,8000,.7), print_perf=T)
#mr = run_model(conds, "kachergis", c(1,3,.97), print_perf=T)
#mod = run_model(conds[["201"]], "kachergis", c(1,3,.97), print_perf=T)
#mod = run_model(conds[["201"]], "strength", c(1,.97), print_perf=T)

#coocs3x4 = make_cooccurrence_matrix(conds[["201"]], print_matrix=T, heatmap_filename="201")
#filt3e6l = make_cooccurrence_matrix(orders[["filt3E_6L"]])


#rwo = fit_model("rescorla-wagner", conds[conds_with_data], c(.0001,.1, .1), c(10,1,10))
#rww = fit_model("rescorla-wagner_words_cues", conds[conds_with_data], c(.0001,.1, .1), c(10,1,10))

#run_model(orders[["freq369-3x3hiCD"]], "Bayesian_decay", c(5.569798, 8.028788, 3.048987), print_perf=T)

faz_parms = c(.0001, 1000, .7)
f = run_model(orders[["freq369-3x3hiCD"]], "fazly", faz_parms, print_perf=T)
# according to Fazly et al. should be: .84 .94 .88 .75 .80 .89 .92 .95 .93 .98 .89 .92 .92 .98 .88 .88 .98 .85
faz_perf = c(.84, .94, .88, .75, .80, .89, .92, .95, .93, .98,.89,.92,.92,.98,.88,.88,.98,.85)
# freq3: .85 freq6: .93  freq9: .92

f = run_model(orders[["freq369-3x3hiCD"]], "fazly", faz_parms, print_perf=T)
cor(faz_perf, f$perf) # .97! really close...

# with no threshold, and with dummy word
fazf = fit_model("fazly", orders[condnames], c(1e-12,5,1e-12), c(.5,60000,1))
faza = fit_model("fazly", conds[conds_with_data], c(1e-12,5,1e-12), c(.5,60000,1))

# with threshold and with dummy word
fazf = fit_model("fazly", orders[condnames], c(1e-12,5,1e-12), c(.5,60000,1))
faza = fit_model("fazly", conds[conds_with_data], c(1e-12,5,1e-12), c(.5,60000,1))
f3 = run_model(orders[["freq369-3x3hiCD"]], "fazly", c(0.01228797, 169.3632, 0.6993908), print_perf=T)
mean(f3$perf[1:6]) # .43
mean(f3$perf[7:12]) # .55
mean(f3$perf[13:18]) # .65

# no threshold no dummy word
fazf = fit_model("fazly", orders[condnames], c(1e-12,5,1e-12), c(.5,60000,1))
faza = fit_model("fazly", conds[conds_with_data], c(1e-12,5,1e-12), c(.5,60000,1))

multinomial_likelihood_perfect <- function(par, ord) {
	M = model(par, ord=ord)
	pOgW = diag(M) / rowSums(M) # p(o|w)
	lik = sum(log(pOgW))
	return(-lik) # 18*log(1/18) = -52.02669 for AFC guessing
}

multinomial_likelihood <- function(cdat, M) {
	M = M / rowSums(M) # p(o|w)
	lik = 0
	for(i in 1:dim(cdat)[1]) { 
		wordi = cdat[i,"Word"]
		response_probs = M[wordi,unlist(cdat[i,c("Obj1","Obj2","Obj3","Obj4")])] # strengths of test objects
		resp = cdat[wordi,]$Response  #resp = cdat[which(cdat$Word==i),]$Response
		lik = lik + log(M[wordi,resp] / sum(response_probs)) 
	}
	return(-lik) # minimize -loglik
}


binomial_likelihood <- function(cdat, M) {
	est_prob = diag(M) / rowSums(M) # prob correct
	lik = 0
	for(i in 1:length(cdat)) { # dim(M)[1]
		resp = cdat[i]
		if(resp==i) {
			lik = lik + log(est_prob[i])
		} else {
			lik = lik + log(1-est_prob[i])
		}
	}
	return(lik) # 18*log(1/18) = -52.02669 for guessing
}


fit_subj <- function(par, ord, sdat) {
	tot_lik = 0
	M <- model(par, ord=ord)
	# need: for each word (CorrectAns), what object they chose (Response)
	mlik = binomial_likelihood(sdat, M)
	return(-mlik)
}

fit_all <- function(par, ord, dat) {
	tot_lik = 0
	for(s in dim(dat)[1]) {
		sdat <- unlist(dat[s,])
		M <- model(par, ord=ord)
		# need: for each word (CorrectAns), what object they chose (Response)
		tot_lik = tot_lik + binomial_likelihood(sdat, M)
	}
	mlik = tot_lik #/ length(unique(dat$Subject))
	return(-mlik)
}