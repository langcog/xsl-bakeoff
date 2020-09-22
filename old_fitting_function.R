

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