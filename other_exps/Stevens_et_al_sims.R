# trying to piece together Stevens et al 2017 experiments

# Their repo has Gleitman et al 2011 sample file: 
# PURSUIT-master/Experiments/Sample.csv
# should see if that's enough to use for modeling

# Koehne, Trueswell, & Gleitman 2013 - order effects:
# 2 train/test blocks, 8 words each; 1 word and 4 objects per trial; 
# 6 repetitions per word; 2 meanings per word: 1 present whenever the there; 
# one in only half the case; other objects appeared only once with any other word (17%)

# manipulated the order in which trials including and excluding the 50% referent 
# were presented within four levels (within participants): Firstly, the 50%-present (P) 
# and 50%-absent (A) trials could be either blocked (AAAPPP and PPPAAA) or not blocked 
# (APAPAP and PAPAPA); secondly, the first encounter of a noun could be either an 
# A trial (AAAPPP and APAPAP) or a P trial (PPPAAA and PAPAPA)
pppaaa = read.table("KoehneTrueswellGleitman2013-pppaaa.txt", header=T, sep='\t') # line/trial: w o1 o2 o3 o4
# each word appears in six consecutive trials; the first 3 appearances have the 50%-referent present, 
# and the later 3 trials are absent the 50%-referent
aaappp = read.table("KoehneTrueswellGleitman2013-aaappp.txt", header=T, sep='\t') 

apapap = read.table("KoehneTrueswellGleitman2013-apapap.txt", header=T, sep='\t') 
papapa = read.table("KoehneTrueswellGleitman2013-papapa.txt", header=T, sep='\t') 

require(DEoptim)

# for specific order format: w o o o 
coocs <- function(ord) { 
  nrefs = max(unlist(ord[,2:5]))
  nwords = max(unlist(ord[,1]))
  m = matrix(0, nrow=nwords, ncol=nrefs)
  for(t in 1:nrow(ord)) {
    tr = unlist(ord[t,])
    m[tr[1],tr[2:5]] = m[tr[1],tr[2:5]] + 1
  }
  return(m)
}

coocs(pppaaa)
coocs(aaappp)
coocs(apapap)
coocs(papapa)

library(plotrix)
heatmap(coocs(pppaaa)[8:1,], Rowv=NA, Colv=NA, labRow=8:1) # all orders look the same

source("models/model_1x4.R")
# Conditions PPPAAA: 34.4%; PAPAPA = 27.3% 
# and APAPAP 23.4%; not in AAAPPP 16.4%
# were tested 8AFC, but without the 100% (diagonal) referent available

run_model <- function(par, ord) {
  mp = model(par, ord)$m
  diag(mp[,1:8])/rowSums(mp[,1:8]) # 100% referemts: ~60% perf
  diag(mp) = 0
  perf = c()
  for(w in 1:nrow(mp)) {
    perf[w] = mp[w,w+1]/sum(mp[w,1:9])
  }
  return(perf) # ranging .17 - .24
}


evalSSE <- function(par, hum_perf, verbose=F) {
  mod_perf = c( mean(run_model(par, aaappp)),
                mean(run_model(par, apapap)),
                mean(run_model(par, papapa)),
                mean(run_model(par, pppaaa)) )
  if(verbose) print(mod_perf)
  return(sum((hum_perf-mod_perf)^2))
}

run_model(c(.1,1,.95), aaappp) # .19 - .36
run_model(c(.1,1,.95), pppaaa) # .43 - .56
evalSSE(c(.1,1,.95))

# Experiment 1 required clicking on a referent each training trial (response XSL)
exp1_hum_perf = c(.164, .234, .273, .344)
# Exp 2 was same conditions with just eye-tracking (passive XSL)
exp2_hum_perf = c(.195, .227, .188, .299) # PPPAAA: 29.9%, PAPAPA: 18.8%, APAPAP: 22.7%, AAAPPP: 19.5

best <- DEoptim(fn=evalSSE, hum_perf=exp1_hum_perf, lower=c(.001, .1, .7), upper=c(2, 7, 1), 
                DEoptim.control(reltol=.001, steptol=50, itermax=200, trace=10))
# the model predicts the same ordering as humans out of the box -- PPPAAA is best, AAAPPP is worst
# contradicts Koehne et al.'s interpretation: "This finding is inconsistent with a standard cross-situational 
# account because all conditions should have been above chance independent of presentation order.

# best fit: SSE = 0.001710  par = c(0.481103, 6.753934, 0.806177)
evalSSE( c(0.481103, 6.753934, 0.806177), exp1_hum_perf, verbose=T) 
# 0.1907993 0.2381775 0.2660204 0.3135700

# Fit Exp2
best2 <- DEoptim(fn=evalSSE, hum_perf=exp2_hum_perf, lower=c(.001, .1, .7), upper=c(2, 7, 1), 
                DEoptim.control(reltol=.001, steptol=50, itermax=200, trace=10))
evalSSE( c(0.490592, 6.927328, 0.802789), exp2_hum_perf, verbose=T)  # SSE=.0012
# .191 0.223 0.213 0.277

#evalSSE(c( 0.028632, 0.161190, 0.999978), exp2_hum_perf, verbose=T) # SSE=.004 

combined_hum_perf = (exp1_hum_perf + exp2_hum_perf) / 2 
# 0.1795 0.2305 0.2305 0.3215
best2 <- DEoptim(fn=evalSSE, hum_perf=combined_hum_perf, lower=c(.001, .1, .7), upper=c(2, 7, 1), 
                 DEoptim.control(reltol=.001, steptol=50, itermax=200, trace=10))
# SSE = .0003
evalSSE(c(0.483031,6.737743,0.806709), combined_hum_perf, verbose=T) 
# 0.1923499 0.2315942 0.2311730 0.3089080

# Pursuit (Steven et al 2017) - (Exp 1?)
# .16 .18 .32 .34