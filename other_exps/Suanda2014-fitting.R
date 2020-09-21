# Suanda et al. 2014 - contextual diversity effects in children
# Low CD = .34 sd=.18
# Mod CD = .39 sd=.20
# Hi CD = .48 sd=.21
human = c(.34, .39, .48)

loCD = read.table("Suanda2014-lowCD.txt", header=F, sep='\t') 
medCD = read.table("Suanda2014-medCD.txt", header=F, sep='\t') 
hiCD = read.table("Suanda2014-hiCD.txt", header=F, sep='\t') 

require(DEoptim)

# for specific order format: w o o o 
coocs <- function(ord) { 
  voc_sz = max(unlist(ord))
  m = matrix(0, nrow=voc_sz, ncol=voc_sz)
  for(t in 1:nrow(ord)) {
    tr = unlist(ord[t,])
    m[tr,tr] = m[tr,tr] + 1
  }
  return(m)
}

coocs(loCD)
coocs(medCD)
coocs(hiCD)

source("models/model.R")

evalSSE <- function(par, human, verbose=F) {
  mod_perf = c( mean(model(par, loCD)$perf),
                mean(model(par, medCD)$perf),
                mean(model(par, hiCD)$perf) )
  if(verbose) print(mod_perf)
  return(sum((human-mod_perf)^2))
}

Suanda_fit <- DEoptim(fn=evalSSE, human=human, lower=c(.001, .1, .7), upper=c(2, 7, 1), 
                 DEoptim.control(reltol=.0001, steptol=50, itermax=200, trace=10))
evalSSE(c(0.044, 2.840, 0.999), human, verbose=T) # SSE=.004
# 0.38 0.41 0.43

# adult params get high performance
evalSSE( c(0.490592, 6.927328, 0.802789), human, verbose=T) 
