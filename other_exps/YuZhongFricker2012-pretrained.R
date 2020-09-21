# - Yu Zhong Fricker 2012: 
# orig4x4, but 3 pairs (which ones??) are pretrained: accuracy on those is .9187, 
# and accuracy on the other 15 is .5812 
# How variable is this effect based on selecting different 3 items for retraining?)
emp = c(.9187, .5812)

orig4x4 <- read.table("ords/orig_order4x4.txt")

source("models/model.R")

run_pretrain_experiment <- function(parms, n_pretrained=3, start_val=1) {
  voc_sz = 18
  start_matrix = matrix(0, voc_sz, voc_sz)
  pretrain_inds = sample(1:18, n_pretrained)
  other_inds = setdiff(1:18, pretrain_inds)
  #start_matrix[pretrain_inds, pretrain_inds] = .01 # irrelevant to seed off-diagonal or not
  diag(start_matrix)[pretrain_inds] = start_val
  mm = model(parms, orig4x4, start_matrix = start_matrix) 

  return(c(mean(mm$perf[pretrain_inds]), mean(mm$perf[other_inds])))
}

run_experiment_batch <- function(parms, n_pretrained=3, start_val=1, Nsim=100) {
  dat = data.frame()
  for(i in 1:Nsim) {
    dat = rbind(dat, round(run_pretrain_experiment(parms, n_pretrained=n_pretrained, start_val=start_val), 3))
  }
  names(dat) = c("pretrain", "other")
  return(dat)
}

mean(model(c(.1, 2, .92), orig4x4)$perf) # .45

pt3 = run_experiment_batch(c(.1, 2, .92), n_pretrained=3, start_val=1.6, Nsim=200)
colMeans(pt3) # pretrained = .89, other = .55

sum((colMeans(pt3) - emp)^2) # .002

pt6 = run_experiment_batch(c(.1, 2, .92), n_pretrained=6, start_val=1.6, Nsim=200)
colMeans(pt6) # pretrained = .89, other = .66

pt9 = run_experiment_batch(c(.1, 2, .92), n_pretrained=9, start_val=1.6, Nsim=200)
colMeans(pt9) # pretrained = .89, other = .76
