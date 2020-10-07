# simple cooccurrence-counting baseline model

model <- function(params, ord, reps=1) {
  C = params # decay 
  voc_sz = max(unlist(ord$words), na.rm=TRUE) # vocabulary size
  ref_sz = max(unlist(ord$objs), na.rm=TRUE) # number of objects
  traj = list()
  m <- matrix(0, voc_sz, ref_sz) # association matrix
  perf = matrix(0, reps, voc_sz) # a row for each block
  # training
  for(rep in 1:reps) { # for trajectory experiments, train multiple times
    for(t in 1:nrow(ord$words)) { 
      tr_w = as.integer(ord$words[t,])
      tr_w = tr_w[!is.na(tr_w)]
      tr_o = as.integer(ord$objs[t,])
      tr_o = tr_o[!is.na(tr_o)]
      
      m = m*C # bestvalit: 0.618814 bestmemit:    0.990905
      m[tr_w,tr_o] = m[tr_w,tr_o] + 1 
      
      index = (rep-1)*nrow(ord$words) + t # index for learning trajectory
      traj[[index]] = m
    }
    perf[rep,] = diag(m) / rowSums(m)
  }
  want = list(perf=perf, matrix=m, traj=traj)
  return(want)
}

