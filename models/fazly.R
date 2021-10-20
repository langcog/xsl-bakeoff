# reimplemented following Fazly et al.'s code + paper
# George Kachergis June 11, 2015

model <- function(params, ord=c(), reps=1) {
  lambda <- params[1] # small smoothing factor (1e-5)
  beta <- params[2] # upper bound on number of symbol types to expect? (8500)
  #theta <- params[3] # threshold for knowledge (they used .7)
  # best-fitting pars with threshold: c(0.077884, 5.553602, 0.188449) group SSE=.72
  
  # for alignment prob calculation Fazly et al use two extra fixed parameters for smoothing:
  # (lines 174-5 learn.py)
  alpha = 10
  epsilon = 0.001
  voc = unique(unlist(ord$words))
  ref = unique(unlist(ord$objs[!is.na(ord$objs)]))
  voc_sz = length(voc) # vocabulary size
  ref_sz = length(ref) # number of objects
  traj = list()
  perf = matrix(0, reps, voc_sz) # a row for each block
  align = matrix(0, voc_sz, ref_sz) # calc from probs of stim on trial
  assoc = matrix(0, voc_sz, ref_sz) # sum of alignments over time
  probs = matrix(0, voc_sz, ref_sz) # normalized assocs
  colnames(align) = ref
  rownames(align) = voc
  colnames(assoc) = ref
  rownames(assoc) = voc
  colnames(probs) = ref
  rownames(probs) = voc
  
  t_unseen = rep(1/beta, voc_sz)
  names(t_unseen) = voc
  
  #assm <- matrix(0, voc_sz+1, ref_sz) # track assoc scores SEPARATELY
  # training
  for(rep in 1:reps) { # for trajectory experiments, train multiple times
    for(t in 1:length(ord$words)) {
      
      tr_w = unlist(ord$words[t])
      tr_w = tr_w[!is.na(tr_w)]
      tr_w = tr_w[tr_w != ""]
      tr_o = unlist(ord$objs[t])
      tr_o = tr_o[!is.na(tr_o)]
      tr_o = tr_o[tr_o != ""]
      
      align = matrix(0, voc_sz, ref_sz) # calc from probs of stim on trial
      tr_probs = probs
      colnames(align) = ref
      rownames(align) = voc
      colnames(tr_probs) = ref
      rownames(tr_probs) = voc
        for(f in tr_o) {
          for(w in tr_w) {
            if(tr_probs[w,f]==0) {
              tr_probs[w,f] = t_unseen[w]
            } 
          }
          sumT = sum(tr_probs[tr_w,f]) + alpha*epsilon
          for(w in tr_w) {
            align[w,f] = (tr_probs[w,f]+epsilon) / sumT 
          }
        }
        assoc[tr_w,tr_o] = assoc[tr_w,tr_o] + align[tr_w,tr_o]
        #probs[tr,] = assoc[tr,] / (rowSums(assoc[tr,])+beta*lambda) 
        for(w in tr_w) {
          sumA = sum(assoc[w,])
          denom = sumA + beta*lambda
          probs[w,] = (assoc[w,] + lambda) / denom
          t_unseen[w] = lambda / denom
        }
      #index = (rep-1)*nrow(ord$trials$words) + t # index for learning trajectory
      index = t
      traj[[index]] = probs
    }
    perf[rep,] = get_perf(probs)
  } 
  want = list(perf=perf, matrix=probs, traj=traj)
  return(want)
} 
