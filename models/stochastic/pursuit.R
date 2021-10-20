# Stevens et al. 2014 pursuit model
# The pursuit model assumes that when word w is heard, it’s strongest associated meaning (h) is selected, 
# and the association A(w,h) is updated based on the presence or absence of that referent. 
# A(w,h) is increased if h is present in the current context, and no other association involving w is strengthened. 
# If h is not present, A(w,h) is decreased and a single new referent is chosen from the context. 
# Whenever the P(h’|w) for some h’ exceeds a threshold value, that meaning is added to the lexicon (never to be removed).

# for each word w in utterance
# 1) if w is novel, choose a referent in context with minimum association and give it gamma
# 2) else, add new objects to w's hypothesis space
# 3) if w is in the lexicon: if w's hypothesized meaning is in the scene, reward that meaning
#	 otherwise, penalize that meaning
# 4) if w is not in the current lexicon:
#      for each obj o in the scene, if o is not in the current lexicon, reward that meaning
# 5) update the current lexicon (threshold)

# following Bush and Mosteller (1951) Linear Reward-Penalty (LR-P) scheme:
# reward: p(h) = p(h) + gamma*(1-p(h)) and reduce all h'!=h: p(h') = p(h')*(1-gamma)
# penalty: p(h) = p(h)*(1-gamma)

#params = c(.2, .6, .05)

model <- function(params, ord=c(), start_matrix=c(), verbose=F) {
  gamma = params[1] # learning rate
  thresh = params[2] # threshold (prob) to move an association to the known lexicon 
  lambda = params[3] # smoothing prob
  
  voc = unique(unlist(ord$words))
  ref = unique(unlist(ord$objs[!is.na(ord$objs)]))
  voc_sz = length(voc) # vocabulary size
  ref_sz = length(ref) # number of objects
  
  if(is.matrix(start_matrix)) {
    m <- start_matrix
  } else {
    m <- matrix(0, voc_sz, ref_sz) # association matrix A(w,h)
  }
  colnames(m) = ref
  rownames(m) = voc
  lexicon <- matrix(0, voc_sz, ref_sz) # for any association that reaches threshold
  colnames(lexicon) = ref
  rownames(lexicon) = voc
  compScore = rep(0, length(ord$words))
  freq = rep(0,voc_sz) # number of occurrences per pair, so far (to index the resps matrix)
  names(freq) = voc
  traj = list()
  
  for(t in 1:length(ord$words)) {
    tr_w = unlist(ord$words[t])
    tr_w = tr_w[!is.na(tr_w)]
    tr_w = tr_w[tr_w != ""]
    tr_o = unlist(ord$objs[t])
    tr_o = tr_o[!is.na(tr_o)]
    if(length(tr_o) == 0) {
      index = t
      traj[[index]] = m
      next
    }
      novel = tr_w[which(freq[tr_w]==0)] # novel words
      freq[tr_w] = freq[tr_w] + 1 # tracks word freq
      if(length(tr_w)==1) {
        have_hypoths = tr_w[which(sum(m[tr_w,1:ref_sz])!=0)]
      } else {
        have_hypoths = tr_w[which(rowSums(m[tr_w,1:ref_sz])!=0)] # words which have non-zero assocs
      }
      #need_hypoths = tr_w[which(rowSums(m[tr_w,1:ref_sz])==0)] # words with zero 
      
      # initialize...
      # 1) if w is novel, select one from available refs with minimum assoc to any other word
      for(w in 1:length(novel)) {
        if (length(novel) == 0) {next}
        # find max assoc for each referent (including to words not on trial?)
        max_assoc = apply(as.matrix(m[,tr_o]), 2, max)
        # select one referent with minimum maximum association
        min_ref = tr_o[which(max_assoc==min(max_assoc))]
        if(length(min_ref)>1) min_ref = sample(min_ref, 1) # break ties 
        m[novel[w], min_ref] = gamma 
      }
      
      for(w in have_hypoths) {
        hypo = which(m[w,]==max(m[w,1:ref_sz])) # strongest one
        if(length(hypo)>1) hypo = sample(hypo, 1) # (choose one if >1 strongest)
        if(!is.element(hypo, tr_o)) { # not on trial, so weaken:
          m[w,hypo] = m[w,hypo]*(1-gamma) # disconfirmed
          # not on trial, so random new hypothesized referent
          new_hyp = sample(tr_o, 1, replace = TRUE)
          m[w,new_hyp] = m[w,new_hyp] + gamma * (1 - m[w,new_hyp])
        } else {
          m[w,hypo] = m[w,hypo] + gamma * (1 - m[w,hypo]) # confirmed: strengthen
        }
      }
      
      Pm_w = m+lambda
      Pm_w = Pm_w / rowSums(Pm_w) # Eq 1
      lexicon[which(Pm_w>thresh)] = 1 # add to lexicon
      traj[[t]] = lexicon
      #compScore[t] = sum(diag(lexicon) / rowSums(lexicon+1e-9)) # should use index
      compScore[t] = sum(get_perf(Pm_w))
    }
  resp_prob = get_perf(lexicon+1e-12) 
  
  if(verbose) print(m)
  #want = list(perf=diag(Pm_w), matrix=Pm_w, compScore=compScore) 
  want = list(perf=as.numeric(resp_prob), matrix=lexicon+1e-12, compScore=as.numeric(compScore), traj=traj)
  return(want)
}

