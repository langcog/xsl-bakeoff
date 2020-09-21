# Stevens et al. 2014 pursuit model
# The pursuit model assumes that when word w is heard, it’s strongest associated meaning (h) is selected, and the association A(w,h) is updated based on the presence or absence of that referent. A(w,h) is increased if h is present in the current context, and no other association involving w is strengthened. If h is not present, A(w,h) is decreased and a single new referent is chosen from the context. Whenever the P(h’|w) for some h’ exceeds a threshold value, that meaning is added to the lexicon (never to be removed).

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

### where did i get the following??
#	for all h'!=h, p(h') = gamma/(n-1) + p(h')*(1-gamma) where n=# of hypotheses being considered

# for testing:
mat = matrix(c(1,2, 1,3), nrow=2, ncol=2, byrow=T)
# ord = list(words=mat, objs=mat)
mat2 = matrix(c(1,2,3, 1,4,5, 2,3,4, 5,6,1), nrow=4, ncol=3, byrow=T)
# ord = list(words=mat2, objs=mat2)

# params = c(.001, .208, .001) # from response XSL paradigm

model <- function(params, ord=c(), reps=1, verbose=F) {
	gamma = params[1] # learning rate
	thresh = params[2] # threshold (prob) to move an association to the known lexicon 
	lambda = params[3] # smoothing prob
	
	voc_sz = max(unlist(ord$words), na.rm=TRUE) # vocabulary size
	ref_sz = max(unlist(ord$objs), na.rm=TRUE)
	
	m <- matrix(0, voc_sz, ref_sz) # association matrix A(w,h)
  lexicon <- matrix(0, voc_sz, ref_sz) # for any association that reaches threshold
  
	traj = list()
	perf = matrix(0, nrow=reps, ncol=voc_sz) # a row for each block
	freq = rep(0,voc_sz) # number of occurrences per pair, so far 
	
	#mem_strength = rep(0,voc_sz) # how strong a w's hypothesis is (strengthens if confirmed)
	for(rep in 1:reps) {
		for(t in 1:nrow(ord$words)) {
			tr_w = as.integer(ord$words[t,]) # ASSUMES words==objects
			tr_o = as.integer(ord$objs[t,]) 
			novel = tr_w[which(freq[tr_w]==0)]
			freq[tr_w] = freq[tr_w] + 1 
			have_hypoths = tr_w[which(rowSums(m[tr_w,1:voc_sz])!=0)]
			need_hypoths = tr_w[which(rowSums(m[tr_w,1:voc_sz])==0)] # (selects novel words/objs)
			
			# initialize...
			new_hyps = sample(novel, length(novel), replace=FALSE)
			# 1) if w is novel, select min available assoc...(occurs by default)
			#m[diag(outer(new_hyps, novel))] = gamma
			for(w in 1:length(novel)) {
				m[novel[w], new_hyps[w]] = gamma
			}
			
			for(w in have_hypoths) {
				hypo = which(m[w,]==max(m[w,1:ref_sz])) # strongest one
				if(length(hypo)>1) hypo = sample(hypo, 1) # (choose one if >1 strongest)
				if(!is.element(hypo, tr)) { # not on trial, so weaken:
					m[w,hypo] = m[w,hypo]*(1-gamma) # disconfirmed
					need_hypoths = c(need_hypoths, w) # not on trial, so find another
				} else {
					m[w,hypo] = m[w,hypo] + gamma*(1-m[w,hypo]) # strengthen
					#m[w,which(1:voc_sz!=hypo)] = m[w,hypo]*(1-gamma) # weaken other hypotheses
				}
			}
			store = need_hypoths
			new_hyps = sample(store, length(store), replace=FALSE)
			for(w in 1:length(store)) {
				m[need_hypoths[w], new_hyps[w]] = gamma*(1-m[need_hypoths[w],new_hyps[w]])
			}
			
			Pm_w = m+lambda
			Pm_w = Pm_w / rowSums(Pm_w) # Eq 1
			lexicon[which(Pm_w>thresh)] = 1 # add to lexicon
			
			index = (rep-1)*length(ord$trials) + t # index for learning trajectory
			traj[[index]] = lexicon
				
		}
		perf[rep,] = diag(lexicon)
		if(verbose) print(m)
	}
	want = list(perf=perf, matrix=m, traj=traj)
	return(want)
}
