# Guess-and-Test hypothesis-testing model based on Medina, Snedeker, 
# Trueswell, & Gleitman, 2011's verbal description:
# one-trial / "fast mapping" hypothesis:
#   i) learners hypothesize a single meaning based on their first encounter with a word
#  ii) learners neither weight nor even store back-up alternative meanings
# iii) on later encounters, learners attempt to retrieve this hypothesis from memory 
#      and test it against a new context, updating it only if it is disconfirmed
# Thus, they do not accrue a "best" final hypothesis by comparing multiple 
# episodic memories of prior contexts or multiple semantic hypotheses.


model <- function(params, ord=c(), ord_name="", reps=1, K=NA, verbose=F) {
	f <- params[1] # forget at retrieval
	sa <- params[2] # prob of storage (slow learning down)
	
	voc_sz = max(ord, na.rm=TRUE) # vocabulary size
	# want an item x occurrence matrix, to be filled in during training 
	freq = rep(0,voc_sz) # number of occurrences per pair, so far (to index the resps matrix)
	
	voc_sz = max(ord, na.rm=TRUE) # vocabulary size
	ppt = dim(ord)[2] # pairs per trial
	compScore = rep(0, reps*nrow(ord))
	m <- matrix(0, voc_sz, voc_sz) # hypothesis matrix
	perf = c()
	for(rep in 1:reps) {
		for(t in 1:dim(ord)[1]) {
			tr = as.integer(ord[t,])
			freq[tr] = freq[tr] + 1 
			forget = tr[which(runif(ppt) < f)]
			m[forget,] = m[forget,]*0
			have_hypoths = tr[which(rowSums(m[tr,])!=0)] # throw out inconsistent ones
			for(w in have_hypoths) {
				if(!is.element(which(m[w,]==1), tr)) { m[w,] = m[w,]*0 } # disconfirmed	
			}
			need_hypoths = tr[which(rowSums(m[tr,])==0)]
			store = need_hypoths[which(runif(length(need_hypoths)) < sa)]
			if(verbose) {
			  print("forgotten hyps for:")
			  print(forget)
			  print("storing hyps for:")
			  print(store)
			}
			if(length(store)>1) {
			  new_hyps = sample(store, length(store), replace=FALSE)
			} else {
			  new_hyps = store
			}
			for(w in 1:length(store)) {
				m[need_hypoths[w], new_hyps[w]] = 1 # was m[need_hypoths[w], store[w]]
			}
			
			for(s in tr) { # sometimes a word still has no new hypothesized meaning at the end of a trial..
			  if(sum(m[s,tr])==0) { # if it was forgotten (or didn't have a hypothesis at all)
			    # choose randomly from the ones that needed hypoths
			     if(length(need_hypoths)>1) {
			       response = sample(need_hypoths,1)
			     } else {
			       response = need_hypoths
			     }
			  } else { # sample from whatever we have stored
			    response = sample(tr, 1, prob=m[s,tr])
			  }
			}
			compScore[t] = sum(diag(m)) # normalize?
		}
		perf = c(perf, sum(diag(m))/voc_sz)
		#if(verbose) print(m)
	}

	resp_prob = diag(m) / (rowSums(m)+1e-9)
	want = list(perf=resp_prob, matrix=m, compScore=compScore) # , traj=traj
	return(want)
}
