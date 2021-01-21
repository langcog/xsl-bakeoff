# hypothesis-testing model based on Medina, Snedeker, 
# Trueswell, & Gleitman, 2011's verbal description:
# one-trial / "fast mapping" hypothesis:
#  i) learners hypothesize a single meaning based on their first encounter with a word
# ii) learners neither weight nor even store back-up alternative meanings
# iii) on later encounters, learners attempt to retrieve this hypothesis from memory and test it against a new context, updating it only if it is disconfirmed
# Thus, they do not accrue a "best" final hypothesis by comparing multiple episodic memories of prior contexts or multiple semantic hypotheses.

# for ICDL 2012 was: hypoth_model.R
# similar to Blythe, Smith, and Smith's guess-and-test model


model <- function(params, ord=c(), reps=1, verbose=F) {
	f <- params[1] # forget at retrieval
	sa <- params[2] # prob of storage (slow learning down)
	
	voc_sz = max(unlist(ord$words), na.rm=TRUE) # vocabulary size
	ref_sz = max(unlist(ord$objs), na.rm=TRUE)
	ppt = length(ord$words[1,]) # pairs per trial ASSUMES num words = num objs per trial
	m <- matrix(0, voc_sz, ref_sz) # hypothesis matrix
	
	traj = list()
	perf = matrix(0, nrow=reps, ncol=voc_sz) # a row for each block
	freq = rep(0,voc_sz) # number of occurrences per pair, so far 
	
	for(rep in 1:reps) {
		for(t in 1:nrow(ord$words)) {
			tr_w = as.integer(ord$words[t,]) # ASSUMES words==objects
			tr_o = as.integer(ord$objs[t,])
			freq[tr_w] = freq[tr_w] + 1 
			# forget randomly-selected hypotheses
			forget = tr_w[which(runif(ppt) < f)] 
			m[forget,] = m[forget,]*0 
			if(length(tr_w)==1) {
			  have_hypoths = tr_w[which(sum(m[tr_w,])!=0)]
			} else {
			  have_hypoths = tr_w[which(rowSums(m[tr_w,])!=0)] # throw out inconsistent ones
			}
			for(w in have_hypoths) {
				if(!is.element(which(m[w,]==1), tr_o)) { m[w,] = m[w,]*0 } # disconfirmed	
			}
			
			# make new hypotheses
			if(length(tr_w)==1) {
			  need_hypoths = tr_w[which(sum(m[tr_w,])==0)]
			} else {
			  need_hypoths = tr_w[which(rowSums(m[tr_w,])==0)]
			}
			store = need_hypoths[which(runif(length(need_hypoths)) < sa)]
			new_hyps = sample(tr_o, length(store), replace=FALSE) 
			for(w in 1:length(store)) {
				m[need_hypoths[w], new_hyps[w]] = 1 # was m[need_hypoths[w], store[w]]
			}
			index = (rep-1)*nrow(ord$words) + t # index for learning trajectory
			traj[[index]] = m
		}
		perf[rep,] = diag(m)
	}
	if(verbose) print(perf)
	want = list(perf=perf, matrix=m+1e-12, traj=traj)
	return(want)
}
