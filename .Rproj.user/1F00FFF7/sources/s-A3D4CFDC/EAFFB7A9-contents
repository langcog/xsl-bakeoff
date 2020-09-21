# now based on Trueswell et al 2013 model:
# 1. guess at chance, 2. next time a word occurs, remember previous guess w prob alpha
# 3. if the remembered guess is present, increase alpha; otherwise choose a new random guess
# was:
# hypothesis-testing model based on Medina, Snedeker, 
# Trueswell, & Gleitman, 2011's verbal description:
# one-trial / "fast mapping" hypothesis:
#  i) learners hypothesize a single meaning based on their first encounter with a word
# ii) learners neither weight nor even store back-up alternative meanings
# iii) on later encounters, learners attempt to retrieve this hypothesis from memory and test it against a new context, updating it only if it is disconfirmed
# Thus, they do not accrue a "best" final hypothesis by comparing multiple episodic memories of prior contexts or multiple semantic hypotheses.

# for testing:
#mat = matrix(c(1,2, 1,3), nrow=2, ncol=2, byrow=T)
# ord = list(words=mat, objs=mat)
#mat2 = matrix(c(1,2,3, 1,4,5, 2,3,4, 5,6,1), nrow=4, ncol=3, byrow=T)
# ord = list(words=mat2, objs=mat2)

# params = c(0.2376261, 0.3546257) # from response XSL paradigm
# params = c(0.2077282, 0.1916493)

model <- function(params, ord=c(), reps=1, verbose=F) {
	if(verbose) print(params)
	alpha = params[1] # prob to remember first guess
	alpha_increase = params[2] # Trueswell 2013 empirically estimates this...
	#sa <- params[2] # prob of storage (slow learning down)

	voc_sz = max(unlist(ord$words), na.rm=TRUE) # vocabulary size
	m <- matrix(0, voc_sz, voc_sz) # hypothesis matrix
  
	traj = list()
	perf = matrix(0, reps, voc_sz) # a row for each block
	freq = rep(0,voc_sz) # number of occurrences per pair, so far (to index the resps matrix)
    
	for(rep in 1:reps) {
		for(t in 1:nrow(ord$words)) {
			tr = as.integer(ord$words[t,]) # ASSUMES words==objects
			freq[tr] = freq[tr] + 1 
			probs = runif(length(tr))
			forget = tr[which(probs > rowSums(m[tr,]))]
			#remember = tr[which(probs <= mem_strength[tr])]
			m[forget,] = m[forget,]*0
			have_hypoths = tr[which(rowSums(m[tr,])!=0)] # throw out inconsistent ones
			for(w in have_hypoths) {
				hypo = which(m[w,]>0)
				if(!is.element(hypo, tr)) { 
					m[w,] = m[w,]*0 # disconfirmed
				} else {
					m[w,hypo] = m[w,hypo] + alpha_increase # strengthen
				}
			}
			need_hypoths = tr[which(rowSums(m[tr,])==0)]
			#store = need_hypoths[which(runif(length(need_hypoths)) < sa)]
			store = need_hypoths
			new_hyps = sample(store, length(store), replace=FALSE)
			for(w in 1:length(store)) {
				m[need_hypoths[w], new_hyps[w]] = alpha
			}
			index = (rep-1)*length(ord$trials) + t # index for learning trajectory
			traj[[index]] = m
		}
		perf[rep,] = diag(m) / (rowSums(m)+1e-12) # just in case of zeros
		#if(verbose) print(m)
	}
	if(verbose) print(perf)
	want = list(perf=perf, matrix=m, traj=traj)
	return(want)
}
