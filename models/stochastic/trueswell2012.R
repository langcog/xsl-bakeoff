# based on Trueswell et al 2013 propose-but-verify model:
# 1. guess at chance, 2. next time a word occurs, remember previous guess w prob alpha
# 3. if the remembered guess is present, increase alpha; otherwise choose a new random guess

# for testing:
#mat = matrix(c(1,2, 1,3), nrow=2, ncol=2, byrow=T)
# ord = list(words=mat, objs=mat)
#mat2 = matrix(c(1,2,3, 1,4,5, 2,3,4, 5,6,1), nrow=4, ncol=3, byrow=T)
# ord = list(words=mat2, objs=mat2)

# params = c(0.2376261, 0.3546257) # from response XSL paradigm
# params = c(0.2077282, 0.1916493)

model <- function(params, ord=c(), reps=1, verbose=F) {
	if(verbose) print(params)
  #print(ord)
	alpha = params[1] # prob to remember first guess
	alpha_increase = params[2] # Trueswell 2013 empirically estimates this...
	#sa <- params[2] # prob of storage (slow learning down)

	voc_sz = max(unlist(ord$words), na.rm=TRUE) # vocabulary size
	ref_sz = max(unlist(ord$objs), na.rm=TRUE)
	m <- matrix(0, voc_sz, ref_sz) # hypothesis matrix
  
	traj = list()
	perf = matrix(0, reps, voc_sz) # a row for each block
	freq = rep(0,voc_sz) # number of occurrences per pair, so far (to index the resps matrix)
  
	for(rep in 1:reps) {
		for(t in 1:nrow(ord$words)) {
			tr_w = as.integer(ord$words[t,]) # ASSUMES words==objects
			tr_o = as.integer(ord$objs[t,])
			freq[tr_w] = freq[tr_w] + 1 
			
			# for each word, 1) check if there is a hypothesized ref
			# if so, is it on this trial? yes -> strengthen
			
			# forget if runif > stored hyp strength (should be 1 non-zero entry per row)
			# identify words that have hypothesized refs
			if(length(tr_w)>1) {
			  forget = tr_w[which(runif(length(tr_w)) > rowSums(m[tr_w,]))]
			  m[forget,] = 0
			  have_hypoths = tr_w[which(rowSums(m[tr_w,])!=0)] 
			} else { # 1 word/trial
			   if(runif(1) > sum(m[tr_w,])) {
			      m[tr_w,] = 0 # forgotten
			   } 
			  have_hypoths = tr_w[which(sum(m[tr_w,])!=0)] 
			}
			
			# throw out inconsistent hyps
			for(w in have_hypoths) {
				hypo = which(m[w,]>0)
				consistent = intersect(hypo, tr_o) # hyp on trial, strengthen
				m[w,consistent] = m[w,consistent] + alpha_increase
				inconsistent = setdiff(hypo, tr_o) # hyp not on trial, disconfirmed
				m[w,inconsistent] = 0
			}
			if(length(tr_w)>1) {
			  need_hypoths = tr_w[which(rowSums(m[tr_w,])==0)] # any words that don't have a hyp
			} else if(sum(m[tr_w,]==0)) { # no hyp exists for this word
			  need_hypoths = tr_w
			}
			store = need_hypoths
			new_hyps = sample(tr_o, length(store), replace=FALSE) # select new random refs from trial
			for(w in 1:length(store)) {
				m[need_hypoths[w], new_hyps[w]] = alpha
			}
			index = (rep-1)*nrow(ord$words) + t # index for learning trajectory
			traj[[index]] = m
		}
		perf[rep,] = diag(m) / (rowSums(m)+1e-12) # just in case of zeros
		#if(verbose) print(m)
	}
	if(verbose) print(perf)
	want = list(perf=perf, matrix=m, traj=traj)
	return(want)
}
