# based on Trueswell et al 2013 model -- but now responds "Don't Know" when it has no stored association for a word:
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

# for hypothesis/response-training condition, want to track correct responses at each occurrence of a word

model <- function(params, ord=c(), ord_name="", reps=1, K=NA, verbose=F) {

	alpha = params[1] # prob to remember first guess
	alpha_increase = params[2] # Trueswell 2013 empirically estimates this...
	#sa <- params[2] # prob of storage (slow learning down)

	voc_sz = max(ord, na.rm=TRUE) # vocabulary size
	ppt = dim(ord)[2] # pairs per trial
	compScore = rep(0, reps*nrow(ord))
	m <- matrix(0, voc_sz, voc_sz) # hypothesis matrix
  
	# want an item x occurrence matrix, to be filled in during training 
	freq = rep(0,voc_sz) # number of occurrences per pair, so far (to index the resps matrix)
  
	#mem_strength = rep(0,voc_sz) # how strong a w's hypothesis is (strengthens if confirmed)
	perf = c()
	for(rep in 1:reps) {
		for(t in 1:dim(ord)[1]) {
			tr = as.integer(ord[t,])
			freq[tr] = freq[tr] + 1 
			probs = runif(ppt)
			forget = tr[which(probs > rowSums(m[tr,]))] # forget objects for these words--and these are the ones we say "Don't Know" for
			#remember = tr[which(probs <= mem_strength[tr])]
			m[forget,] = 0 # m[forget,]*discount_par # could have a forgetting parameter..
			have_hypoths = tr[which(rowSums(m[tr,])!=0)] # throw out inconsistent ones
			for(w in have_hypoths) {
				hypo = which(m[w,]>0)
				if(!is.element(hypo, tr)) { # if a word has a hypothesis obj not on the current trial, throw it out
					m[w,] = 0 # m[w,]*discount_par # disconfirmed
				} else { # if it is on the trial, strengthen it
					m[w,hypo] = m[w,hypo] + alpha_increase # strengthen
				}
			}
			need_hypoths = tr[which(rowSums(m[tr,tr])==0)]
			store = need_hypoths
			if(length(store)>1) {
			  new_hyps = sample(store, length(store), replace=FALSE) # sample on one integer X returns an integer [1,X]...not what we want!
			} else {
			  new_hyps = store
			}
			if(length(need_hypoths)!=length(new_hyps)) { # shouldn't happen...right?
			  print(need_hypoths)
			  print(new_hypoths)
			}
			for(w in 1:length(store)) {
				m[need_hypoths[w], new_hyps[w]] = alpha
			}
		compScore[t] = sum(diag(m)>0)	
		}
		perf = c(perf, sum(diag(m)>0)/voc_sz)
		if(verbose) print(m)
	}

	resp_prob = diag(m) / (rowSums(m)+1e-9)
	want = list(perf=resp_prob, matrix=m, compScore=compScore) # , traj=traj
	return(want)
}
