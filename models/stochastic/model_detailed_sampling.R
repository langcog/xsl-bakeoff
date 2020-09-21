# Associative Uncertainty- (Entropy) & Familiarity-Biased Model
# George Kachergis June 10, 2011

shannon.entropy <- function(p) {
	if (min(p) < 0 || sum(p) <= 0)
		return(NA)
	p.norm <- p[p>0]/sum(p)
	-sum(log2(p.norm)*p.norm)
	}

update_known <- function(m, tr) {
	startval = .01
		
	for(i in tr) {
		for(c in 1:dim(m)[2]) {
			if(sum(m[,c]>0) & m[i,c]==0) {
				m[i,c] = startval
				m[c,i] = startval
			}
		}
		for(j in tr) {
			if(m[i,j]==0) m[i,j] = startval
			if(m[j,i]==0) m[j,i] = startval
			}
		}
		return(m)
	}


model <- function(params, ord=c(), K=1, name="model", save_traj=FALSE, print_matrix=FALSE) {
  if(K=="max") K = dim(ord)[2] 
  reps = 1
  #K = of assocs to update per word
  X <- params[1] # associative weight to distribute
	B <- params[2] # weighting of uncertainty vs. familiarity
	C <- params[3] # decay

	voc_sz = max(ord, na.rm=TRUE) # vocabulary size
	ppt = dim(ord)[2] # pairs per trial
	mean_ent = c()
	m <- matrix(0, voc_sz, voc_sz) # association matrix
	compScore = rep(0, nrow(ord))
	trial_sz = dim(ord)[2]
  
	# want an item x occurrence matrix, to be filled in during training 
	freq = rep(0,voc_sz) # number of occurrences per pair, so far (to index the resps matrix)
  
	perf = c() # mean perf at end of each training ord
	# training
	for(rep in 1:reps) { # for trajectory experiments, train multiple times
	  for(t in 1:dim(ord)[1]) { 
		#print(format(m, digits=3))
		tr = as.integer(ord[t,])
		tr = tr[!is.na(tr)]
		freq[tr] = freq[tr] + 1
		m = update_known(m, tr) # what's been seen so far?
		ent = rep(0, voc_sz)
		for(w in tr) { ent[w] = shannon.entropy(m[w,]) }
		ent = exp(B*ent) # this is really big at first..maybe normalize entropy? I think I tried that..
		
		temp_wts = matrix(0, voc_sz, voc_sz)
		temp_wts[tr,tr] = m[tr,tr] # use these weights to calculate entropy
		temp_wts = temp_wts * (ent %*% t(ent)) 
		#denom = sum(temp_wts * (ent %*% t(ent)))
		#temp_wts = temp_wts / denom
		# now sample K objs per word
		# could instead sample K assocs from all_wts, period
		# or do strong ME (queens problem...)
		chosen_assocs = matrix(0, voc_sz, voc_sz)
		#print(tr)
		for(w in tr) { 
		  x <- sample(1:voc_sz, K, replace=FALSE, prob=temp_wts[w,]) 
		  chosen_assocs[w,x] = m[w,x] # PK for chosen
		}
		denom = sum(chosen_assocs * (ent %*% t(ent)))
		chosen_assocs = (X * chosen_assocs * (ent %*% t(ent))) / denom
		m = m*C # decay everything
		m = m + chosen_assocs
		
		if(print_matrix) print(m)
		compScore[t] = sum( diag(m) / rowSums(m+1e-9) )
	  }
	  perf = c(perf, mean(diag(m) / rowSums(m+1e-9)))
	}
  resp_prob = diag(m) / rowSums(m)
  want = list(perf=resp_prob, matrix=m, compScore=compScore) # , traj=traj
	return(want)
	}

