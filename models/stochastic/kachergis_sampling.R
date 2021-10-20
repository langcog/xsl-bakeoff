# Associative Uncertainty- (Entropy) & Familiarity-Biased Model
# George Kachergis June 10, 2011

shannon.entropy <- function(p) {
	if (min(p) < 0 || sum(p) <= 0)
		return(NA)
	p.norm <- p[p>0]/sum(p)
	-sum(log2(p.norm)*p.norm)
	}

update_known <- function(m, tr_w, tr_o, startval = .01) {
  tr_assocs = m[tr_w, tr_o]
  tr_assocs[which(tr_assocs==0)] = startval
  m[tr_w, tr_o] = tr_assocs
  
  
  
  # for any other experienced word (not on this trial), fill in startval
  
  fam_objs = which(colSums(m)>0)
  fam_words = which(rowSums(m)>0)
  
  for(w in tr_w) {
    
    zeros = which(m[w,fam_objs]==0)
    m[w,zeros] = startval
  }
  
  for(o in tr_o) {
    zeros = which(m[fam_words,o]==0)
    m[zeros,o] = startval
  }
  
  return(m)
}


model <- function(params, ord=c(), reps=1, K=1) {
  #K = of assocs to update per word
  X <- params[1] # associative weight to distribute
	B <- params[2] # weighting of uncertainty vs. familiarity
	C <- params[3] # decay

	voc = unique(unlist(ord$words))
	ref = unique(unlist(ord$objs[!is.na(ord$objs)]))
	voc_sz = length(voc) # vocabulary size
	ref_sz = length(ref) # number of objects
	traj = list()
	m <- matrix(0, voc_sz, ref_sz) # association matrix
	colnames(m) = ref
	rownames(m) = voc
	perf = matrix(0, reps, voc_sz) # a row for each block
  
	mean_ent = c()

	# want an item x occurrence matrix, to be filled in during training 
	freq = rep(0,voc_sz) # number of occurrences per word, so far (to index the resps matrix)
	names(freq) = voc
	# training
	for(rep in 1:reps) { # for trajectory experiments, train multiple times
	  for(t in 1:length(ord$words)) { 
	    
		#print(format(m, digits=3))
	    tr_w = unlist(ord$words[t])
	    tr_w = tr_w[!is.na(tr_w)]
	    tr_w = tr_w[tr_w != ""]
	    tr_o = unlist(ord$objs[t])
	    tr_o = tr_o[!is.na(tr_o)]
		
	  freq[tr_w] = freq[tr_w] + 1
		m = update_known(m, tr_w, tr_o) # what's been seen so far?
		
		ent_w = rep(0, voc_sz)
		names(ent_w) = voc
		
		for(w in tr_w) { 
		  ent_w[w] = shannon.entropy(m[w,]) 
		}
		ent_w = exp(B*ent_w) 
		
		ent_o = rep(0, ref_sz)
		names(ent_o) = ref
		
		for(o in tr_o) { 
		  ent_o[o] = shannon.entropy(m[,o]) 
		}
		ent_o = exp(B*ent_o)
		
		temp_wts = matrix(0, voc_sz, ref_sz)
		colnames(temp_wts) = ref
		rownames(temp_wts) = voc
		temp_wts[tr_w,tr_o] = m[tr_w,tr_o] # use these weights to calculate entropy
		nent = (ent_w %*% t(ent_o)) 
		temp_wts = temp_wts * as.matrix(nent)
		
		
		chosen_assocs = matrix(0, voc_sz, ref_sz)
		colnames(chosen_assocs) = ref
		rownames(chosen_assocs) = voc
		for(w in tr_w) { 
		  # if (sum(temp_wts[w,]) == 0) {next}
		  x <- sample(1:ref_sz, K, replace=TRUE, prob=temp_wts[w,]) 
		  chosen_assocs[w,x] = m[w,x] # PK for chosen
		}
		denom = sum(chosen_assocs * nent)
		chosen_assocs = (X * chosen_assocs * nent) / denom
		m = m*C # decay everything
		m = m + chosen_assocs
		
		index = (rep-1)*length(ord$words) + t # index for learning trajectory
		traj[[index]] = m
	  }
	  perf[rep,] = get_perf(m + 1e-9)
	}
  resp_prob = get_perf(m)
  want = list(perf=resp_prob, matrix=m, traj=traj) 
	return(want)
	}

