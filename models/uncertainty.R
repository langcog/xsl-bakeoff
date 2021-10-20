# Associative Uncertainty-/Entropy-Biased Model
# (just the uncertainty bias of the Kachergis et al. 2012 model)
# George Kachergis  george.kachergis@gmail.com  

shannon.entropy <- function(p) {
	if (min(p) < 0 || sum(p) <= 0)
		return(NA)
	p.norm <- p[p>0]/sum(p)
	-sum(log2(p.norm)*p.norm)
	}

update_known <- function(m, tr_w, tr_o, startval = .01) {
  tr_assocs = m[tr_w, tr_o]
  tr_assocs[which(tr_assocs==0)] = startval
  m[tr_w, tr_o] = tr_assocs # or do this below?
  
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

model <- function(params, ord=c(), start_matrix=c(), reps=1, test_noise=0) {
	X <- params[1] # associative weight to distribute
	B <- params[2] # weighting of uncertainty vs. familiarity
	C <- params[3] # decay
	
	voc = unique(unlist(ord$words))
	ref = unique(unlist(ord$objs[!is.na(ord$objs)]))
	voc_sz = length(voc) # vocabulary size
	ref_sz = length(ref) # number of objects
	traj = list()
	if(is.matrix(start_matrix)) {
	  m <- start_matrix
	} else {
	  m <- matrix(0, voc_sz, ref_sz) # association matrix
	}
	colnames(m) = ref
	rownames(m) = voc
	perf = matrix(0, reps, voc_sz) # a row for each block
	# training
	for(rep in 1:reps) { # for trajectory experiments, train multiple times
	  for(t in 1:length(ord$words)) { 
		#print(format(m, digits=3))
		
	    tr_w = unlist(ord$words[t])
	    tr_w = tr_w[!is.na(tr_w)]
	    tr_w = tr_w[tr_w != ""]
	    tr_o = unlist(ord$objs[t])
	    tr_o = tr_o[!is.na(tr_o)]
		m = update_known(m, tr_w, tr_o) # what's been seen so far?
		ent_w = c() # more entropy = more dispersive
		for(w in tr_w) { ent_w = c(ent_w, shannon.entropy(m[w,])) }
		ent_w = exp(B*ent_w)
		
		ent_o = c() # more entropy = more dispersive
		for(o in tr_o) { ent_o = c(ent_o, shannon.entropy(m[,o])) }
		ent_o = exp(B*ent_o)
		
		nent = (ent_w %*% t(ent_o))
		denom = sum(nent)
		m = m*C # decay everything
		# update associations on this trial
		m[tr_w,tr_o] = m[tr_w,tr_o] + (X * (ent_w %*% t(ent_o))) / denom 

		index = (rep-1)*length(ord$words) + t # index for learning trajectory
		traj[[index]] = m
	  }
	m_test = m+test_noise # test noise constant k
	perf[rep,] = get_perf(m_test)
	}
	want = list(perf=perf, matrix=m, traj=traj)
	return(want)
	}

