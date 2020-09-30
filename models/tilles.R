# Tilles and Fontanari's (2013) cross-situational word learning model
# George Kachergis  george.kachergis@gmail.com  September 16, 2013

shannon.entropy <- function(p) { # some NA values getting in here...
	if (min(p) < 0 || sum(p) <= 0) 
	  return(NA)
	p.norm <- p[p>0]/sum(p)
	-sum(log2(p.norm)*p.norm)
}



model <- function(params, ord=c()) {
	X <- params[1] # reinforcement parameter for stimuli in current context
	B <- params[2] # inference parameter, regulating ME and prior info integration 
	# (applies either to known words that do not appear in the current context or to new words in the current context)
	alpha_0 <- params[3] # baseline efficiency corresponding to maximal uncertainty about referent of target word
	# all parms in [0,1]
	# B=1 enforces ME: new word is associated with equal prob to any available new objects
	# B=0 forces new word to be associated to all objects already seen (including current)
	
	voc_sz = max(unlist(ord$words), na.rm=TRUE) # vocabulary size
	ref_sz = max(unlist(ord$objs), na.rm=TRUE) # number of objects
	
	novel_w = rep(TRUE, voc_sz) # set novel[i]=0 once i has appeared
	novel_o = rep(TRUE, ref_sz)
	m <- matrix(0, voc_sz, ref_sz) # association matrix - probabilities
	compScore = rep(0, nrow(ord$words))
	# training
	tr_w = as.integer(ord$words[1,])
	tr_o = as.integer(ord$objs[1,])
	m[tr_w,tr_o] = 1/length(tr_w) # eq 1: 1/C, where C=|context| (current words and objs)
	novel_w[tr_w] = FALSE
	novel_o[tr_o] = FALSE
	
	for(t in 2:nrow(ord$words)) { 
	  tr_w = as.integer(ord$words[t,])
	  tr_w = tr_w[!is.na(tr_w)]
	  tr_o = as.integer(ord$objs[t,])
	  tr_o = tr_o[!is.na(tr_o)]
		
		Nprev_w = length(which(novel_w==FALSE)) # N_{t-1} for alpha_{t-1}
		Nprev_o = length(which(novel_o==FALSE)) 
		
		# update rule for context omega_t depends on membership in
		# 1) words and referents appearing for first time on trial t, or
		# 2) words and referents not appearing on t, but that have appeared before
		
		# objects
		cur_novel_o = tr_o[which(novel_o[tr_o])] # ~ = current novel objects
		cur_old_o = tr_o[which(!novel_o[tr_o])] # current trial's old objects
		other_old_o = setdiff(which(!novel_o), cur_old_o) # stimuli appearing before, but not currently
		novel_o[tr_o] = FALSE
		# words
		cur_novel_w = tr_w[which(novel_w[tr_w])] # ~ = current novel stimuli
		cur_old_w = tr_w[which(!novel_w[tr_w])] # current trial's old words
		other_old_w = setdiff(which(!novel_w), cur_old_w) # stimuli appearing before, but not currently
		novel_w[tr_w] = FALSE
		
		N = length(which(novel_o==FALSE)) # number of distinct objects appearing up to and including N_t
		
		# preprocessing of novel stimuli
		if(length(cur_novel_w)!=0) { # what if cur_novel_o / cur_old_o / other_old_o == NULL?
			m[cur_novel_w,cur_novel_o] = B/length(cur_novel) + (1-B)/N # Eqn 2
			m[cur_novel_w,cur_old_o] = (1-B) / N # Eqn 3
			m[cur_novel_w,other_old_o] = (1-B) / N # Eqn 4 (obj not appearing on current t, but earlier)
		}
		
		# alpha_{t-1} -- must be previous trial!
		alpha = rep(NA, voc_sz) # length(tr)
		for(w in tr_w) { # 1:voc_sz
			alpha[w] = alpha_0 + (1-alpha_0) * (1-shannon.entropy(m[w,])/log(N)) # eq 7 - Nprev?
		} 
		
		# now for all stimuli on the current trial...
		# net gain of confidence for association w_i and o_j:
		
		flux = rep(NA, voc_sz)
		r <- matrix(0, voc_sz, voc_sz)
		
		# slow iterative method; rowSums(m) = 1
		for(w in tr_w) {
			flux[w] = sum(m[w,other_old_o]) / sum(m[w,tr_o]) # eq 5.1
			r[w,tr_o] = X*m[w,tr_o]*flux[w] # eq 5.2
			m[w,tr_o] = m[w,tr_o] + alpha[w]*r[w,tr_o] + (1-alpha[w])*(1/N - m[w,tr_o]) # eq 8
			m[w,other_old_o] = m[w,other_old_o] - alpha[w]*X*m[w,other_old_o] + (1-alpha[w])*(1/N - m[w,other_old_o]) # eq 9
		}
    

		# X=1 transfers confidences for absent objects to the currently ones
		# model also assumes that the more certain an association is, the more
		# efficiently that information should be used for reinforcement
		mt = m
		# now update the other_old words
    # if params[3] < .268 on "block2_369-3x3hiCD" m[18,18] becomes negative at trial 9
		flux = rep(0,voc_sz)
		r <- matrix(0, voc_sz, ref_sz)
		flux[other_old_w] = rowSums(m[other_old_w,tr_o]) / rowSums(m[other_old_w,other_old_o]) # eq 11.1
		r[other_old_w,other_old_o] = B*mt[other_old_w,other_old_o]*flux[other_old_w] # eq 11.2
		m[other_old_w,other_old_o] = mt[other_old_w,other_old_o] + alpha[other_old_w]*r[other_old_w,other_old_o] + (1-alpha[other_old_w])*(1/N - mt[other_old_w,other_old_o]) # eq 12
		m[other_old_w,tr_o] = mt[other_old_w,tr_o] - alpha[other_old_w]*B*mt[other_old_w,tr_o] + (1-alpha[other_old_w])*(1/N - mt[other_old_w,tr_o]) # eq 13
		
		compScore[t] = sum(diag(m))
	}
	perf = diag(m) / rowSums(m)
	#print(perf)
	want = list(perf=perf, matrix=m, compScore=compScore)
	return(want)
	}

#ord = read.table("block2_3_6_9-3x3.txt", header=F)
#print(model(c(.6, .8, .85), ord)) # works

#print(model(c(.6, .8, .23), ord) # doesn't work
#print(model(c(1, 1, .25), ord)) # doesn't work - .29 works, .28 doesn't