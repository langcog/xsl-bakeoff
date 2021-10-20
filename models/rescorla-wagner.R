# Rescorla-Wagner (1972) associative learning model 
# adapted for cross-situational word learning by
# George Kachergis  george.kachergis@gmail.com

#ord = list(trialsc(1,2,3, 1,4,5, 2,3,4, 5,6,1), nrow=4, ncol=3, byrow=T)

#get_test_study_ord = function() {
# 	ord = list()
#	ord$trials[[1]] = list(words=c(1,2,3), objs=c(2,1,3))
#	ord$trials[[2]] = list(words=c(1,4,5), objs=c(1,4,5))
#	ord$trials[[3]] = list(words=c(2,3,4), objs=c(4,3,2))
#	ord$trials[[1]] = list(words=c(5,6,1), objs=c(5,6,1))
#	return(ord)
#}

get_test_study_ord = function(simple=T) {
	if(simple) { 
		design = matrix(c(1,2, 1,3), nrow=2, ncol=2, byrow=T)
	} else {
		design = matrix(c(1,2,3, 1,4,5, 2,3,4, 5,6,1), nrow=4, ncol=3, byrow=T)
	}
 	ord = list(words=design, objs=design)
	return(ord)
}

#ord = get_test_study_ord(simple=T)
#params = c(.1, 1, 1) # 3rd should be > 1st
# model(params, ord=ord, reps=3)

model <- function(params, ord=c(), reps=1, test_noise=0) {
	C = params[2] # decay
	lambda = params[3] # maximum associative value that a CS can achieve - should be larger than learning rate
	beta = params[1]*lambda # learning rate -- a proportion of lambda
	alpha = 1 # salience (fix at 1 unless manipulated)
	
	voc = unique(unlist(ord$words))
	ref = unique(unlist(ord$objs[!is.na(ord$objs)]))
	voc_sz = length(voc) # vocabulary size
	ref_sz = length(ref) # number of objects
	traj = list()
	m <- matrix(0, voc_sz, ref_sz) # association matrix
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
				
		# if objects are cues that predict words, then we want colSums;
		# if words are cues, use rowSums--but only of the currently-presented stimuli
		if(length(tr_w)==1) {
		  pred = m[tr_w,tr_o] # should prediction be based on entire col of obj assocs?
		} else if(length(tr_o)==1) {
		  pred = sum(m[tr_w,tr_o])
		} else {
		  pred = colSums(m[tr_w,tr_o]) 
		}
		delta = alpha*beta*(lambda - pred)
		m[tr_w,tr_o] = m[tr_w,tr_o] + delta 
		
		m = m*C
		
		index = (rep-1)*length(ord$words) + t # index for learning trajectory
		traj[[index]] = m
	  }
	
	m_test = m+test_noise # test noise constant k
	perf[rep,] = get_perf(m_test)
	}
	want = list(perf=perf, matrix=m, traj=traj)
	return(want)
	}

