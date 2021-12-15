require(here)
# example
# load("data/combined_data.RData")
# source("models/fazly.R")

# group_fits$fazly$optim$bestmem 

#gold_fgt = read.csv(here("data/FGT_data/gold.txt"), sep = " ")
#names(gold_fgt) = c("words", "objs")

# get_cooc_matrix - take a training order, build a cooccurrence matrix
create_matrix <- function(train) {
  Nwords = length(unique(unlist(train$words)))
  Nobjs = length(unique(unlist(train$objs)))
  M = matrix(0, nrow=Nwords, ncol=Nobjs)
  rownames(M) = sort(unique(unlist(train$words)))
  colnames(M) = sort(unique(unlist(train$objs)))
  # iterate over training scenes, M[train$words[i], grain$objs[i]] = M[train$words[i], grain$objs[i]] + 1
  
  return(M)
}


get_perf <- function(m) {
  perf <- rep(0, nrow(m))
  names(perf) <- rownames(m)
  for (ref in colnames(m)) {
    if (!(ref %in% rownames(m))) {
      next
    }
    correct <- m[ref, ref]
    total <- sum(m[ref,])
    if (total == 0) {
      next
    }
    perf[ref] <- correct / total
  }
  return(perf)
}

get_fscore <- function(thresh, mat, fscore_only=T, gold_lexicon = c(), verbose=F) {
  tmat <- mat >= thresh
  tp = get_tp(tmat, gold_lexicon) # correct referents selected
  words = gold_lexicon[["word"]]
  words = words[words %in% rownames(tmat)]
  objects = gold_lexicon[["object"]]
  objects = objects[objects %in% colnames(tmat)]
  if (length(gold_lexicon) > 0) {
    fp = sum(tmat[words, objects]) - tp
    fn = length(objects) - tp
  } else {
    fp = sum(tmat) - tp # incorrect referents selected: all selected referents - TPs
    fn = ncol(tmat) - tp # correct referents missed: num of words - TPs
  }
  if(verbose) print(c(tp, fp, fn))
  precision = tp / (tp + fp) 
  recall = tp / (tp + fn) # aka sensitivity / true positive rate
  tn = sum(!tmat) - fn # all the 0s that should be 0s
  specificity = tn / (tn + fp) # TN = 0 where should be 0
  fscore = 2*precision*recall / (precision + recall)
  if(is.nan(fscore)) fscore = 0 # if tp+fn=0 or tp+fp=0
  if(fscore_only) {
    return(fscore)
  } else {
    return(tibble(thresh=thresh, precision=precision, recall=recall, 
                  fscore=fscore, specificity=specificity))
  }
} 


# 
get_tp <- function(m, gold_lexicon) {
  count = 0
  if (length(gold_lexicon) > 0) {
    for (i in 1:length(gold_lexicon[["word"]])) {
      word = gold_lexicon[["word"]][i]
      ref = gold_lexicon[["object"]][i]
      if (!(word %in% rownames(m)) | !(ref %in% colnames(m))) {
        next
      }
      count = count + m[word, ref]
    }
    return(count)
  } else {
    for (ref in colnames(m)) {
      if (!(ref %in% rownames(m))) {
        next
      }
      count = count + m[ref, ref]
    }
    return(count)
  }
}

# given model association matrix, returns df with all f-scores or f-scores + precision + recall
get_roc <- function(mdat, fscores_only=T, plot=F, gold_lexicon = c()) {
  #mat <- mdat / max(unlist(mdat)) # normalize so max value(s) in entire matrix are 1
  mat <- mdat / rowSums(mdat) # row-normalize matrix (better for all models?)
  threshes <- seq(0,1,.01)
  #fscores <- unlist(lapply(threshes, get_fscore, mat))
  prf <- bind_rows(lapply(threshes, get_fscore, mat, fscore_only=F, gold_lexicon = gold_lexicon))
  if(plot) {
    g <- prf %>% ggplot(aes(x=1-specificity, y=recall)) + geom_line() +
      theme_classic() + xlim(0,1) + ylim(0,1)
    print(g)
  }
  if(fscores_only) {
    return(prf$fscore)
  } else {
    return(prf)
  }
}

get_roc_max <- function(mdat, gold_lexicon = c()) {
  fscores <- get_roc(mdat, gold_lexicon = gold_lexicon)
  return(max(fscores[!is.na(fscores)]))
}

