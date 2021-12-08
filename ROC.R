# example
# load("data/combined_data.RData")
# source("models/fazly.R")

# group_fits$fazly$optim$bestmem 


get_fscore <- function(thresh, mat, fscore_only=T, gold_lexicon) {
  tmat <- mat >= thresh
  tp = get_tp(tmat, gold_lexicon) # correct referents selected
  fp = sum(tmat) - tp # incorrect referents selected: all selected referents - TPs
  fn = ncol(tmat) - tp # correct referents missed: num of words - TPs
  precision = tp / (tp + fp) 
  recall = tp / (tp + fn) # aka sensitivity / true positive rate
  tn = sum(!tmat) - fn # all the 0s that should be 0s
  specificity = tn / (tn + fp) # TN = 0 where should be 0
  fscore = 2*precision*recall / (precision + recall)
  if(is.nan(fscore)) fscore = 0 # if tp+fn=0 or tp+fp=0
  if(fscore_only) {
    return(fscore)
  } else {
    return(tibble(precision=precision, recall=recall, 
                  fscore=fscore, specificity=specificity))
  }
} 


# 
get_tp <- function(m) {
  count = 0
  for (ref in colnames(m)) {
    if (!(ref %in% rownames(m))) {
      next
    }
    count = count + m[ref, ref]
  }
  return(count)
}

# given model association matrix, returns df with all f-scores or f-scores + precision + recall
get_roc <- function(mdat, fscores_only=T, plot=F) {
  #mat <- mdat / max(unlist(mdat)) # normalize so max value(s) in entire matrix are 1
  mat <- mdat / rowSums(mdat) # row-normalize matrix (better for all models?)
  threshes <- seq(0,1,.01)
  #fscores <- unlist(lapply(threshes, get_fscore, mat))
  prf <- bind_rows(lapply(threshes, get_fscore, mat, fscore_only=F))
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

get_roc_max <- function(mdat) {
  fscores <- get_roc(mdat)
  return(max(fscores[!is.na(fscores)]))
}

