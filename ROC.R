# example
# load("data/combined_data.RData")
# source("models/fazly.R")

# group_fits$fazly$optim$bestmem 


get_fscore <- function(thresh, mat) {
  tmat <- mat >= thresh
  tp = get_tp(tmat) # correct referents selected
  fp = sum(tmat) - tp # incorrect referents selected: all selected referents - TPs
  fn = ncol(tmat) - tp # correct referents missed: num of words - TPs
  precision = tp / (tp + fp)
  recall = tp / (tp + fn)
  return(2*precision*recall / (precision + recall))
} # better to return c(precision, recall) so that we can do ROC curves?

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

get_roc <- function(mdat) {
  mat <- mdat / max(unlist(mdat)) # normalize so max value(s) are 1
  threshes <- seq(0,1,.01)
  fscores <- unlist(lapply(threshes, get_fscore, mat))
  return(fscores)
}

get_roc_max <- function(mdat) {
  fscores <- get_roc(mdat)
  return(max(fscores[!is.na(fscores)]))
}

