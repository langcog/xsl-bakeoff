# example
load("data/combined_data.RData")
source("models/fazly.R")

# group_fits$fazly$optim$bestmem 


get_fscore <- function(thresh, mat) {
  tmat <- mat >= thresh
  tp = sum(diag(tmat)) # correct referents selected
  fp = sum(tmat) - tp # incorrect referents selected: all selected referents - TPs
  fn = nrow(tmat) - tp # correct referents missed: num of words - TPs
  precision = tp / (tp + fp)
  recall = tp / (tp + fn)
  return(2*precision*recall / (precision + recall))
}


get_roc <- function(mdat) {
  mat <- mdat$matrix / max(unlist(mdat$matrix)) # normalize so max value(s) are 1
  threshes <- seq(0,1,.01)
  fscores <- lapply(threshes, get_fscore, mat)
}


m <- model(c(0.473378, 3.933056), combined_data[[1]]$train)

fs <- get_roc(m)