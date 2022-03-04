# mostly imported from ROC.R

#' Evaluate m-alternative forced choice test
#'
#' This function evaluates a given set of test trials using the provided model memory matrix
#' (word x referent). Each test trial is assumed to present one word and a set of referents
#' of size less than the width of the model memory matrix.
#'
#' @return A vector with the probability of choosing the correct object, given each word.
#' @export
mafc_test <- function(mperf, test) {
  perf = rep(0, length(test$trials))
  for(i in 1:length(test$trials)) {
    w = test$trials[[i]]$word
    denom = sum(mperf[w, test$trials[[i]]$objs])
    perf[i] = mperf[w,w] / denom
  }
  return(perf)
}


#' Get true positives (TP), given a knowledge matrix and a gold lexicon.
#' 
#' This function iterates over words in a given gold lexicon and accumulates the associative
#' strength (can be integral e.g. 1, or real-valued) in the knowledge matrix for the intended 
#' referents (present in the gold lexicon). Returns the number of expected true positives (TP)
#' for this gold lexicon and knowledge matrix.
#' 
#' @return A single value with the expected number of true positives.
#' @export
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

#' Get f-score for a model knowledge matrix at a given threshold, with option gold lexicon.
#' 
#' long description
#' @return blah
#' @export
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


#' Get ROC scores.
#' 
#' Given model association matrix, returns dataframe with just f-scores (fscore_only=T), or
#' with f-scores, precision, and recall. Optionally accepts a gold lexicon
#' 
#' @return Dataframe with fscore or fscore, precision, and recall form thresholds [0, .01, .., 1].
#' @export
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


#' Get maximum ROC score.
#' 
#' maybe delete, or fold into get_roc ?
#' 
#' @return Maximum f-score (and threshold?)
#' @export
get_roc_max <- function(mdat, gold_lexicon = c()) {
  fscores <- get_roc(mdat, gold_lexicon = gold_lexicon)
  return(max(fscores[!is.na(fscores)]))
}