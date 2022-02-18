

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