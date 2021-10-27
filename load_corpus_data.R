require(tidyverse)
require(here)

# run once
process_FMcorpus <- function() {
  fm_files = sort(list.files(here("data/FMcorpus/")))[1:24]
  fmc <- tibble() # makes sense to concatenate? or treat each child as an 'experimental condition'
  #fmc <- list() 
  # try concatenation; check referent/word overlap across kids
  for(file in fm_files) {
    tmp <- read_csv(here("data/FMcorpus", file))
    fmc <- bind_rows(fmc, tmp)
  }
  save(fmc, file=here("data/FMcorpus_processed.Rdata"))
}

# ToDo: split objects.present and utt into lists of items

#process_FMcorpus()
load(here("data/FMcorpus_processed.Rdata"))

fgt_w <- read_lines(here("data/FGT_data/words.txt"))
fgt_o <- read_lines(here("data/FGT_data/objects.txt"))

create_scenes <- function(words, objects) {
  train <- list(words = list(), objs = list())
  if(length(words)!=length(objects)) print("unequal number of words/objects")
  for(i in 1:length(words)) {
    words_tmp = unlist(str_split(str_trim(words[i]), ' '))
    objs_tmp = unlist(str_split(str_trim(objects[i]), ' '))
    if(length(words_tmp)>0) train$words[[i]] = words_tmp
    if(length(objs_tmp)>0) train$objs[[i]] = objs_tmp
  }
  return(train)
}

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

fm_ord <- create_scenes(fmc$utt, fmc$objects.present)

fgt_ord <- create_scenes(fgt_w, fgt_o)
for (t in 1:length(fgt_ord$objs)) {
  if (fgt_ord$objs[t] == "NA") {
    fgt_ord$objs[t] <- NA
  }
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

get_fscore <- function(thresh, mat) {
  tmat <- mat >= thresh
  tp = sum(get_perf(tmat)) # correct referents selected
  fp = sum(tmat) - tp # incorrect referents selected: all selected referents - TPs
  fn = ncol(tmat) - tp # correct referents missed: num of words - TPs
  precision = tp / (tp + fp)
  recall = tp / (tp + fn)
  return(2*precision*recall / (precision + recall))
} # better to return c(precision, recall) so that we can do ROC curves?


get_roc <- function(mdat) {
  mat <- mdat$matrix / max(unlist(mdat$matrix)) # normalize so max value(s) are 1
  threshes <- seq(0,1,.01)
  fscores <- unlist(lapply(threshes, get_fscore, mat))
  return(fscores)
}

