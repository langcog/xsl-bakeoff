require(tidyverse)
require(here)

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

# run once
process_FMcorpus <- function(resave=F) {
  if(resave) {
    fm_files = sort(list.files(here("data/FMcorpus/")))[1:24]
    fmc <- tibble() # makes sense to concatenate? or treat each child as an 'experimental condition'
    #fmc <- list() 
    # try concatenation; check referent/word overlap across kids
    for(file in fm_files) {
      tmp <- read_csv(here("data/FMcorpus", file))
      fmc <- bind_rows(fmc, tmp)
    }
    save(fmc, file=here("data/FMcorpus_processed.Rdata"))
  } else {
    load(here("data/FMcorpus_processed.Rdata"))
  }
  fm_ord <- create_scenes(fmc$utt, fmc$objects.present)
  load("data/FMcorpus/gold_fm.RData")
  
  FM_corpus = list(train = fm_ord, gold_lexicon = gold_fm)
  save(FM_corpus, file="XSLmodels/data/FM_corpus.Rdata")
}

# ToDo: split objects.present and utt into lists of items

#process_FMcorpus()
load("XSLmodels/data/FM_corpus.Rdata")
# ToDo: need gold_lexicon

# run once
process_FGTcorpus <- function() {
  fgt_w <- read_lines(here("data","FGT_data","words.txt"))
  fgt_o <- read_lines(here("data","FGT_data","objects.txt"))
  
  fgt_ord <- create_scenes(fgt_w, fgt_o)
  
  for (t in 1:length(fgt_ord$objs)) {
    if (fgt_ord$objs[t] == "NA") {
      fgt_ord$objs[t] <- NA
    }
  }
  
  fgt_gold <- read.csv(here("data","FGT_data","gold.txt"), header=F, sep=' ')
  names(fgt_gold) = c("word","object")
  FGT_corpus = list(train = fgt_ord, gold_lexicon = fgt_gold)
  save(FGT_corpus, file="XSLmodels/data/FGT_corpus.Rdata")
}  

load("XSLmodels/data/FGT_corpus.Rdata")


# OLD FUNCTIONS (superseded by ROC.R functions, yes?) - DELETE?

#get_fscore <- function(thresh, mat) {
#  tmat <- mat >= thresh
#  tp = sum(get_perf(tmat)) # correct referents selected
#  fp = sum(tmat) - tp # incorrect referents selected: all selected referents - TPs
#  fn = ncol(tmat) - tp # correct referents missed: num of words - TPs
#  precision = tp / (tp + fp)
#  recall = tp / (tp + fn)
#  return(2*precision*recall / (precision + recall))
#} # better to return c(precision, recall) so that we can do ROC curves?


#get_roc <- function(mdat) {
#  mat <- mdat$matrix / max(unlist(mdat$matrix)) # normalize so max value(s) are 1
#  threshes <- seq(0,1,.01)
#  fscores <- unlist(lapply(threshes, get_fscore, mat))
#  return(fscores)
#}

