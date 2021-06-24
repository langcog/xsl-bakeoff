require(tidyverse)
require(here)

fm_files = sort(list.files(here("data/FMcorpus/")))[1:24]
fmc <- tibble() # makes sense to concatenate? or treat each child as an 'experimental condition'
#fmc <- list() 
# try concatenation; check referent/word overlap across kids
for(file in fm_files) {
  tmp <- read_csv(paste0(here("data/FMcorpus/"), file))
  fmc <- bind_rows(fmc, tmp)
}

# ToDo: split objects.present and utt into lists of items

save(fmc, file=here("data/FMcorpus_processed.Rdata"))


fgt_w <- read_lines(here("data/FGT_data/words.txt"))
fgt_o <- read_lines(here("data/FGT_data/objects.txt"))

create_scenes <- function(words, objects) {
  train <- list(words = list(), objs = list())
  if(length(words)!=length(objects)) print("unequal number of words/objects")
  for(i in length(words)) {
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

