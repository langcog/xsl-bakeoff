# convert matrices to lists

load(here("data/combined_data.RData"))

for(c in names(combined_data)) {
  new_train = list(words = list(), objs = list())
  old_train = combined_data[[c]]$train
  for(i in 1:nrow(old_train$words)) {
    new_train$words[[i]] = old_train$words[i,]
    new_train$objs[[i]] = old_train$objs[i,]
  }
  combined_data[[c]]$train = new_train
}

save(combined_data, file=here("data/combined_data_lists.Rdata"))