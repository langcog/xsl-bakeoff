order_dir = "orders/"
model_dir = "models/"
data_dir = "data/"

load("data/combined_data.RData")

# also establish a baseline

source("fitting_functions.R")

run_model(combined_data, "baseline", c(), print_perf=T) # baseline SSE=.620
# cond 214 has very high performance! (some items =.85)