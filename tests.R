order_dir = "orders/"
model_dir = "models/"
data_dir = "data/"

load("data/combined_data.RData")

# also establish a baseline

source("fitting_functions.R")

run_model(combined_data, "baseline", c(), print_perf=T) # baseline SSE=.620
# cond 214 has very high performance! (some items =.85)





# test pursuit model
par = group_fits$pursuit_detailed$optim$bestmem
mp = run_stochastic_model(combined_data[["orig_3x3"]], "pursuit_detailed", 
                         par, SSE_only = F, print_perf=T)

model(par, combined_data[["orig_3x3"]]$train)
ord = combined_data[["orig_3x3"]]$train