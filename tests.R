order_dir = "orders/"
model_dir = "models/"
data_dir = "data/"

load("data/combined_data.RData")

# also establish a baseline

source("fitting_functions.R")


runs <- list()

# baseline SSE=.620
# cond 214 has very high performance! (some items =.85)
runs[["baseline"]] <- run_model(combined_data, "baseline", c(), print_perf=T)

runs[["Bayesian_decay"]] <- run_model(combined_data, "Bayesian_decay", c(.9, 1.1, 1), print_perf=T) 
runs[["Bayesian_decay"]]$params <- c(.9, 1.1, 1)

runs[["kachergis"]] <- run_model(combined_data, "kachergis", c(.04, 2, .96), print_perf=T)
runs[["kachergis"]]$params <- c(.04, 2, .96)

runs[["novelty"]] <- run_model(combined_data, "novelty", c(.04, 2, .96), print_perf=T)
runs[["novelty"]]$params <- c(.04, 2, .96)

runs[["strength"]] <- run_model(combined_data, "strength", c(.04, .96), print_perf=T)
runs[["strength"]]$params <- c(.04, .96)

runs[["decay"]] <- run_model(combined_data, "decay", c(.96), print_perf=T)
runs[["decay"]]$params <- c(.96)

runs[["fazly"]] <- run_model(combined_data, "fazly", c(1e-5, 8500), print_perf=F)
runs[["fazly"]]$params <- c(1e-5, 8500)

runs[["fazlyt"]] <- run_model(combined_data, "fazlyt", c(1e-5, 8500), print_perf=F)
runs[["fazlyt"]]$params <- c(1e-5, 8500)

# Error!
# runs[["tilles"]] <- run_model(combined_data, "tilles", c(.5, .8, .85), print_perf=F)
# runs[["tilles"]]$params <- c(.5, .8, .85)

saveRDS(runs, file="orig_xsl-bakeoff_model_test_runs.rds")



# test pursuit model
par = group_fits$pursuit_detailed$optim$bestmem
mp = run_stochastic_model(combined_data[["orig_3x3"]], "pursuit_detailed", 
                         par, SSE_only = F, print_perf=T)

model(par, combined_data[["orig_3x3"]]$train)
ord = combined_data[["orig_3x3"]]$train