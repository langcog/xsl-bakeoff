model_grid_eval <- function(model_name, conds, lower, upper, step_size=c(.05,.05)) {
  p1 = seq(lower[1], upper[1], step_size[1])
  p2 = seq(lower[2], upper[2], step_size[2])
  if(length(lower)>2) {
    p3 = seq(lower[3], upper[3], step_size[3])
    d = expand.grid(par1=p1, par2=p2, par3=p3)
    parcols = 3
  } else{
    d = expand.grid(par1=p1, par2=p2)
    parcols = 2
  }
  d$SSE = NA
  for(i in 1:nrow(d)) {
    if(is.element(model_name, stochastic_models)) {
      m = run_stochastic_model(conds, model_name, as.vector(d[i,1:parcols]))
    } else {
      m = run_model(conds, model_name, d[i,1:parcols])
    }
    d[i,]$SSE = m$SSE
    print(paste(i,"/",nrow(d),"SSE =",m$SSE,"par =",paste(d[i,1:parcols])))
  }
  return(d)
}

run_stochastic_model(combined_data, "trueswell2012", c(.01,.01)) # if 2nd parm is small (.01), error:
# Error in m[need_hypoths[w], new_hyps[w]] = alpha : incorrect number of subscripts on matrix
# In if (!is.element(hypo, tr_o)) { ... : the condition has length > 1 and only the first element will be used


mod_grids = list()
mod_grids[["fazly"]] = model_grid_eval("fazly", combined_data, c(1e-10,2), c(2,20000)) 
mod_grids[["trueswell2012"]] = model_grid_eval("trueswell2012", combined_data, lower=c(.01,.01), upper=c(1,1))

mod_grids[["guess-and-test"]] = model_grid_eval("guess-and-test", combined_data, lower=c(.01,.01), upper=c(1,1))
mod_grids[["pursuit_detailed"]] = model_grid_eval("pursuit_detailed", combined_data, 
                                                  lower=c(.01,.01,.01), upper=c(1,1,1), step_size=c(.1,.1,.1))

save(mod_grids, file="grid_evaluations.RData")
