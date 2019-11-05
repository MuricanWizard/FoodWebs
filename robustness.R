## function for structural robustness
## takes (x,y)
## x: sequential (sequential extinctions; output from Sim_...)
## y: S (number of species)
robustness <- function(x, y){
  
  ## cumulative secondary extinctions
  cum_sec <- apply(x, 1, cumsum)
  ## secondary extinctions of 50% of comm
  sec_50 <- apply(cum_sec >= (y/2),2, which)
  ## number of primary removals to get to 50%
  n_prim_50 <- unlist(lapply(sec_50, min))
  n_prim_50[n_prim_50 == Inf] <- ncol(x)
  
  str_robust <- n_prim_50 / y
  return(str_robust)
  
}


robustness(sequential, S)
