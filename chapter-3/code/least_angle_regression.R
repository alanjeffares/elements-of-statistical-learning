# Algorithm 3.2 - Least Angle Regression.
# This script implements the naive version where alpha is increased 
# incrementally by a configurable value until a new element of the 
# inactive set is equally correlated with the residual vector.

# helper functions
# get index of variable most correlated with current residual
find_most_correlated_x <- function(r, X){
  correlations <- abs(cor(x=X, y=r))
  max_index <- which(correlations == max(correlations))
  return(max_index)
}

# calculate direction to keep correlations tied and decreasing
delta_k <- function(X, active_set, r){
  solve(t(X[,active_set]) %*% X[,active_set]) %*% t(X[,active_set]) %*% r
}

# calculate residual for current active set
calculate_r <- function(Y, X, active_set, betas){
  r = Y - as.matrix(X[,active_set]) %*% betas[active_set]
  return(r)
}

# find correct alpha ... 
step_forward <- function(active_set, betas, X, r, Y, alpha, most_correlated){
  delta_val <- delta_k(X, active_set, r)

  while(any(most_correlated %in% active_set)){
    betas[active_set] <- betas[active_set] + alpha*delta_val
    r <- calculate_r(Y, X, active_set, betas)
    most_correlated <- find_most_correlated_x(r=r, X=X)
  }
  active_set <- c(active_set, most_correlated)
  return(list(active_set, betas, r, most_correlated, delta_val))
}

# main logic for LAR
least_angle_regression <- function(X, Y, alpha, nsteps){
  r <- Y - mean(Y)
  betas <- rep(0, ncol(X))
  
  most_correlated <- find_most_correlated_x(r=r, X=X) 
  active_set <- c(most_correlated)
  
  calculate_least_squares <- FALSE
  if(nsteps == ncol(X)){
    nsteps <- nsteps - 1
    calculate_least_squares <- TRUE
  }
  
  for(itteration in 1:nsteps){
    output <- step_forward(active_set, betas, X, r, Y, alpha, most_correlated)
    active_set <- output[[1]]
    betas <- output[[2]]
    r <- output[[3]]
    most_correlated <- output[[4]]
  }
  
  # if step = number of independant variables
  if(calculate_least_squares == TRUE){
    delta_val <- delta_k(X, active_set, r)
    betas[active_set] <- betas[active_set] + delta_val
  }
  return(betas)
}



