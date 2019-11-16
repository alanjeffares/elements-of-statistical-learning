# Algorithm 3.2 - Least Angle Regression with look ahead.  
# Implements LAR using my solution to ex. 3.25 in order to "look 
# ahead" and find the next value of alpha. 

# helper functions
# get index of variable most correlated with current residual
find_most_correlated_x <- function(r, X){
  correlations <- abs(cor(x=X, y=r))
  correlations <- round(correlations, 4)
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


# main logic for LAR
# return_c_alpha to optionally return c(alpha) values with coefficients
least_angle_regression_la <- function(X, Y, nsteps, return_c_alpha=FALSE){
  r <- Y - mean(Y)
  betas <- rep(0, ncol(X))
  alphas <- rep(0, ncol(X))
  c_alpha_vec = c()
  
  most_correlated <- find_most_correlated_x(r=r, X=X) 
  active_set <- c(most_correlated)
  variables <- 1:ncol(X)
  
  for(step in 1:nsteps){
    delta_val <- delta_k(X, active_set, r)
    Xa <- X[,active_set[1]]
    c_a_alpha = sign(t(Xa)%*%r)
    if (return_c_alpha){
      c_alpha_vec <- c(c_alpha_vec, abs(t(Xa)%*%r))
    }
    
    inactive_set <- variables[-active_set]
    
    # if last variable calculate betas and finish
    # should equal least squares estimates
    if (length(inactive_set)==0){
      betas[active_set] <- betas[active_set] + delta_val
      if (return_c_alpha){
        return(list(betas, c_alpha_vec))
      }
      else{
        return(betas)
      }
    }
    
    for (i in inactive_set){
      
      Xb <- X[,i]
      c_b_alpha = sign(t(Xb)%*%r)
      
      # see my solution to Ex 3.25 for derivation of this formula
      # positive case
      if(c_a_alpha==c_b_alpha){
        numerator =  t(Xa)%*%r - t(Xb)%*%r
        denominator =  t(Xa)%*%X[,active_set]%*%delta_val - t(Xb)%*%X[,active_set]%*%delta_val
      }
      # negative case
      else{
        numerator =  t(Xa)%*%r + t(Xb)%*%r
        denominator =  t(Xa)%*%X[,active_set]%*%delta_val + t(Xb)%*%X[,active_set]%*%delta_val
      }
      
      alphas[i] <- abs(numerator/denominator)
      
    }
    alpha <- min(alphas[inactive_set])    
    most_correlated <- which(alphas == alpha)
    betas[active_set] <- betas[active_set] + alpha*delta_val
    r <- calculate_r(Y, X, active_set, betas)
    most_correlated <- find_most_correlated_x(r=r, X=X)
    active_set <- unique(c(active_set, most_correlated))
  }
  if (return_c_alpha){
    return(list(betas, c_alpha_vec))
  }
  else{
    return(betas)
  }

}




