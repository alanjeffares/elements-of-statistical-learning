# Implementation of algorithm 3.1 on page 54

# helper functions
regress_b_on_a <- function(a, b){
  # following procedure on page 53
  top <- t(a)%*%b
  bottom <- t(a)%*%a
  beta <- top/bottom
  return(as.numeric(beta))
}

get_residual <- function(a, b, gamma){
  gamma_a <- matrix(rep(0, nrow(a[[1]])))
  for (k in 1:length(gamma)){
    gamma_a <- gamma_a + gamma[k]*a[[k]]
  }
  return(b - gamma_a)
}


# Gram-Schmidt algorithm (3.1 on page 54)
GramSchmidt <- function(X, y) {
  # initialise Z and X_0
  Z <- list()
  Z[[1]] <- X_0 <- matrix(rep(1, nrow(X)))
  # follow steps 2 for each column in the data
  for (j in 1:ncol(X)){
    gamma <- matrix(nrow = j)
    for (l in 1:j){
      gamma[l] <- regress_b_on_a(a=Z[[l]], b=X[,j])
    }
    Z[[j+1]] <- get_residual(a=Z, b=X[,j], gamma)
  }
  # step 3: calculate beta p
  print(paste0('Beta', j, ': ', regress_b_on_a(Z[[j+1]], y)))
}


# test procedure on generated data  
X <- matrix(c(4, 7, 9, 1, 8, 4), 3)
y =  X[,1]*2 + X[,2]*3 + rnorm(3)

GramSchmidt(X,y)

# and compare to lm estimate
lm(y ~ X[,1] + X[,2] )
