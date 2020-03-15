# script to recreate figure 6.1 applying both k-nn and Nadaraya–Watson kernel-weighted
# average to some synthetically generated data 
set.seed(1)

X <- runif(100)
e <- rnorm(100, sd =1/3)
f <- function(x){sin(4*x)}
Y <- f(X) + e

# K-NN
# get indexes of k nearest neighbours to x0
k_nn_idx <- function(X, x0, k) {
  k_nn_bool <- rep(F, length(X))
  temp_X <- X
  while(sum(k_nn_bool) < k){
    min_idx <- which.min(abs(temp_X-x0))
    k_nn_bool[which(X == temp_X[min_idx])] <- TRUE
    temp_X <- temp_X[-min_idx]  
  }
  return(k_nn_bool)
}
# we will superimpose a particular x0 value
x0 <- 0.5
c <- k_nn_idx(X, x0, 30)

# apply k-nn to range of x vals
x_vals <- seq(from=0, to=1, length.out = 200)
y_vals <- c()
for(x in x_vals){
  idxs <- k_nn_idx(X, x, 30)
  y_vals <- append(y_vals, mean(Y[idxs]))
}

# plot results 
par(mfrow=c(1,2))
plot(X,Y, xlim = c(0,1), ylim = c(-1.3,1.5), ylab='', xlab = '')
title('Nearest-Neighbour Kernel', line = 0.3, cex.main=0.9)
axis(side=1, at=0.5, labels = expression('x'[0]), col = 'red', cex.axis=1.2)
kernel_max <- 1.4
kernel_min <- 0
rect(min(X[c]), kernel_min, max(X[c]), kernel_max, col = "khaki1", border = NA)
lines(x_vals , y_vals, col = 'green', lwd=2)
points(X[c], Y[c], col = 'red')
points(x0, mean(Y[c]), col = 'red', pch =16)
curve(f, add = T, lwd=2, col = 'blue')
segments(x0 = x0, y0 = -1.3, x1 = x0, y1=mean(Y[c]), col = 'red')
segments(x0 = min(X[c]), y0 = mean(Y[c]), x1 = max(X[c]), y1=mean(Y[c]), col = 'red')
text(x = x0, y = mean(Y[c]) + 0.15, labels = expression(hat(f)(x[0])), font = 2)


# Epanechnikov Kernel
# define the quadratic kernel function
K <- function(x, x0, lambda){
  t <- abs(x - x0)/lambda
  if(abs(t) <= 1){
    return(0.75*(1 - t**2))
  }
  else{
    return(0)
  }
}

# Nadaraya–Watson kernel-weighted average
nw_average <- function(x0, lambda, X, Y){
  numerator <- c()
  for (i in 1:length(X)) {
    numerator <- append(numerator, K(X[i], x0, lambda)*Y[i])
  }
  denom <- c()
  for (i in 1:length(X)) {
    denom <- append(denom, K(X[i], x0, lambda))
  }
  return(sum(numerator)/sum(denom))
}

# visualise the kernel at x0 and superimpose on plot
x0 <- 0.5
weights <- c()
x_vals <- seq(from=0, to=1, length.out = 200)
for (x in x_vals){
  weight <- K(x, x0, lambda=0.2)
  weights <- append(weights, weight)
}
kernel_X <- x_vals[weights > 0]
kernel_Y <- weights[weights > 0]
scaled_kernel_Y <- ((kernel_Y-min(kernel_Y))/(max(kernel_Y)-min(kernel_Y))) * kernel_max

# find the points influenced by this kernel
x0_ys <- c()
for (x in X){
  x0_y <- K(x, x0, lambda=0.2)
  x0_ys <- append(x0_ys, x0_y)
}

# apply this kernel approach to range of x vals
x_vals <- seq(from=0, to=1, length.out = 200)
y_vals <- c()
for(x in x_vals){
  y_vals <- append(y_vals, nw_average(x0=x, lambda = 0.2, X, Y))
}

# plot results 
plot(X, Y, xlim = c(0,1), ylim = c(-1.3,1.5), ylab='', xlab = '')
title('Epanechnikov Kernel', line = 0.3, cex.main=0.9)
axis(side=1, at=0.5, labels = expression('x'[0]), col = 'red', cex.axis=1.2)
polygon(x=c(kernel_X), c(scaled_kernel_Y), col = 'khaki1', border = F)
lines(x_vals , y_vals, col = 'green', lwd = 2)
points(X[x0_ys >0], Y[x0_ys >0], col = 'red')
y0 <- nw_average(x0, lambda = 0.2, X, Y)
curve(f, add = T, lwd=2, col = 'blue')
points(x0, y0, col = 'red', pch =16, cex = 1.2)
segments(x0 = x0, y0 = -1.3, x1 = x0, y1=y0, col = 'red')
segments(x0 = min(X[x0_ys >0]), y0 = y0,
         x1 = max(X[x0_ys >0]), y1 = y0, col = 'red')
text(x = x0, y = y0 + 0.15, labels = expression(hat(f)(x[0])), font = 2)

