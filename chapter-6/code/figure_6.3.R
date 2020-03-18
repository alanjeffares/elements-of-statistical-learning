# script to recreate figure 6.3 comparing Nadaraya–Watson kernel-weighted average
# to local linear regression at the boundary where the latter reduces the bias
set.seed(1)

X <- runif(100)
e <- rnorm(100, sd =1/3)
f <- function(x){sin(4*x)}
Y <- f(X) + e

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
x0 <- 0.05
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
title('N-W Kernel at Boundary', line = 0.3, cex.main=0.9)
axis(side=1, at=x0, labels = expression('x'[0]), col = 'red', cex.axis=1.2)
polygon(x=c(kernel_X, 0), c(scaled_kernel_Y, 0), col = 'khaki1', border = F)
lines(x_vals , y_vals, col = 'green', lwd = 2)
points(X[x0_ys >0], Y[x0_ys >0], col = 'red')
y0 <- nw_average(x0, lambda = 0.2, X, Y)
curve(f, add = T, lwd=2, col = 'blue')
points(x0, y0, col = 'red', pch =16, cex = 1.2)
segments(x0 = x0, y0 = -1.3, x1 = x0, y1=y0, col = 'red', lwd = 2)
segments(x0 = min(X[x0_ys >0]), y0 = y0,
         x1 = max(X[x0_ys >0]), y1 = y0, col = 'red', lwd = 2)
text(x = x0, y = y0 + 0.25, labels = expression(hat(f)(x[0])), font = 2)


# local linear regression
# locally weighted regression at x0
lwr <- function(x0, lambda, X, Y){
  b <- matrix(c(1, x0))
  B <- cbind(rep(1, length(X)), X)
  W <- diag(sapply(X, K, x0 = x0, lambda = lambda))
  return(t(b) %*% solve(t(B) %*% W %*% B) %*% t(B) %*% W %*% Y)  # eqn 6.8
}

# apply this kernel approach to range of x vals
x_vals <- seq(from=0, to=1, length.out = 200)
y_vals <- c()
for(x in x_vals){
  y_vals <- append(y_vals, lwr(x, lambda = 0.2, X, Y))
}

# find coefficients at x0
b <- matrix(c(1, x0))
B <- cbind(rep(1, length(X)), X)
W <- diag(sapply(X, K, x0 = x0, lambda = 0.2))
coeffs <- solve(t(B) %*% W %*% B) %*% t(B) %*% W %*% Y
y1 <- t(matrix(c(1,min(X[x0_ys >0])))) %*% coeffs
y2 <- t(matrix(c(1,max(X[x0_ys >0])))) %*% coeffs

# plot results 
plot(X, Y, xlim = c(0,1), ylim = c(-1.3,1.5), ylab='', xlab = '')
title('Local Linear Regression at Boundary', line = 0.3, cex.main=0.9)
axis(side=1, at=x0, labels = expression('x'[0]), col = 'red', cex.axis=1.2)
polygon(x=c(kernel_X, 0), c(scaled_kernel_Y, 0), col = 'khaki1', border = F)
lines(x_vals , y_vals, col = 'green', lwd = 2)
points(X[x0_ys >0], Y[x0_ys >0], col = 'red')
y0 <- lwr(x0, lambda = 0.2, X, Y)
curve(f, add = T, lwd=2, col = 'blue')
points(x0, y0, col = 'red', pch =16, cex = 1.2)
segments(x0 = x0, y0 = -1.3, x1 = x0, y1=y0, col = 'red', lwd = 2)
segments(x0 = min(X[x0_ys >0]), y0 = y1,
         x1 = max(X[x0_ys >0]), y1 = y2, col = 'red', lwd = 2)
text(x = x0, y = y0 + 0.25, labels = expression(hat(f)(x[0])), font = 2)



