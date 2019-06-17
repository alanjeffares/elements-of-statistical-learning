# Ex 3.2 simulation experiment 
# Generate data
set.seed(3)
N <- 20
x_values <- rnorm(N, 0, 2)
X <- matrix(c(x_values^0, x_values, x_values^2, x_values^3), nrow = N)  # data 
B <- rnorm(4, mean = 0, sd = 0.1)  # coefficients 
noise <- rnorm(N, sd=10)  # N(0,1)
y <- X[,1]*B[1] + X[,2]*B[2] + X[,3]*B[3] + X[,4]*B[4] + noise  # generate y


# Approach 1
store_upper <- list()
store_lower <- list()
store_y_true <- list()
vals <- seq(-5, 5, by=0.1)
for(val in vals){
  x_0 <- c(val^0, val^1, val^2, val^3)
  
  part_1 <- t(x_0)%*%solve(t(X)%*%X)%*%t(X)%*%matrix(y)
  part_2 <- 1.96*sqrt(t(x_0)%*%solve(t(X)%*%X)%*%matrix(x_0))
  y_true <- part_1
  upper <- part_1 + part_2
  lower <- part_1 - part_2
  store_y_true <- append(store_y_true, y_true)
  store_upper <- append(store_upper, upper)
  store_lower <- append(store_lower, lower)
}

# plot the results
plot(vals, unlist(store_lower), type ='n', xlab = expression(x[0]), ylab = expression(f(x[0])), 
     main = 'Simulation Experiment')
lines(vals, unlist(store_upper),lwd=2, col='lightsalmon2')
lines(vals, unlist(store_lower),lwd=2, col='lightsalmon2')
polygon(x=c(vals, rev(vals)), y = c(unlist(store_upper), rev(unlist(store_lower))),  col = 'lightsalmon2', border = NA)
lines(vals, unlist(store_y_true),lwd=3, col='black')
legend(x = 'topright', legend = c('Curve', '95% CI'), fill = c('black', 'lightsalmon2'), 
      box.lty=0, inset=.02, cex = 0.8)




