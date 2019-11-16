# load my two implementations of LAR
source('least_angle_regression.r')
source('least_angle_regression_la.r')

set.seed(2)
# generate some data to test the functions
X <- matrix(rnorm(100, 0, 1), nrow = 20)
Y <- 2*X[,1] - 0.5*X[,2] - 1.5*X[,3] + X[,4] -0.5*X[,5] + rnorm(20)

for (predictor_index in 1:ncol(X)){
  X[,predictor_index] <- (X[,predictor_index] - mean(X[,predictor_index]))/(sd(X[,predictor_index]))
}

# ensure that outputs match least squares for nsteps = P
least_angle_regression(X, Y, alpha = 0.001, nsteps = 5)
least_angle_regression_la(matrix(X, nrow = 20), Y, nsteps = 5)
lm(Y ~ X-1)


# now lets check coefficients match each step of the way by comparing to 
# the authors implementation
library(lars)
obj <- lars(x=X, y=Y, type = 'lar')
print('Lars package coefficients:')
coef(obj)

print('First impementation from least_angle_regression.R')
for (step in 1:5){
  print(least_angle_regression(X, Y, alpha = 0.001, nsteps = step))
}

print('Second impementation from least_angle_regression_la.R')
for (step in 1:5){
  print(least_angle_regression_la(X, Y, nsteps = step))
}


# finally lets plot the evolving values of coefficients
coefs_vec <- c()
for (step in 1:5){
  mod <- least_angle_regression_la(X, Y, nsteps = step, return_c_alpha = TRUE)
  coefs <- mod[[1]]
  coefs_vec <- c(coefs_vec, coefs)
  c_alphas = mod[[2]]
}

coefficient_matrix <- matrix(coefs_vec, nrow = 5)

y_lim_max <- max(coefficient_matrix)
y_lim_min <- min(coefficient_matrix)
x_lim_left = c_alphas[1] + 1
x_lim_right = c_alphas[5] - 1

plot(1, type="n", ylab="Coefficient Values", xlab = expression(c(alpha)), ylim=c(y_lim_min, y_lim_max), 
     xlim=c(x_lim_left, x_lim_right), xaxt = 'n', yaxt = 'n',
     main = "Coefficient Values Evolving in LAR")
axis(1, at=c_alphas, labels = round(c_alphas, digits = 1))
axis(2, at=round(seq(y_lim_min, y_lim_max , 0.5),1), las=1)
abline(v=c_alphas, lty=2, lwd=2, col='grey')

for (i in 1:nrow(coefficient_matrix)){
  coef_vals = coefficient_matrix[i,]
  lines(x=c_alphas, y=coef_vals, type='b', col=i+1, pch=16)
}


# compare the timings of the two approaches on this data 
# 1 - without look ahead 
ptm <- proc.time()
# 1000 repititions
for (i in 1:1000){
  mod <- least_angle_regression(X, Y, alpha = 0.001, nsteps = step)
}
no_look_ahead_timing = proc.time() - ptm

# 2 - with look ahead 
ptm <- proc.time()
# 1000 repititions
for (i in 1:1000){
  mod <- least_angle_regression_la(X, Y, nsteps = step)
}
look_ahead_timing = proc.time() - ptm

# using look ahead is >70 times faster in this case
no_look_ahead_timing['elapsed']/look_ahead_timing['elapsed']


