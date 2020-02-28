# recreating figure 5.3
library(splines)

# generate the data 
set.seed(12)
x <- runif(50)
y <- x + rnorm(50)

# aside - note on bs function and how the intercept is included
# these two are equivalent
summary(fm1 <- lm(weight ~ 0 + bs(height, df = 4, intercept = TRUE), data = women))
summary(fm1 <- lm(weight ~ 1 + bs(height, df = 3, intercept = FALSE), data = women))

# fitting models
# global linear
H <- bs(x, degree = 1, df=2, intercept = TRUE, Boundary.knots = c(0,1))  
sigma_linear <- solve(t(H)%*%H)
var_linear <- diag(H%*%sigma_linear%*%t(H))

# global cubic polynomial
H <- bs(x, degree = 3, df=4, intercept = TRUE, Boundary.knots = c(0,1))
sigma_cubic <- solve(t(H)%*%H)
var_cubic <- diag(H%*%sigma_cubic%*%t(H))

# cubic spline with 2 knots 
H <- bs(x, degree = 3, df=6, intercept = TRUE, Boundary.knots = c(0,1), knots = c(0.33, 0.66))
sigma_spline <- solve(t(H)%*%H)
var_spline <- diag(H%*%sigma_spline%*%t(H))

# natural cubic spline with 6 knots 
knots <- seq(0.1, 0.9, length.out = 6)[2:5]  # 4 interior knots evenly spaced
H <- ns(x, intercept = TRUE, Boundary.knots = c(0.1,0.9), knots = knots)
sigma_natural <- solve(t(H)%*%H)
var_natural <- diag(H%*%sigma_natural%*%t(H))

# plotting the results
plot(x, var_cubic, ylim = c(0,0.6), col = 'red', pch = 16, 
     ylab = 'Pointwise Variances', xlab = 'X')
points(x, var_linear, col='orange', pch = 16)
points(x, var_spline, col = 'green3', pch = 16)
points(x, var_natural, col = 'navy', pch = 16)
lines(x[order(x)], var_linear[order(x)], col='orange')
lines(x[order(x)], var_spline[order(x)], col = 'green3')
lines(x[order(x)], var_cubic[order(x)], col = 'red')
lines(x[order(x)], var_natural[order(x)], col = 'navy')
legend(x=0.26, y=0.6, 
       legend = c('Global Linear', 'Global Cubic Polynomial', 'Global Spline - 2 knots', 
                  'Natural Cubic Spline - 6 knots'),
       col = c('orange', 'red', 'green3', 'navy'), lty = 1)



