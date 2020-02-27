# recreating figure 5.1

# first generate some data from a distribution similar to that in the textbook
set.seed(5)
n = 50
x = runif(n, min=0, max=3)
# polynomial with first derivative tangent = 0 at x = 0.5, 1.5 and 3
true_func = function(x){0.25*x**4 - (5/3)*x**3 + (6.75/2)*x**2 - (2.25)*x + 10}
y = true_func(x) + rnorm(n,0,0.15)

# utility functions
positive_part <- function(x){x[x<0] <- 0; return(x)}
indicator_func <- function(x, min_, max_){
  return(ifelse((x >= min_) & (x < max_), 1, 0))
}

# script params
delta=0.01
xi_1 = 1
xi_2 = 2

par(mfrow=c(2,2))
# ------------------------------------------------------------------------------------
# plot the data for top left
plot(x, y, xlab = NA, ylab = NA, xaxt='n', yaxt='n', type = 'n', main = 'Piecewise Constant')
rect(0, par("usr")[3]+delta, xi_1, par("usr")[4]-delta,col = "gold1", border = NA)
rect(xi_1, par("usr")[3]+delta, xi_2, par("usr")[4]-delta,col = "gold3", border = NA)
rect(xi_2, par("usr")[3]+delta, 3, par("usr")[4]-delta, col = "gold1", border = NA)
abline(v=xi_1, lty=5)
abline(v=xi_2, lty=5)
points(x, y)
axis(side = 1, at = list(xi_1,xi_2), labels = c(expression(xi[1]), expression(xi[2])), cex.axis=1.5)
curve(true_func, from=0, to=3, add = TRUE,  col = 'darkblue', lwd = 3)


# fit piecewise constant
spline_mod <- lm(y ~ 0 + indicator_func(x, 0, xi_1) + indicator_func(x, xi_1, xi_2) + indicator_func(x, xi_2, 3))
predict_spline <- function(x, spline_mod, xi_1, xi_2){
  y <- indicator_func(x, 0, xi_1)*spline_mod$coefficients[[1]] + 
    indicator_func(x, xi_1, xi_2)*spline_mod$coefficients[[2]] + 
    indicator_func(x, xi_2, 3)*spline_mod$coefficients[[3]] 
  return(y)
}

plot_linear_segment <- function(min_, max_, spline_mod, xi_1, xi_2){
  epsilon = 0.0001  # deals with non continuous knots
  y0 = predict_spline(min_ + epsilon, spline_mod, xi_1, xi_2)
  y1 = predict_spline(max_ - epsilon, spline_mod, xi_1, xi_2)
  segments(x0=min_, y0 = y0, x1=max_, y1=y1, col='forestgreen', lwd = 3)
}
# add each of the segments individually
plot_linear_segment(0, xi_1, spline_mod, xi_1, xi_2)
plot_linear_segment(xi_1, xi_2, spline_mod, xi_1, xi_2)
plot_linear_segment(xi_2, 3, spline_mod, xi_1, xi_2)


# ------------------------------------------------------------------------------------
# plot the data for top right
plot(x, y, xlab = NA, ylab = NA, xaxt='n', yaxt='n', type = 'n', main = 'Piecewise Linear')
rect(0, par("usr")[3]+delta, xi_1, par("usr")[4]-delta,col = "gold1", border = NA)
rect(xi_1, par("usr")[3]+delta, xi_2, par("usr")[4]-delta,col = "gold3", border = NA)
rect(xi_2, par("usr")[3]+delta, 3, par("usr")[4]-delta, col = "gold1", border = NA)
abline(v=xi_1, lty=5)
abline(v=xi_2, lty=5)
points(x, y)
axis(side = 1, at = list(xi_1,xi_2), labels = c(expression(xi[1]), expression(xi[2])), cex.axis=1.5)
curve(true_func, from=0, to=3, add = TRUE,  col = 'darkblue', lwd = 3)


# fit piecewise linear
spline_mod <- lm(y ~ 0 + indicator_func(x, 0, xi_1) + indicator_func(x, xi_1, xi_2) +
                   indicator_func(x, xi_2, 3) + x:indicator_func(x, 0, xi_1) + 
                   x:indicator_func(x, xi_1, xi_2) + x:indicator_func(x, xi_2, 3))
predict_spline <- function(x, spline_mod, xi_1, xi_2){
  y <- indicator_func(x, 0, xi_1)*spline_mod$coefficients[[1]] + 
    indicator_func(x, xi_1, xi_2)*spline_mod$coefficients[[2]] + 
    indicator_func(x, xi_2, 3)*spline_mod$coefficients[[3]] +
    x*indicator_func(x, 0, xi_1)*spline_mod$coefficients[[4]] + 
    x*indicator_func(x, xi_1, xi_2)*spline_mod$coefficients[[5]] + 
    x*indicator_func(x, xi_2, 3)*spline_mod$coefficients[[6]]
  return(y)
}


plot_linear_segment <- function(min_, max_, spline_mod, xi_1, xi_2){
  epsilon = 0.0001  # deals with non continuous knots
  y0 = predict_spline(min_ + epsilon, spline_mod, xi_1, xi_2)
  y1 = predict_spline(max_ - epsilon, spline_mod, xi_1, xi_2)
  segments(x0=min_, y0 = y0, x1=max_, y1=y1, col='forestgreen', lwd = 3)
}
# add each of the segments individually
plot_linear_segment(0, xi_1, spline_mod, xi_1, xi_2)
plot_linear_segment(xi_1, xi_2, spline_mod, xi_1, xi_2)
plot_linear_segment(xi_2, 3, spline_mod, xi_1, xi_2)


# ------------------------------------------------------------------------------------
# plot the data for bottom left
plot(x, y, xlab = NA, ylab = NA, xaxt='n', yaxt='n', type = 'n', main = 'Continuous Piecewise Linear')
rect(0, par("usr")[3]+delta, xi_1, par("usr")[4]-delta,col = "gold1", border = NA)
rect(xi_1, par("usr")[3]+delta, xi_2, par("usr")[4]-delta,col = "gold3", border = NA)
rect(xi_2, par("usr")[3]+delta, 3, par("usr")[4]-delta, col = "gold1", border = NA)
abline(v=xi_1, lty=5)
abline(v=xi_2, lty=5)
points(x, y)
axis(side = 1, at = list(xi_1,xi_2), labels = c(expression(xi[1]), expression(xi[2])), cex.axis=1.5)
curve(true_func, from=0, to=3, add = TRUE,  col = 'darkblue', lwd = 3)

# fit continuous piecewise linear
spline_mod <- lm(y ~ 1 + x + positive_part(x-xi_1) + positive_part(x-xi_2))
predict_spline <- function(x, spline_mod, xi_1, xi_2){
  y <- spline_mod$coefficients[[1]] + x*spline_mod$coefficients[[2]] +
    positive_part(x-xi_1)*spline_mod$coefficients[[3]] + 
    positive_part(x-xi_2)*spline_mod$coefficients[[4]]
  return(y)
}

plot_linear_segment <- function(min_, max_, spline_mod, xi_1, xi_2){
  y0 = predict_spline(min_, spline_mod, xi_1, xi_2)
  y1 = predict_spline(max_, spline_mod, xi_1, xi_2)
  segments(x0=min_, y0 = y0, x1=max_, y1=y1, col='forestgreen', lwd = 3)
}
# add each of the segments individually
plot_linear_segment(0, xi_1, spline_mod, xi_1, xi_2)
plot_linear_segment(xi_1, xi_2, spline_mod, xi_1, xi_2)
plot_linear_segment(xi_2, 3, spline_mod, xi_1, xi_2)

# ------------------------------------------------------------------------------------
# plot the data for bottom right
plot(x, positive_part(x-xi_1), xlab = NA, ylab = NA, xaxt='n', yaxt='n', type = 'n', main = 'Piecewise-linear Basis Function')
rect(0, par("usr")[3]+delta, xi_1, par("usr")[4]-delta,col = "gold1", border = NA)
rect(xi_1, par("usr")[3]+delta, xi_2, par("usr")[4]-delta,col = "gold3", border = NA)
rect(xi_2, par("usr")[3]+delta, 3, par("usr")[4]-delta, col = "gold1", border = NA)
abline(v=xi_1, lty=5)
abline(v=xi_2, lty=5)
text(x=(xi_1+xi_2)/2, y=max(positive_part(x-xi_1))/2, labels = expression((X - xi[1])['+']), cex=1.5)
axis(side = 1, at = list(xi_1,xi_2), labels = c(expression(xi[1]), expression(xi[2])), cex.axis=1.5)

# add the two linear segments
segments(x0=0, y0 = positive_part(0-xi_1), x1=xi_1, y1=positive_part(xi_1-xi_1), col='forestgreen', lwd = 3)
segments(x0=xi_1, y0 = positive_part(xi_1-xi_1), x1=3, y1=positive_part(3-xi_1), col='forestgreen', lwd = 3)
points(x, positive_part(x-xi_1), pch=16)

# ------------------------------------------------------------------------------------

