# script to generate splines for periodic data as derived in question 5.6

set.seed(11) 

period <- 2*pi
x = runif(100, 0, 2*period)
y = sin(x) + rnorm(100, sd=0.5)

# plot the data 
delta = 0.01
plot(x, y, type = 'n', xlim = c(0.3, 2*period - 0.3), xaxt='n', yaxt='n', xlab = '', ylab = '')
title(main = 'Cubic Splines for Periodic Data', cex.main = 1.2)
rect(0, par("usr")[3]+delta, period/3, par("usr")[4]-delta,col = "gold1", border = NA)
rect(period/3, par("usr")[3]+delta, 2*period/3, par("usr")[4]-delta,col = "gold3", border = NA)
rect(2*period/3, par("usr")[3]+delta, period, par("usr")[4]-delta, col = "gold1", border = NA)
rect(period, par("usr")[3]+delta, 4*period/3, par("usr")[4]-delta,col = "gold3", border = NA)
rect(4*period/3, par("usr")[3]+delta, 5*period/3, par("usr")[4]-delta, col = "gold1", border = NA)
rect(5*period/3, par("usr")[3]+delta, 2*period, par("usr")[4]-delta,col = "gold3", border = NA)
axis(side = 1, at = c(0, period, 2*period),
     labels = c(0, expression(T), expression(2*T)), cex.axis=1, line = +0.3, lwd = 0)
abline(v=period/3, lty=5)
abline(v=2*period/3, lty=5)
abline(v=period, lty=5)
abline(v=4*period/3, lty=5)
abline(v=5*period/3, lty=5)
points(x,y, cex = 1.5)

# plot the true underlying function
x_vals = seq(from=0, to=2*period, length.out = 200)
lines(x_vals, sin(x_vals), lwd=7, col='darkblue')

# subtract the period from observations until they are at their minimum positve value
x <- ifelse(x>period, x-period, x)

# fit a periodic cubic polynomial, continuous at the boundries 
basis <- cbind(rep(1,100), x**2 - period*x,  x**3 - (period**2)*x)
mod <- lm(y ~ 0 + basis[,1] + basis[,2] + basis[,3])
mod_vals = seq(from=0, to=period, length.out = 100)
mod_basis <- cbind(rep(1,100), mod_vals**2 - period*mod_vals,  mod_vals**3 - (period**2)*mod_vals)
y_mod <- mod$coefficients[1]*mod_basis[,1] + mod$coefficients[2]*mod_basis[,2] +
  mod$coefficients[3]*mod_basis[,3]
lines(mod_vals, y_mod, col='forestgreen', lwd = 4.5)
lines(mod_vals + period, y_mod, col='forestgreen', lwd = 4.5)

# first order continuous (NOT RUN)
basis <- cbind(rep(1,100), -(5/2)*x*period**2 + (3/2)*period*x**2 + x**3)
mod <- lm(y ~ 0 + basis[,1] + basis[,2])
mod_vals = seq(from=0, to=period, length.out = 100)
mod_basis <- cbind(rep(1,100), -(5/2)*mod_vals*period**2 + (3/2)*period*mod_vals**2 + mod_vals**3)
y_mod <- mod$coefficients[1]*mod_basis[,1] + mod$coefficients[2]*mod_basis[,2] 
#lines(mod_vals, y_mod)

# fitting a spline with 2 knots
positive_part <- function(x){x[x<0] <- 0; return(x)}

basis <- cbind(rep(1,100), x**2 - period*x, x**3 - x*period**2, 
               positive_part(x-period/3)**3 - x*((period - period/3)**3)/period,
               positive_part(x-2*period/3)**3 - x*((period - 2*period/3)**3)/period)
mod <- lm(y ~ 0 + basis[,1] + basis[,2] +  basis[,3] + basis[,4] + basis[,5])
mod_vals = seq(from=0, to=period, length.out = 100)
mod_basis <- cbind(rep(1,100), mod_vals**2 - period*mod_vals, mod_vals**3 - mod_vals*period**2, 
                   positive_part(mod_vals-period/3)**3 - mod_vals*((period - period/3)**3)/period,
                   positive_part(mod_vals-2*period/3)**3 - mod_vals*((period - 2*period/3)**3)/period)
y_mod <- mod$coefficients[1]*mod_basis[,1] + mod$coefficients[2]*mod_basis[,2] + mod$coefficients[3]*mod_basis[,3] +
   mod$coefficients[4]*mod_basis[,4] + mod$coefficients[5]*mod_basis[,5] 
lines(mod_vals, y_mod, col='violetred3', lwd = 4.5)
lines(mod_vals + period, y_mod, col='violetred3', lwd = 4.5)
legend(x = 4.05*period/3, y = 2, 
       legend = c('True function', 'Periodic global cubic polynomial', 'Periodic cubic polynomial spline'),
       col = c('darkblue', 'forestgreen', 'violetred3'), lty = 1, lwd = 3, cex = 0.65) 

