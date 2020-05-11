# Script to recreate figure 7.4 on page 232. 
# This plot examines the performance of AIC for estimating optimal
# complexity for logistic regression with both log-likelihood and 0-1
# loss on the phoneme dataset. The AIC calculation doesnt strictly 
# apply here and the actual optimism from Efron (1986) is included 
# in the bottom of this script. 

library(splines)

# data available at https://web.stanford.edu/~hastie/ElemStatLearn/
data <- read.csv('Desktop/phoneme.csv')
data <- data[,-c(1,ncol(data))]
data <- data[(data$g == 'aa') | (data$g == 'ao'), ]
data$g <- sapply(data$g, FUN = function(x) if(x=='ao') 1 else 0)

# sample and split the data
set.seed(174) 
frequencies <- 1:256
train.idx <- sample(1:nrow(data), 1000)   # taking a random sample N=1000
train.data <- data[train.idx, ] 
test.data <- data[-train.idx, ] 
train.X <- train.data[, -257]
train.y <- train.data[, 257]
test.X <- test.data[, -257]
test.y <- test.data[, 257]


# log-likelihood loss plot
train.ll <- c()
test.ll <- c()
aic.ll <- c()

# logic for calculating for each num of basis functions
basis.funcs <- c(2,4,8,16,32,64,126,254)
for (i in 1:length(basis.funcs)){
  n.basis.funcs <- basis.funcs[i]
  H <- ns(frequencies, df = n.basis.funcs + 2, intercept = T)  # +2 for boundary knots
  Xstar = as.matrix(train.X)%*%as.matrix(H)
  smooth_mod <- glm(train.y ~ 0 + Xstar, family = binomial())
  n <- length(smooth_mod$y)
  aic.ll[i] <- smooth_mod$aic/n
  train.ll[i] <- -2 * sum(log((smooth_mod$y * smooth_mod$fitted.values) + 
                           ((1 - smooth_mod$y) * (1 - smooth_mod$fitted.values))))/n
  # predict on test set
  Xstar = as.matrix(test.X) %*% as.matrix(H)
  y.pred <- predict.glm(smooth_mod, as.data.frame(Xstar), type = 'response')
  test.ll[i] <- -2 * sum(log((test.y * y.pred) + 
                                ((1 - test.y) * (1 - y.pred))))/length(test.y)
  }

# create the log-likelihood plot
par(mfrow=c(1,2))
plot(train.ll, type = 'n', ylim = c(0.4, 2.5), main = 'Log-likelihood Loss', xaxt = 'n',
     xlab = 'Number of Basis Functions', ylab = 'Log-likelihood')
basis.funcs[8] = ''
axis(1, at = 1:8, lab = basis.funcs[1:8])
lines(train.ll, col = 'darkgoldenrod', type = "b", cex = 1.5, lwd = 1.5)
lines(aic.ll, col = 'darkgreen', type = "b", cex = 1.5, lwd = 1.5)
lines(test.ll, col = 'dodgerblue', type = "b", cex = 1.5, lwd = 1.5)
legend(x = 1, y = 2.5, legend = c('Train', 'Test', 'AIC'), col = c('darkgoldenrod', 'dodgerblue', 'darkgreen'), lty = 1)



# now repeat the previous logic for 0-1 loss
train.01 <- c()
test.01 <- c()
aic.01 <- c()

# an estimate of sigma hat squared as described on page 241.
# This estimated the error using the least restrictive (the overparameterised) model
H <- ns(frequencies, df = 256, intercept = T)  
Xstar = as.matrix(train.X)%*%as.matrix(H)
smooth_mod <- glm(train.y ~ 0 + Xstar, family = binomial())
sigma.hat.sq <- sum((train.y - mean(train.y))**2)/(n - 256)

basis.funcs <- c(2,4,8,16,32,64,126,254)
for (j in 1:length(basis.funcs)){
  n.basis.funcs <- basis.funcs[j]
  H <- ns(frequencies, df = n.basis.funcs + 2, intercept = T)  # +2 for boundary knots
  Xstar = as.matrix(train.X) %*% as.matrix(H)
  smooth_mod <- glm(train.y ~ 0 + Xstar, family = binomial())
  n <- length(smooth_mod$y)
  # 0-1 loss deviance
  deviance <- sum(abs((smooth_mod$y - round(smooth_mod$fitted.values))))
  aic.01[j] <- deviance/n + 2 * n.basis.funcs * sigma.hat.sq / n

  train.01[j] <- sum(abs(smooth_mod$y - round(smooth_mod$fitted.values)))/n
  # predict on test set
  Xstar = as.matrix(test.X) %*% as.matrix(H)
  y.pred <- predict.glm(smooth_mod, as.data.frame(Xstar), type = 'response')
  test.01[j] <- sum(abs(test.y - round(y.pred)))/length(test.y)
}

# create the 0-1 loss plot
plot(train.01, type = 'n', ylim = c(0.08, 0.35), main = '0-1 Loss', xaxt = 'n',
     xlab = 'Number of Basis Functions', ylab = 'Misclassification Error')
basis.funcs[8] = ''
axis(1, at = 1:8, lab = basis.funcs[1:8])
lines(train.01, col = 'darkgoldenrod', type = "b", cex = 1.5, lwd = 1.5)
lines(aic.01, col = 'darkgreen', type = "b", cex = 1.5, lwd = 1.5)
lines(test.01, col = 'dodgerblue', type = "b", cex = 1.5, lwd = 1.5)




# an estimate of omega (the optimism) from Efron (1986). This is what should be used
# for 0-1 loss rather than (2d/N)*sigma.hat.squared. This can easily be pasted into 
# 0-1 loss loop to compare to the rougher estimate. 
# ------------
sigma <- 0
pi.hat <- smooth_mod$fitted.values
n <- nrow(Xstar)
for (i in 1:n){
  ti <- as.matrix(Xstar[i,])
  sigma <- sigma +  (pi.hat[i] * (1 - pi.hat[i]) * ti %*% t(ti))
}

d <- diag(Xstar %*% solve(sigma) %*% t(Xstar))

# selecting C_0 = 0.5
alpha <- as.matrix(smooth_mod$coefficients)
c <- - Xstar %*% alpha

phi <- function(z) (2 * pi)**(-0.5) * exp(-0.5 * z**2)

omega <- 0
for (i in 1:n){
  omega <- omega + (pi.hat[i] * (1 - pi.hat[i]) * phi(c[i]/sqrt(d[i])) * sqrt(d[i]))
}
omega <- (2 * omega)/n

aic.01[j] <- deviance/n + omega
# ------------
