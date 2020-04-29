# recreating figure 7.1 by generating a synthetic dataset with 35 variables and modelling 
# using LAR using the lasso modification. 100 train and test sets are generated with the
# RSS of each plotted along with their avarages.

library(lars)
library(scales)
library(shape)

# note lars uses Algorithm 3.2a (p 76) which makes it identical to lasso estimates.
# However it may have more than p steps as steps may now include dropping variables as well as adding them. 
# We will only use the final inclusian of any given variable in order to ensure that we have a single 
# unique fit for each of the 35 degrees of freedom.

# need function to mark last element of each digit as True in vector 
# this is an O(n) implementation 
match.actions.to.dof <- function(actions){
  bool.vals <- c()
  for (i in 1:length(actions)){
    # check if action value is positive
    if (actions[i] > 0) bool.vals[i] <- T
    else{
      bool.vals[i] <- F
      # and change previous existing T value from bool.vals to F
      j <- i - 1
      while(T){
        if (bool.vals[j]){
          bool.vals[j] <- F
          break
        }
        else{
          j <- j - 1
        }
      }
    }
  }
  return(bool.vals)
}

# experiment parameters
train.size <- 50
test.size <- 1000
N <- train.size + test.size

# select noise in data
set.seed(7) 
sd_X <- 1
sd_beta <- 0.145
sd_noise <- 0.45

train.RSS.df <- matrix(ncol=35, nrow=100)
test.RSS.df <- matrix(ncol=35, nrow=100)

plot(c(1), type = 'n', ylim = c(0,1.2), xlim = c(0,35), 
     xlab = 'Model Complexity (df)', ylab = 'Prediction Error')
# define the population to draw from
beta <- as.matrix(rnorm(35, sd = sd_beta))
for (i in 1:100){
  noise <- as.matrix(rnorm(N, mean = 0, sd = sd_noise))
  X <- matrix(rnorm(N * nrow(beta), sd = sd_X), nrow = N)
  Y <- X %*% beta + noise
  
  train.X <- X[1:train.size,]
  test.X <- X[-(1:train.size),]
  train.Y <- Y[1:train.size,]
  test.Y <- Y[-(1:train.size),]
  
  # fit lasso via lars algorithm
  mod <- lars(x = train.X, y = train.Y, type = 'lasso', normalize = T, 
              intercept = F )
  preds <- predict(mod, test.X)
  
  # only count variables the final time they are entered into the active set
  actions <- unlist(mod$actions)
  unique.dof <- match.actions.to.dof(actions)
  
  # train RSS for each of the p models (given in mod output)
  train.RSS <- mod$RSS[unique.dof][-1]/train.size
  
  # calculate test RSS for each of the p lasso models 
  pred.Y <- preds$fit[,unique.dof]  # preds for each step on the test data
  pred.Y <- pred.Y[,-1]  # drop first col
  # calculate Err_T = (1/n)*Sum((y_true - y_pred)^2) across test set
  test.RSS <- diag(t(pred.Y - test.Y) %*% (pred.Y - test.Y))/test.size
  
  # plot train & test RSS
  lines(train.RSS, col = alpha(rgb(102, 178, 255, maxColorValue = 255), 0.5))
  lines(test.RSS, col =  alpha(rgb(255, 20, 0, maxColorValue = 255), 0.2))
  
  # store this itterations results
  train.RSS.df[i,] <- train.RSS
  test.RSS.df[i,] <- test.RSS
}
# finally add expected training and test error to plot
lines(colSums(train.RSS.df)/100, col = 'darkblue', lwd = 3)
lines(colSums(test.RSS.df)/100, col = 'red', lwd = 3)

# add text and arrows
text(7.5, 1.18, 'High Bias\nLow Variance', cex = 0.7)
Arrows(x0 = 9, y0 = 1.1, x1 = 6, y1 = 1.1, arr.type = 'triangle', 
       arr.lwd = 0.1, arr.length = 0.3)
text(29.5, 1.18, 'Low Bias\nHigh Variance', cex = 0.7)
Arrows(x0 = 28, y0 = 1.1, x1 = 31, y1 = 1.1, arr.type = 'triangle', 
       arr.lwd = 0.1, arr.length = 0.3)




