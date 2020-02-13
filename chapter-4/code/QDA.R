library(mvtnorm)
library(datasets)

# main logic for fitting QDA using discriminant function (eqn 4.12) & "Computations for LDA" 4.3.2
QDA <- function(data, target){
  unique(data[target])
  classes <- unique(data[,target])
  K <- length(classes)
  N <- nrow(data)
  pi_list <- vector('list', length=K)  # store k pi_k's
  mu_list <- vector('list', length=K)  # store k mu_k's
  sigma_list <- vector('list', length=K)  # store k sigma_k's
  
  # loop through classes to get parameter estimates by class
  # eg pi_k, mu_k and sigma_k
  i <- 0
  for (class in classes){
    i <- i+1
    subset_df <- data[data[,target] == class,]
    N_k <- nrow(subset_df)
    pi_k <- N_k/N  # 1 x 1
    X_subset <- subset_df[ , !names(subset_df) %in% c(target)]
    mu_k <- colSums(X_subset)/N_k  # p x 1 
    X_subset_cent <- sweep(X_subset, 2, mu_k, FUN = '-')
    sigma_k <- cov(X_subset_cent)
    pi_list[[i]] <- pi_k
    mu_list[[i]] <- mu_k
    sigma_list[[i]] <- sigma_k
  }
  # return estimates for pi, mu and sigma
  output <- list(pi_list, mu_list, sigma_list)
  names(output) <- list('pi_k', 'mu_k', 'sigma_k')
  return(output)
}

predict_discriminants <- function(x, pi_list, mu_list, sigma_list, K=length(pi_list)){
  scores <- c()
  # apply eqn 4.12 for each of the K classes
  for (i in 1:K){
    sigma <- sigma_list[[i]]
    mu <- mu_list[[i]]
    pi <- pi_list[[i]]
    
    # see section 4.3.2 on p113 for details
    sphered <- eigen(sigma)
    U <- sphered$vectors
    D <- diag(sphered$values)
    
    a <- t(t(U) %*% (t(x) - mu)) %*% solve(D) %*% t(U) %*% (t(x) - mu)
    b <- sum(diag(log(D)))
    
    score <- as.numeric(-0.5*a - 0.5*b + log(pi))
    scores[[i]] <- score
  }
  return(scores)
}

# predict label for a single observation
predict_label <- function(x, pi_list, mu_list, sigma_list){
  preds <- predict_discriminants(x, pi_list, mu_list, sigma_list)
  return(which.max(preds))
}

# predict for a full data.frame of observations
predict_labels <- function(X, pi_list, mu_list, sigma_list){
  predictions <- c()
  for (i in 1:nrow(X)){
    x <-  as.matrix(X[i,])
    label_i = predict_label(x, pi_list, mu_list, sigma_list)
    predictions[i] <- label_i
  }
  return(predictions)
}


# first fit to the data 
data(iris)
data_flat <- iris[,c(1,2,5)]
fit <- QDA(data_flat, target = 'Species')

# make a grid of values to predict on (so we can see the rough boundries)
delta <- 0.05
x_vals = seq(from=min(data_flat$Sepal.Length)-delta, to=max(data_flat$Sepal.Length)+delta, length.out = 200)
y_vals = seq(from=min(data_flat$Sepal.Width)-delta, to=max(data_flat$Sepal.Width)+delta, length.out = 200)
grid <- expand.grid(x=x_vals, y_vals)

# predict the label of each point on the grid
preds <- predict_labels(grid, fit$pi_k, fit$mu_k, fit$sigma_k)

# visualise the results
palette(c("skyblue","palegreen1","pink"))
plot(x=data_flat$Sepal.Length, data_flat$Sepal.Width, xlab = 'Sepal Length', ylab = 'Sepal Width', type = 'n')
title('QDA decision boundries on first 2 dimensions of Iris data', line = 0.4)
points(x=grid[,1], y=grid[,2], col=preds, pch=20, cex=0.1)  # plot the colured grid in the background

# optionally include contour plots
levels <- list(c(0.5, 1, 1.5), c(0.5, 0.75, 1), c(0.5, 0.65, 0.8))  # custom contours
nlevels <- c(3, 3, 2)
x.points <- seq(4,8,length.out=100)
y.points <- seq(1,5,length.out=100)
for (k in 1:length(fit$mu_k)){
  z <- matrix(0,nrow=100,ncol=100)
  mu <- fit$mu_k[[k]]
  sigma <- fit$sigma_k[[k]]
  for (i in 1:100) {
    for (j in 1:100) {
      z[i,j] <- dmvnorm(c(x.points[i],y.points[j]),
                        mean=mu,sigma=sigma)
    }
  }
  contour(x.points, y.points, z, add = TRUE, levels = levels[[k]])
}

palette(c("blue","seagreen4","orangered3"))
points(data_flat$Sepal.Length, data_flat$Sepal.Width, 
       col=data_flat$Species, pch=16, cex=1.25)  # plot the true vals coloured by their actual label




# testing performance on vowel data (https://web.stanford.edu/~hastie/ElemStatLearn/)
test <- read.csv('/Users/alan.jeffares/Desktop/voweltest.csv')
train <- read.csv('/Users/alan.jeffares/Desktop/voweltrain.csv')

v_fit <- QDA(train[,2:12], target = 'y')

# confirm that these predictions match those in table 4.1 (p 107)
train_preds <- predict_labels(train[,3:12], v_fit$pi_k, v_fit$mu_k, v_fit$sigma_k)
train_accuracy <- sum(train_preds == train$y)/nrow(train)
train_error <- 1 - train_accuracy
print(paste0('Train error: ', train_error))

test_preds <- predict_labels(test[,3:12], v_fit$pi_k, v_fit$mu_k, v_fit$sigma_k)
test_accuracy <- sum(test_preds == test$y)/nrow(test)
test_error <- 1 - test_accuracy
print(paste0('Test error: ', test_error))

# and they match!


