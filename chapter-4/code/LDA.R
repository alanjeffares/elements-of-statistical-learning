
# main logic for fitting LDA using discriminant function (eqn 4.10)
LDA <- function(data, target){
  unique(data[target])
  classes <- unique(data[,target])
  K <- length(classes)
  N <- nrow(data)
  pi_list <- vector('list', length=K)  # store k pi_k's
  mu_list <- vector('list', length=K)  # store k mu_k's
  sigma_list <- vector('list', length=K)  # store k cov matrices to sum
  
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
    sigma_k <- cov(X_subset_cent)*(N_k-1)  # N_k-1 as cov function has this on the denominator
    pi_list[[i]] <- pi_k
    mu_list[[i]] <- mu_k
    sigma_list[[i]] <- sigma_k
  }
  sigma <- Reduce('+', sigma_list)/(N-K)
  # return estimates for pi, mu and sigma
  output <- list(pi_list, mu_list, sigma)
  names(output) <- list('pi_k', 'mu_k', 'sigma')
  return(output)
}


predict_discriminants <- function(x, pi_list, mu_list, sigma, K=length(pi_list)){
  scores <- c()
  # apply eqn 4.10 for each of the K classes
  for (i in 1:K){
    a <- x %*% solve(sigma) %*% mu_list[[i]]
    b <- 0.5*t(mu_list[[i]]) %*% solve(sigma) %*% mu_list[[i]]
    score <- as.numeric(a - b + log(pi_list[[i]]))
    scores[[i]] <- score
  }
  return(scores)
}

# predict label for a single observation
predict_label <- function(x, pi_list, mu_list, sigma){
  preds <- predict_discriminants(x, pi_list, mu_list, sigma)
  return(which.max(preds))
}

# predict for a full data.frame of observations
predict_labels <- function(X, pi_list, mu_list, sigma){
  predictions <- c()
  for (i in 1:nrow(X)){
    x <-  as.matrix(X[i,])
    label_i = predict_label(x, pi_list, mu_list, sigma)
    predictions[i] <- label_i
  }
  return(predictions)
}



# now lets test out the fuction on a subset of the iris dataset
data_flat <- data[,c(1,2,5)]
# first fit to the data 
fit <- LDA(data_flat, target = 'Species')

# make a grid of values to predict on (so we can see the rough boundries)
delta <- 0.05
x_vals = seq(from=min(data$Sepal.Length)-delta, to=max(data$Sepal.Length)+delta, length.out = 200)
y_vals = seq(from=min(data$Sepal.Width)-delta, to=max(data$Sepal.Width)+delta, length.out = 200)
grid <- expand.grid(x=x_vals, y_vals)

# predict the label of each point on the grid
preds <- predict_labels(grid, fit$pi_k, fit$mu_k, fit$sigma)

# visualise the results
palette(c("skyblue","palegreen1","pink"))
plot(data$Sepal.Length, data$Sepal.Width, xlab = 'Sepal Length', ylab = 'Sepal Width', type = 'n')
title('LDA decision boundries on first 2 dimensions of Iris data', line = 0.4)
points(x=grid[,1], y=grid[,2], col=preds, pch=20, cex=0.1)
palette(c("blue","seagreen4","orangered3"))
points(data$Sepal.Length, data$Sepal.Width, col=data$Species, pch=16, cex=1.25)

