library(class)
library(MLmetrics)

# set up parameters for chosen figure
figure = 2.7
if(figure == 2.7){
  f_x <- function(row) exp(-8*norm(row, type="2")^2)
  f_x0 <- rep(1,100)
}
if(figure == 2.8){
  f_x <- function(row) 0.5*((row[1] + 1)^3)
  f_x0 <- rep(0.5,100)
}

# function to generate training data and labels for a given function
generate_training_data <- function(dimensions){
  dimensions = 1:dimensions
  training_data = matrix(nrow = 1000, ncol = length(dimensions))
  for (p in dimensions){
    column <- runif(n = 1000, min = -1, max = 1)
    training_data[,p] <- column
  }
  training_labels = apply(training_data, 1, f_x)
  return(list(training_data, training_labels))
}



# loop to apply knn to training data and predict x0
store_output <- list()
for(dimension in 1:10){
  store_dimension <- list()
  for(repetition in 1:100){
    output <- generate_training_data(dimension)
    training_data <- output[1][[1]]
    training_labels <- output[2][[1]]
    test_observation <- rep(0, dimension)
    prediction <- knn(training_data, test_observation, training_labels, k = 1)
    store_dimension[[repetition]] <- prediction
  }
  store_output[[dimension]] <- store_dimension
}

# calculate MSE(x0), Var & Bias for each dimension 
mse_list <- list()
variance_list <- list()
bias_list <- list()
count <- 1
for(dimension in store_output){
  f = unlist(dimension)
  predictions <- as.numeric(levels(f))[f]
  mse_list[[count]] <- MSE(predictions, f_x0)
  expected_y_hat <- mean(predictions)
  variance_list[[count]] <- MSE(predictions, rep(expected_y_hat,100))
  bias_list[[count]] <- MSE(rep(expected_y_hat,100), f_x0)
  count = count + 1
}


# plot the results
plot(unlist(mse_list), xlab = "Dimension", ylab = "MSE", col = 'red', pch = 16, main = "MSE vs. Dimension")
points(unlist(variance_list), col = 'green', pch = 16)
points(unlist(bias_list), col = 'blue', pch = 16)
lines(unlist(mse_list), col = 'red')
lines(unlist(variance_list), col = 'green')
lines(unlist(bias_list), col = 'blue')
y_legend <- max(unlist(mse_list))*0.95
legend(x = 1, y = y_legend, legend = c("MSE", "Variance", "Sq. Bias"), col = c('red', 'green', 'blue'), pch = 16)


