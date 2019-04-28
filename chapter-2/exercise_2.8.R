train <- read.csv("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/waveform.train", sep='\t', header=TRUE)
test <- read.csv("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/waveform.test", sep='\t', header=TRUE)

train <- train[(train$y==2) | (train$y==3),]
test <- test[(test$y==2) | (test$y==3),]

train <- train[,-1]
test <- test[,-1]

# begin with linear regression (not even logistic)
mod <- glm(y ~ . , data = train)
summary(mod)

round_to_class_values <- function(value){
  if (value < 2.5) return(2)
  else (return(3))
}

pred_train <- predict(mod)
pred_test <- predict(mod, test[,-1])

pred_train_rounded <- sapply(pred_train, round_to_class_values)
pred_test_rounded <- sapply(pred_test, round_to_class_values)

table(pred_train_rounded, train$y)
table(pred_test_rounded, test$y)

mse_train_glm <- mean((train$y - pred_train)^2)
mse_test_glm <- mean((test$y - pred_test)^2)

# now try knn 
library(class)

K <- c(1,3,5,7,15)
store_train_mse <- list()
store_test_mse <- list()

for (k in K){
  train_pred_knn <- knn(train[,-1], train[,-1], train$y, k = k)
  test_pred_knn <- knn(train[,-1], test[,-1], train$y, k = k)
  
  train_pred_knn <- as.numeric(levels(train_pred_knn))[train_pred_knn]
  test_pred_knn <- as.numeric(levels(test_pred_knn))[test_pred_knn]
  
  mse_train_knn <- mean((train$y - train_pred_knn)^2)
  mse_test_knn <- mean((test$y - test_pred_knn)^2)
  
  store_train_mse <- append(store_train_mse, mse_train_knn)
  store_test_mse <- append(store_test_mse, mse_test_knn)
}


# plot results
plot(y = unlist(store_train_mse), x = K, type='b', ylim = c(0,max(unlist(store_test_mse))), 
     col = 'red', pch = 19, xaxt = 'n', ylab = 'MSE', main = 'Training vs Test Error')
axis(side=1,at=K)
points(y = unlist(store_test_mse), x = K, type='b', col = 'blue', pch = 19)
abline(h = mse_test_glm, col = 'green')
abline(h = mse_train_glm, col = 'purple')
legend(x = 11.5, y = 0.16, c("KNN train MSE", "KNN test MSE", "GLM train MSE", 'GLM test MSE'), 
       col = c('red', 'blue', 'purple', 'green'), 
       pch = rep(19, 4) )





