library(MASS)

data <- read.csv('Desktop/phoneme.csv')  # https://web.stanford.edu/~hastie/ElemStatLearn/
data <- data[,-c(1,ncol(data))]

set.seed(1)
# shuffle the data
data <- data[sample(nrow(data)),]

# split into training and testing 
train_prop = 0.8
train_idx <- 1:round(train_prop*nrow(data))
train <- data[train_idx, ]
test <- data[-train_idx, ]


# now perform cross validation on training data to choose number and position of knots  
# create 10 equally size folds
folds <- cut(seq(1,nrow(train)),breaks=10,labels=FALSE)

# knot set up # 1 - same as section 5.2.3
num_knots_1 <- 12
knots_1 <- frequencies[seq(1, length(frequencies), (length(frequencies)/num_knots_1))]

# knot set up # 2 - Same as previous with double the number of knots 
num_knots_2 <- 24
knots_2 <- frequencies[seq(1, length(frequencies), (length(frequencies)/num_knots_2))]

# knot set up # 3 - Same again trying less knots
num_knots_3 <- 8
knots_3 <- frequencies[seq(1, length(frequencies), (length(frequencies)/num_knots_3))]

# knot set up # 4 - Since the lower frequencies seemed to have the most precictive power 
# in example 5.2.3 try distributing the knots more densely in this area
num_knots_4 <- 12
dense <- frequencies[seq(1, 100, (100/8))]
sparse <- frequencies[seq(100, length(frequencies), ((length(frequencies) - 100)/4))]
knots_4 <- c(dense, sparse)

# knot set up # 5 - Frequency based knots
df = 12

# perform 10 fold cross validation
qda_error <- c()
error_1 <- c()
error_2 <- c()
error_3 <- c()
error_4 <- c()
error_5 <- c()
for(i in 1:10){
  #Segement data by fold 
  cv_test_idx <- which(folds==i, arr.ind=TRUE)
  cv_test <- train[cv_test_idx, ]
  cv_train <- train[-cv_test_idx, ]
  
  # qda on raw data 
  qda_model <- qda(g ~ ., data = cv_train)
  qda_preds <- predict(qda_model, cv_test[,-ncol(cv_test)])$class
  qda_error[[i]] <- 1 - sum(qda_preds == cv_test$g)/nrow(cv_test)
  
  # set up # 1 
  H <- ns(frequencies, knots = knots_1[2:(num_knots_1-1)], intercept = TRUE)
  Xstar = as.data.frame(as.matrix(cv_train[,-257])%*%as.matrix(H))
  Xstar$y <- cv_train$g
  smooth_mod_1 <- qda(y ~ 0 + ., data = Xstar)

  Xstar_test <- as.matrix(cv_test[,-257])%*%as.matrix(H)
  smooth_preds <- predict(smooth_mod_1, newdata = as.data.frame(Xstar_test))$class
  error_1[[i]] <- 1 - sum(smooth_preds == cv_test$g)/nrow(cv_test)
  
  # set up # 2
  H <- ns(frequencies, knots = knots_2[2:(num_knots_2-1)], intercept = TRUE)
  Xstar = as.data.frame(as.matrix(cv_train[,-257])%*%as.matrix(H))
  Xstar$y <- cv_train$g
  smooth_mod_2 <- qda(y ~ 0 + ., data = Xstar)
  
  Xstar_test <- as.matrix(cv_test[,-257])%*%as.matrix(H)
  smooth_preds <- predict(smooth_mod_2, newdata = as.data.frame(Xstar_test))$class
  error_2[[i]] <- 1 - sum(smooth_preds == cv_test$g)/nrow(cv_test)
  
  # set up # 3
  H <- ns(frequencies, knots = knots_3[2:(num_knots_3-1)], intercept = TRUE)
  Xstar = as.data.frame(as.matrix(cv_train[,-257])%*%as.matrix(H))
  Xstar$y <- cv_train$g
  smooth_mod_3 <- qda(y ~ 0 + ., data = Xstar)
  
  Xstar_test <- as.matrix(cv_test[,-257])%*%as.matrix(H)
  smooth_preds <- predict(smooth_mod_3, newdata = as.data.frame(Xstar_test))$class
  error_3[[i]] <- 1 - sum(smooth_preds == cv_test$g)/nrow(cv_test)
  
  # set up # 4
  H <- ns(frequencies, knots = knots_4[2:(num_knots_4-1)], intercept = TRUE)
  Xstar = as.data.frame(as.matrix(cv_train[,-257])%*%as.matrix(H))
  Xstar$y <- cv_train$g
  smooth_mod_4 <- qda(y ~ 0 + ., data = Xstar)
  
  Xstar_test <- as.matrix(cv_test[,-257])%*%as.matrix(H)
  smooth_preds <- predict(smooth_mod_4, newdata = as.data.frame(Xstar_test))$class
  error_4[[i]] <- 1 - sum(smooth_preds == cv_test$g)/nrow(cv_test)
  
  # set up # 5
  H <- ns(frequencies, df=df, intercept = TRUE)
  Xstar = as.data.frame(as.matrix(cv_train[,-257])%*%as.matrix(H))
  Xstar$y <- cv_train$g
  smooth_mod_5 <- qda(y ~ 0 + ., data = Xstar)
  
  Xstar_test <- as.matrix(cv_test[,-257])%*%as.matrix(H)
  smooth_preds <- predict(smooth_mod_5, newdata = as.data.frame(Xstar_test))$class
  error_5[[i]] <- 1 - sum(smooth_preds == cv_test$g)/nrow(cv_test)
}

# check median of results across 12 folds 
paste('Set up #1 error:', median(error_1))
paste('Set up #2 error:', median(error_2))
paste('Set up #3 error:', median(error_3))
paste('Set up #4 error:', median(error_4))
paste('Set up #5 error:', median(error_5))

# the differences are marginal but it looks like set up #4 has the lowest error rate
# lets test this model on the held out test set 
H <- ns(frequencies, knots = knots_4[2:(num_knots_4-1)], intercept = TRUE)
Xstar_test <- as.matrix(test[,-257])%*%as.matrix(H)
smooth_preds <- predict(smooth_mod_4, newdata = as.data.frame(Xstar_test))$class
error_rate <- 1 - sum(smooth_preds == test$g)/nrow(test)
paste('Set up #4 error on test set:', error_rate)




