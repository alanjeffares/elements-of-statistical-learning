# This script performs local linear discriminant analysis on the zipcode data for exercise 6.12.
# This implementation is not efficient and took 24 hrs to run on a raspberry pi 4. 

# Note the functions wlda.default and predict.wlda come from the library "locClass", 
# I had issues installing this library so I had to copy the code directly instead.
# Source: https://rdrr.io/rforge/locClass/src/R/wlda.R

# HELPER FUNCTIONS
# epanechnikov quadratic kernel with l2 dist
eqk <- function(x, x0, lambda){
  t <- dist(rbind(x, x0))/lambda
  if( t <= 1){
    return(0.75*(1 - t**2))
  }
  else return(0)
}


# local lda for a test point x0 and training set 
llda.evaluate <- function(train.data, x0, lambda){
  weights <- c()
  # get weights using epanechnikov quadratic kernel with l2 dist from x0
  for (i in 1:nrow(train.data)){
    weights[i] <- eqk(train.data[i, -1], x0, lambda)
  }
  # fit lda using these weights to training set
  mod <- wlda.default(x = train.data[,-1], grouping = train.data[,1], 
                      weights = weights)
  # return prediction for test observation x0
  return(predict.wlda(mod, x0)$class[[1]])
}


# apply local lda for each point in the test set and store predictions
# WARNING: this can take some time
llda <- function(train.data, test.data, lambda){
  preds <- c()
  num.obs <-  nrow(test.data)
  ts <- Sys.time()  # record start time
  for (i in 1:num.obs){
    print(paste0('Working on observaion: ', i, '/', num.obs))
    preds[i] <- llda.evaluate(train.data, test.data[i,], lambda)
    if (i %% 1 == 0){
      tc <- Sys.time()
      batch.time <- difftime(tc, ts, units="hours")
      est.time <- (num.obs - i)*(batch.time / i)  # rem obs * time per obs
      print(paste0('Estimated remaining time: ', est.time[[1]], ' hrs'))
    }
  }
  return(preds)
}


# Note the functions wlda.default and predict.wlda come from the library "locClass", 
# I had issues installing this library so I had to copy the code directly instead.
# Source: https://rdrr.io/rforge/locClass/src/R/wlda.R
wlda.default <- function(x, grouping, weights = rep(1, nrow(x)), method = c("unbiased", "ML"), ...) {
  if (is.null(dim(x))) 
    stop("'x' is not a matrix")
  x <- as.matrix(x)
  if (any(!is.finite(x))) 
    stop("infinite, NA or NaN values in 'x'")
  n <- nrow(x)
  if (n != length(weights))
    stop("nrow(x) and length(weights) are different")
  
  
  if (any(weights < 0))
    stop("weights have to be larger or equal to zero")
  if (all(weights == 0))
    stop("all weights are zero")
  names(weights) <- rownames(x)
  
  if (n != length(grouping)) 
    stop("'nrow(x)' and 'length(grouping)' are different")
  
  
  # remove all observations with weight 0
  x <- x[weights > 0, , drop = FALSE]
  grouping <- grouping[weights > 0]
  w <- weights[weights > 0]
  n <- nrow(x)
  if (!is.factor(grouping))
    warning("'grouping' was coerced to a factor")
  g <- as.factor(grouping)
  lev <- lev1 <- levels(g)
  counts <- as.vector(table(g))
  if (any(counts == 0)) {
    empty <- lev[counts == 0]
    warning(sprintf(ngettext(length(empty), "group %s is empty or weights in this group are all zero", 
                             "groups %s are empty or weights in these groups are all zero"), paste(empty, collapse = ", ")), 
            domain = NA)
    lev1 <- lev[counts > 0]
    g <- factor(g, levels = lev1)
    counts <- as.vector(table(g))
  }
  if (length(lev1) == 1L)
    stop("training data from only one group given")
  method <- match.arg(method)
  
  
  class.weights <- tapply(w, g, sum)
  prior <- class.weights/sum(w)
  ng <- length(prior)
  names(counts) <- lev1
  xwt <- w * x
  center <- t(matrix(sapply(lev1, function(z) colSums(xwt[g == z, , drop = FALSE])), ncol = ng, dimnames = list(colnames(x), lev1)))/as.numeric(class.weights)    
  z <- x - center[g, , drop = FALSE]
  cov <- crossprod(w*z, z)/sum(w)	#ML estimate
  if (method == "unbiased") {
    norm.weights <- w/class.weights[g]
    cov <- cov/(1 - sum(prior * tapply(norm.weights^2, g, sum)))
  }
  cl <- match.call()
  cl[[1L]] <- as.name("wlda")
  return(structure(list(prior = prior, counts = counts, means = center, 
                        cov = cov, lev = lev, N = n, weights = weights, method = method, call = cl), class = "wlda"))
}


predict.wlda <- function(object, newdata, prior = object$prior, ...) {
  if (!inherits(object, "wlda"))
    stop("object not of class \"wlda\"")
  if (!is.null(Terms <- object$terms)) {
    Terms <- delete.response(Terms)
    if (missing(newdata))
      newdata <- model.frame(object)
    else {
      newdata <- model.frame(Terms, newdata, na.action = na.pass,
                             xlev = object$xlevels)
      if (!is.null(cl <- attr(Terms, "dataClasses")))
        .checkMFClasses(cl, newdata)
    }
    x <- model.matrix(Terms, newdata, contrasts = object$contrasts)
    xint <- match("(Intercept)", colnames(x), nomatch = 0L)
    if (xint > 0)
      x <- x[, -xint, drop = FALSE]
  }
  else {
    if (missing(newdata)) {
      if (!is.null(sub <- object$call$subset))
        newdata <- eval.parent(parse(text = paste(deparse(object$call$x,
                                                          backtick = TRUE), "[", deparse(sub, backtick = TRUE),
                                                  ",]")))
      else newdata <- eval.parent(object$call$x)
      if (!is.null(nas <- object$call$na.action))
        newdata <- eval(call(nas, newdata))
    }
    if (is.null(dim(newdata)))
      dim(newdata) <- c(1, length(newdata))
    x <- as.matrix(newdata)
  }
  if (ncol(x) != ncol(object$means))
    stop("wrong number of variables")
  if (length(colnames(x)) > 0L && any(colnames(x) != dimnames(object$means)[[2L]]))
    warning("variable names in 'newdata' do not match those in 'object'")
  ng <- length(object$prior)
  if (!missing(prior)) {
    if (any(prior < 0) || round(sum(prior), 5) != 1)
      stop("invalid prior")
    if (length(prior) != ng)
      stop("'prior' is of incorrect length")
  }
  lev1 <- names(object$prior)
  posterior <- matrix(0, ncol = ng, nrow = nrow(x), dimnames = list(rownames(x), lev1))
  posterior[,lev1] <- sapply(lev1, function(z) log(prior[z]) - 0.5 * mahalanobis(x, center = object$means[z,], cov = object$cov))
  gr <- factor(lev1[max.col(posterior)], levels = object$lev)
  names(gr) <- rownames(x)
  posterior <- exp(posterior - apply(posterior, 1L, max, na.rm = TRUE))
  posterior <- posterior/rowSums(posterior)
  if(any(is.infinite(posterior))) 
    warning("infinite, NA or NaN values in 'posterior'")
  return(list(class = gr, posterior = posterior))
}



# MAIN LOGIC
# predicting the full test data will take ~24 hrs on my machine (due to memory based nature of local regression)
# to reduce run time I will test accuracy on the first 100 observations for each of the lambda values.

# train on full dataset, test on just the first 100 observations to reduce runtime
zip.test <- read.csv('zip.test', sep = '', header = F)
zip.train<- read.csv('zip.train', sep = '', header = F)

# lambda = 18
preds <- llda(zip.train, zip.test[1:100,-1], 18)
acc18 <- sum(preds == zip.test[1:100,1] + 1)/100

# lambda = 20 
preds <- llda(zip.train, zip.test[1:100,-1], 20)
acc20 <- sum(preds == zip.test[1:100,1] + 1)/100

# lambda = 25
preds <- llda(zip.train, zip.test[1:100,-1], 25)
acc25 <- sum(preds == zip.test[1:100,1] + 1)/100

# lambda = 30
preds <- llda(zip.train, zip.test[1:100,-1], 30)
acc30 <- sum(preds == zip.test[1:100,1] + 1)/100

# lambda = 50
preds <- llda(zip.train, zip.test[1:100,-1], 50)
acc50 <- sum(preds == zip.test[1:100,1] + 1)/100

# save results in dataframe
results <- data.frame(c('LLDA -> λ = 18', 'LLDA -> λ = 20', 'LLDA -> λ = 25', 'LLDA -> λ = 30', 'LLDA -> λ = 50'),
                      c(acc18, acc20, acc25, acc30, acc50), stringsAsFactors = FALSE)
colnames(results) = c('Model', 'Accuracy')


# now train & test a regular lda model for comparison 
# note wlda with all weights = 1 is just regular lda
mod <- wlda.default(x = zip.train[,-1], grouping = zip.train[,1], 
                    weights = rep(1, nrow(zip.train)))
preds = predict.wlda(mod, zip.test[1:100,-1])$class
accuracy = sum(zip.test[1:100,1] == preds)/length(preds)
results[nrow(results) + 1,] = list('LDA', accuracy)

# we can additionally confirm that this lda implementation matches the 
# predictions from the MASS implementation
library(MASS)
mod <- lda(x = zip.train[,-1], grouping = zip.train[,1])
preds = predict(mod, zip.test[1:100,-1])$class
accuracy == sum(zip.test[1:100,1] == preds)/length(preds)  # TRUE


write.csv(results, file = 'results.csv')
