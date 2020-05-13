# script to recreate figure 3.5 which compares the least squares RSS of every possible
# combination of variables on the prostate dataset.

# data from: https://web.stanford.edu/~hastie/ElemStatLearn/
prostate.data <- read.csv('Desktop/prostate.csv', header = T, sep = '\t', row.names = 1)

# note scaling is done before train/test split 
scaled.X <- scale(prostate.data[,1:8])
prostate.data[,1:8] <- scaled.X

train.prostate <- prostate.data[prostate.data['train'] == T, -10]
test.prostate <- prostate.data[prostate.data['train'] == F, -10]

# first we need to create all possible combinations of the 8 variables
cols <- colnames(train.prostate[,1:8])
combinations <- stringi::stri_list2matrix(
  do.call(c, lapply(seq_along(cols), combn, x = cols, simplify = FALSE)),
  byrow = TRUE
)

# now loop through each of these possible combinations and calculate RSS
results <- data.frame()
for (i in 1:nrow(combinations)){
  variables <- combinations[i,complete.cases(combinations[i,])]
  formula <- paste('lpsa ~', paste(variables, collapse = ' + '))
  results[i, 'formula'] <- formula
  results[i, 'subset.size'] <- length(variables)
  mod <- lm(formula, data = train.prostate)
  results[i, 'RSS'] <- sum(mod$residuals**2)
}
# intercept model
mod <- lm(lpsa ~ 1, data = train.prostate)
rss.intercept <- sum(mod$residuals**2)

# plot the results
par(mfrow = c(1,1))
plot(results['subset.size'][[1]], results['RSS'][[1]], ylim = c(0,100), xlim = c(0,8), 
     xaxt= 'n', pch = 16, xlab = 'Subset Size k', ylab = 'Residual Sum-of-Squares',
     col = 'dodgerblue4')
axis(1, at = 0:8, labels = 0:8)

# and the red line of minimum values
min.vals <- aggregate(RSS~subset.size, FUN=min, data= results)
min.vals <- rbind(c(0, rss.intercept), min.vals)
points(min.vals['subset.size'][[1]], min.vals['RSS'][[1]], col = 'red', pch = 16, cex=1.3)
lines(min.vals['subset.size'][[1]], min.vals['RSS'][[1]], col = 'red', type = 'b', cex=1.3)

