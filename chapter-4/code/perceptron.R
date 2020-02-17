# Rosenblatt's Perceptron Learning Algorithm
library(MASS)
library(animation)

set.seed(3)
# generate some data from two well seperated Gaussians
class_1 <- data.frame(mvrnorm(100, c(0,0), matrix(c(1,0,0,1), nrow=2)))
class_2 <- data.frame(mvrnorm(100, c(4,4), matrix(c(1,0,0,1), nrow=2)))
class_1$label <- -1
class_2$label <- 1
data <- rbind(class_1, class_2)

# plot the data and colour by if classified correctly 
plot(data[,1], data[,2], type='n', xaxt='n', yaxt='n', xlab='', ylab='', cex.lab=1.2)
title('Two Well Seperated Gaussians', line=0.5, cex.main=1.5)
title(ylab='X2', xlab='X1', line = 0.5, cex.lab = 1.4)
points(data[,1], data[,2], col=rep(c('dodgerblue4','chocolate'), each=100), pch=16, cex=2)
coord <- par('usr')
legend(x=coord[1]*0.995, y=coord[2]*1.005, legend=c('Distribution 1', 'Distribution 2'),
       col=c('dodgerblue4', 'chocolate'), pch=16, cex=1, bty='o', bg='white',
       box.col='white')

# initialise parameters
b <- c(-1,1)
b_0 <- 0

# parameter updates (eqn 4.44 on page 131)
update_params <- function(b_0, b, y_i, x_i, rho){
  b <- b - rho*y_i*x_i
  b_0 <- b_0 - rho*y_i
  return(list(b_0 = b_0, b = b))
}

plot_epoch <- function(data, correct_df, missclass_df, b_0, b, epoch){
  # plot the data and colour by if classified correctly 
  plot(data[,1], data[,2], type='n', xaxt='n', yaxt='n', xlab='', ylab='', cex.lab=1.2)
  title('Perceptron Learning Algorithm', line=0.5, cex.main=2)
  title(ylab='X2', xlab='X1', line = 0.5, cex.lab = 1.4)
  points(correct_df[,1], correct_df[,2], col='black', pch=16, cex=2)
  points(missclass_df[,1], missclass_df[,2], col='red', pch=16, cex=2)
  abline(a=-b_0/b[2], b=-b[1]/b[2], lwd=3, col='grey36')
  coord <- par('usr')  # get four corners of plot
  legend(x=coord[1]*0.995, y=coord[2]*1.005, legend=c('Missclassified', 'Correctly classified'),
         col=c("red", "black"), pch=16, cex=1.6, bty='o', bg='white',
         box.col='white')
  mtext(paste('Epoch:',epoch), side=3, line=0.3, at=coord[2]*0.95, cex=1.3, font=2)
}

# package logic together in gif format
saveGIF(
  {
    rho <- 0.01  # learning rate
    missclass_idx = 1
    epoch = 0
    while(sum(missclass_idx)>0){
      epoch = epoch + 1
      print(epoch)
      # check which rows are missclassified and subset (eqn 4.41)
      missclass_idx <- data[,3]*((data[,1] * b[1]) + (data[,2] * b[2]) + b_0) > 0
      missclass_df = data[missclass_idx,]
      correct_df = data[!missclass_idx,]
      plot_epoch(data, correct_df, missclass_df, b_0, b, epoch)
      # for each misclassified observation update parameters
      for (i in as.integer(rownames(missclass_df))) {
        x_i <- t(as.matrix(data[i,1:2]))
        y_i <- as.numeric(data[i,3])
        
        params <- update_params(b_0, b, y_i, x_i, rho)
        b_0 <- params$b_0
        b <- params$b
      }
    }
    # re plot last frame for visualisation purposes
    plot_epoch(data, correct_df, missclass_df, b_0, b, epoch)
  },
  movie.name = 'perceptron.gif', 
  interval = 0.3, 
  ani.width = 1000, 
  ani.height = 600
)




