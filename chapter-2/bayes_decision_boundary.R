library(MASS)

# generate the 20 bivariate means
N = 10
mu1 = c(0,1)  # orange
mu2 = c(1,0)  # blue
sigma = matrix(c(1, 0, 0, 1),2)
bvn1 <- data.frame(mvrnorm(N, mu = mu1, Sigma = sigma ))
bvn2 <- data.frame(mvrnorm(N, mu = mu2, Sigma = sigma )) 
plot(c(bvn1[,1],bvn2[,1]), c(bvn1[,2],bvn2[,2]), xlab = 'X', ylab = "Y", main="20 Bivariate Means", type = "n")
points(bvn1[,1],bvn1[,2], col='orange', pch=16, cex=2)
points(bvn2[,1],bvn2[,2], col='blue', pch=16, cex=2)

# draw 100 observations from each class
store_observations = data.frame()
for( bivariate_means in list(bvn1,bvn2)) {
  for(observation in 1:100){
    sample_index = sample(nrow(bivariate_means), 1)
    random_draw = bivariate_means[sample_index,]
    observation_i <- mvrnorm(1, mu = t(random_draw), Sigma = sigma/5 )
    store_observations = rbind(observation_i, store_observations)
  }
}


# plot the resulting data
plot(store_observations[,1], store_observations[,2], xlab = 'X', ylab = "Y", main="200 observations from mixed gaussian clusters", type = "n")
points(store_observations[1:100,1],store_observations[1:100,2], col='blue', pch="o", cex=1.5)
points(store_observations[101:200,1],store_observations[101:200,2], col='orange', pch="o", cex=1.5)

# function to find bayes decision boundry
BayesBoundary <- function(x){
  sumM <- 0; sumN <- 0;  
  for(i in 1:10){ sumM <- sumM + exp(-5/2*(x-as.matrix(bvn1[i,]))%*%t(x-as.matrix(bvn1[i,])))}
  for(i in 1:10){ sumN <- sumN + exp(-5/2*(x-as.matrix(bvn2[i,]))%*%t(x-as.matrix(bvn2[i,])))}
  return(sumM-sumN)
}

# plot the bayes decison boundry 
gridX <- seq(-3,3,length=100)
gridY <- seq(-3,3,length=100)
levels <-outer(gridX,gridY,function(x,y)apply(cbind(x,y),1,BayesBoundary))
contour(gridX,gridY,levels,levels=0,add=TRUE,drawlabels=FALSE)
