######### K-Means clustering program #########
# Yicheng Wang
# This program takes matrix x as data, 
# and divide data points into 2 clusters based on K-means clustering

rm(list=ls())
set.seed(123467)

#### Simulate data--x ####
x = matrix(rnorm(100*2),ncol=2)
x[1:50,1] = x[1:50,1] + 5 
x[1:50,2] = x[1:50,2] - 3


#### Your own K-Means program ####

# plot data points in x
plot(x, xlab="V1", ylab="V2", main="Plot of Data points")

# randomly assign data points to clusters 1 & 2
rand <- runif(length(x[,1]), min = 0, max = 1) 
train <- rep(FALSE, length(x[,1]))
train[rand<=0.5] = TRUE 
c1 <- x[train,]
c2 <- x[!train,]

# iterate 1000 times until points in clusters don't change
for (i in 1:1000) {
  
  # centroid of cluster 1 & 2
  mean1 = c(mean(c1[,1]), mean(c1[,2]))
  mean1
  mean2 = c(mean(c2[,1]), mean(c2[,2]))
  mean2
  
  # reassign cluster according to which centroid in obs is closest
  # reassign data points in cluster 1
  for (i in 1:length(c1[,1])) {
    dist11 = norm( as.matrix(c1[i,]- mean1), type="2" )
    dist12 = norm( as.matrix(c1[i,]- mean2), type="2" )
    if (dist11 > dist12) {
      c2 = rbind(c2, c1[i,])
      c1[i,] = c(0,0)
    }
  }
  c1 <- c1[ which(c1[,1] != 0), ]
  
  # reassign data points in cluster 2
  for (i in 1:length(c2[,1])) {
    dist21 = norm( as.matrix(c2[i,]- mean1), type="2" )
    dist22 = norm( as.matrix(c2[i,]- mean2), type="2" )
    if (dist21 < dist22) {
      c1 = rbind(c1, c2[i,])
      c2[i,] = c(0,0)
    }
  }
  c2 <- c2[ which(c2[,1] != 0), ]

}

# let cluster.results be a vector of dummy values
# if the data point is in cluster 1, the value is 1. if it is in cluster 2, the value is 2.
cluster.results <- rep(1, length(x[,1]))
for(i in 1:100) {
  for(j in 1:50) {
    if(x[i,]==c2[j,]){
      cluster.results[i]=2
    }
  }
}
cluster.results


#### Plot results ####
# Let cluster.results be a vector that contains results from your K-Means program with K=2. 

plot(x, col=(cluster.results+1), xlab="V1", ylab="V2", 
     main="My result with 2 clusters")

# compare my result to result from kmeans function built in r
par(mfrow=c(1,2))
plot(x, col=(cluster.results+1), xlab="V1", ylab="V2", 
     main="My result")
km1 = kmeans(x, 2, nstart=100)
km1$cluster
plot(x, col =(km1$cluster +1), xlab="V1", ylab="V2", 
     main="K-Means result with 2 clusters")
par(mfrow=c(1,1))
