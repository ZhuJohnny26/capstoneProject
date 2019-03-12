bCancer <- read.table("D:\\New folder\\wdbc.txt" , header=FALSE, sep = ",")
bCancer[1:2] <- NULL
testMean <- bCancer [1:569,1:5]

myMeans <- colMeans(testMean)
colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)

myMax <-colMax(testMean)
myMin <-colMin(testMean)

k <- 3 #set k
n <- 1000 #set number of iterations for recursion
g <- 1 #convergence limit

pointMean <- function(data, means, column) {
  mean_pts <- rep(0,3)
  noMean <- means[column] #first mean
  subData <- data[1:569, column]
  minData <- subData[subData < noMean] #data between min and first mean
  maxData <- subData[subData > noMean] #data between first mean and max
  mean_pts[1] <- noMean
  
  #at 1 k is placed in 2, 3
  #at 2 k is placed in 4, 5 and so on
  for(i in 1:floor(k/2)){ 
    temp1 <- mean(minData)
    temp2 <- mean(maxData)
    mean_pts[i+i] <- temp1 #adds the means 
    mean_pts[i+i+1]<- temp2
    minData <- minData[minData < temp1]
    maxData <- maxData[temp2 < maxData]
    
  }
  if(k %% 2 == 0){  #if k is even gets rid of the first
                    #mean to have even k means
    mean_pts <- mean_pts[2:floor(k/2)] 
 
  }
  return(mean_pts)
}

col1_mean <- pointMean(testMean, myMeans, 1)
col2_mean <- pointMean(testMean, myMeans, 2)
col3_mean <- pointMean(testMean, myMeans, 3)
col4_mean <- pointMean(testMean, myMeans, 4)
col5_mean <- pointMean(testMean, myMeans, 5)

k_pts <- data.frame(col1_mean, col2_mean, col3_mean, col4_mean, col5_mean)

#euclidean distance 
euclidean_dist <- function(k,unk) {
  distance <- 0
  distance <- sqrt((unk[1] - k[1])^2 + (unk[2] - k[2])^2 + (unk[3] - k[3])^2 + (unk[4] - k[4])^2
                   + (unk[5] - k[5])^2)
  
  return(distance)
} 

clust_1 <- data.frame()
clust_2 <- data.frame()
clust_3 <- data.frame()


get_dist <- rep(0,3)
for(i in 1:nrow(testMean)){
  need_dist <- testMean[i, 1:5]
  mean_pt1 <- k_pts[1, 1:5]
  mean_pt2 <- k_pts[2, 1:5]
  mean_pt3 <- k_pts[3, 1:5]
  get_dist[1] <- euclidean_dist(mean_pt1, need_dist)
  get_dist[2] <- euclidean_dist(mean_pt2, need_dist)
  get_dist[3] <- euclidean_dist(mean_pt3, need_dist)
  dist_1 <- as.numeric(as.character(get_dist[1]))
  dist_2 <- as.numeric(as.character(get_dist[2]))
  dist_3 <- as.numeric(as.character(get_dist[3]))
  theMin <- min(unlist(get_dist))
  if(isTRUE(all.equal(theMin, dist_1))){
    enter1 <- testMean[i, 1:5]
    clust_1 <- rbind(clust_1, enter1)
  } else if(isTRUE(all.equal(theMin, dist_2))){
    
    enter2 <- testMean[i, 1:5]
    clust_2 <- rbind(clust_2, enter2)
  } else if(isTRUE(all.equal(theMin, dist_3))){
    enter3 <- testMean[i, 1:5]
    clust_3 <- rbind(clust_3, enter3)
  }
  
}

