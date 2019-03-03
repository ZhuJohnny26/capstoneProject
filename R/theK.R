#intialization
#first you need to import data but setwd first 
#bCancer <- read.table("/insertyourDatapath/wdbc.txt" , header=false, sep = ",")
#delete first 2 columns
#bCancer[1:2] <- NULL
#create test set called testMean
#testMean <- bCancer [1:569,1:5]
#then source and run the script
#k_pts has the table of mean points

myMeans <- colMeans(testMean)
colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)

myMax <-colMax(testMean)
myMin <-colMin(testMean)

pointMean <- function(data, means, column) {
  mean_pts <- rep(0,3)
  noMean <- means[column]
  subData <- data[1:569, column]
  
  minData <-subData[subData<noMean]
  minMean <- mean(minData)
  minData_1 <- minData[minData < minMean]
  mean_pts[1] <- mean(minData_1)
  
  maxData <- subData[subData >noMean]
  meanData_1 <- mean(maxData)
  maxData_1 <- maxData[maxData< meanData_1]
  mean_pts[2] <- mean(maxData_1)
  maxData_2 <- maxData[maxData >meanData_1]
  mean_pts[3] <- mean (maxData_2)

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
  get_dist[1]<- euclidean_dist(mean_pt1, need_dist)
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
