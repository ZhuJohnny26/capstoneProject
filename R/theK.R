#intialization
#first you need to import data but setwd first 
#bCancer <- read.table("/insertyourDatapath/wdbc.txt" , header=false, sep = ",")
#delete first 2 columns
#bCancer[1:2] <- NULL
#create test set called testMean
#testMean <- bCancer [1:569,1:5]
#then source and run the script
#k_pts has the table of mean points
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

cluster <- rep(list(NULL), k)#creates a list of k clusters


get_dist <- rep(0,3)
for(i in 1:nrow(testMean)){ #goes through the data
  minimum <- 10000
  
 
  for(j in 1:nrow(k_pts)){ #goes through the k means
    
  need_dist <- testMean[i, 1:5]
  mean_pt <- k_pts[j, 1:5]
  get_dist <- euclidean_dist(mean_pt, need_dist)
  dist_1 <- as.numeric(as.character(get_dist))
  minimum <- as.numeric(minimum)
  
  if(dist_1 < minimum){#compares the distance between every k, the min is then put into minimum and is indexed
    index <- j
    minimum <- dist_1
    }
  }

  enter <- testMean[i, 1:5]#places the row of data into the k cluster with min distance
  cluster[[index]] <- rbind(cluster[[index]], enter)
  
}

