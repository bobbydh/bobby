#K-means Clustering
ourdata <- read.csv('Mall_Customers.csv')
x <- ourdata[4:5]

#using the elbow method to find the optimal number of clusters
set.seed(6)
sumofsqures <- vector()
for(i in 1:10) sumofsqures[i] <- sum(kmeans(x,i)$withinss)
plot(1:10, sumofsqures, type = "b",main = paste('cluster'),xlab ='23',ylab='23')

#applying k-means to the mall
kmeansalgorithm = kmeans(x,5,iter.max = 300,nstart = 10)
#visulisaing the results
library(cluster)
clusplot(x,
         kmeansalgorithm$cluster,
         lines =0,
         shade= TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE)
