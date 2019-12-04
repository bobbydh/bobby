#Hierarchical Clustering

#importing the mall dataset
ourdata = read.csv('Mall_Customers.csv')
x = ourdata[4:5]

#using the dendogram to find the optimal number of clusters
dendogram = hclust(dist(x,method = 'euclidean'), method = 'ward.D')
plot(dendogram)

# fitting hierarchical clustering to the mall dataset
thisnigga = hclust(dist(x,method = 'euclidean'), method = 'ward.D')
thisniggacut = cutree(thisnigga,5)

clusplot(x,
         thisniggacut,
         lines=0,
         labels = 2)
