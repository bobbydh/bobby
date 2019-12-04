#PCA
library(tidyverse)
library(caTools)
ourdata = read_csv("Wine.csv")
ourdata$Customer_Segment= factor(x = ourdata$Customer_Segment,
                                 levels= c(1,2,3))

#split the data into test and trainset
splitthedata = sample.split(ourdata$Customer_Segment, SplitRatio = .8)
trainset = subset(ourdata, splitthedata==TRUE)
testset = subset(ourdata,splitthedata == FALSE)

#feature scaling
trainset[,1:13] = scale(trainset[,1:13])
testset[,1:13] = scale(testset[,1:13])



#PCA
# install.packages("caret")
library(caret)
library(e1071)
pca = preProcess(x = trainset[-14],method = 'pca',pcaComp = 2)
trainset =predict(pca,trainset)
trainset = trainset[c('PC1','PC2','Customer_Segment')]
testset =predict(pca,testset)
testset = testset[c('PC1','PC2','Customer_Segment')]

#tree classification
library(rpart)
cL = rpart(formula= Customer_Segment~.,
           data = trainset)
predict(cL,trainset[-3],type='class')
theprediction = predict(cL, testset[-3],type='class')
table(theprediction,testset[,3])

set = trainset
thegridofx1 = seq(min(set[,1])-1,max(set[,1])+1, by = .01)
thegridofx2 = seq(min(set[,2])-1,max(set[,2])+1, by = .01)
thegridnigga = expand.grid(thegridofx1,thegridofx2)
colnames(thegridnigga) = c('PC1','PC2')

thegridofpredictions = predict(cL, thegridnigga,type='class')

plot(set[,-3],
     xlim=range(thegridofx1), ylim = range(thegridofx2))
contour(thegridofx1,thegridofx2, matrix(as.numeric(thegridofpredictions),length(thegridofx1),length(thegridofx2)), add = TRUE)
points(thegridnigga, pch = '.', col = sapply(thegridofpredictions, findingcolors))
points(set,pch=21,bg = sapply(set[,3],findingcolors) )



# Create a list of matrices
MyList <- set[,3]


# Extract the 1st row from `MyList`
lapply(set[,3], findingcolors)
findingcolors<- function(x){
  for(i in 1:length(x)){
  if(x[i] ==3){
    return('red')
  }
  else if(x[i] ==2){
    return('blue')
  }
  else{
    return('green')
  }
  }
}

