#K-NEarest Neigher
ourdata = read.csv('Social_Network_Ads.csv')
ourdata = ourdata[3:5]
library(caTools)
splitthisshit = sample.split(ourdata$Purchased, SplitRatio = .8)
trains = subset(ourdata, splitthisshit == TRUE)
testanigga = subset(ourdata, splitthisshit == FALSE)

#Feature scaling
trains[,1:2] = scale(trains[,1:2])
testanigga[,1:2] = scale(testanigga[,1:2])
#the fitting K-NN ot the train set and predict the test set
#install.packages('class')
library(class)
pred = knn(train = trains[,-3],
           test= testanigga[,-3],
           cl = trains[,3],
           k = 5)

#making the confucsion matrix
cm = table(testanigga[,3],
pred)
 #install.packages('ElemStatLearn')
library(ElemStatLearn)
set = trains
thegridofx1 = seq(min(set[,1])-1,max(set[,1])+1, by = .01)
thegridofx2 = seq(min(set[,2])-1,max(set[,2])+1, by = .01)
thegridnigga = expand.grid(thegridofx1,thegridofx2)
colnames(thegridnigga) = c('Age','EstimatedSalary')
probs = knn(train = trains[,-3],
            test= thegridnigga,
            cl = trains[,3],
            k = 5)
whatitdohoe = ifelse(probs>.5,1,0)
plot(set[,-3],
     xlim=range(thegridofx1), ylim = range(thegridofx2))
contour(thegridofx1,thegridofx2, matrix(as.numeric(probs),length(thegridofx1),length(thegridofx2)), add = TRUE)
points(thegridnigga, pch = '.', col = ifelse(probs ==1, 'springgreen3','tomato'))
points(set,pch=21,bg = ifelse(set[,3]==1,'green4','red3'))
