#SUPPORT VECtor MACHINE
ourdata = read.csv('Social_Network_Ads.csv')
ourdata = ourdata[3:5]

splitthisshit = sample.split(ourdata$Purchased, SplitRatio = .8)
trains = subset(ourdata, splitthisshit == TRUE)
testanigga = subset(ourdata, splitthisshit == FALSE)

#Feature scaling
trains[,1:2] = scale(trains[,1:2])
testanigga[,1:2] = scale(testanigga[,1:2])

classify =svm(formula = Purchased ~.,
              data = trains,
              type = 'C-classification',
              kernel ='linear')


#making the confucsion matrix
theprediction = predict(classify, newdata = testanigga[,-3])

confusion = table(testanigga[,3], theprediction)
#plot
set = trains
thegridofx1 = seq(min(set[,1])-1,max(set[,1])+1, by = .01)
thegridofx2 = seq(min(set[,2])-1,max(set[,2])+1, by = .01)
thegridnigga = expand.grid(thegridofx1,thegridofx2)
colnames(thegridnigga) = c('Age','EstimatedSalary')
#probs = knn(train = trains[,-3],
#            test= thegridnigga,
 #           cl = trains[,3],
  #          k = 5)
thegridofpredictions = predict(classify, thegridnigga)
whatitdohoe = ifelse(probs>.5,1,0)
plot(set[,-3],
     xlim=range(thegridofx1), ylim = range(thegridofx2))
contour(thegridofx1,thegridofx2, matrix(as.numeric(thegridofpredictions),length(thegridofx1),length(thegridofx2)), add = TRUE)
points(thegridnigga, pch = '.', col = ifelse(thegridofpredictions ==1, 'springgreen3','tomato'))
points(set,pch=21,bg = ifelse(set[,3]==1,'green4','red3'))
