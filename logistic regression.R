thedataset = read.csv('Social_Network_Ads.csv')
thedataset = thedataset[3:5]

splitthisshit = sample.split(thedataset$Purchased, SplitRatio = .8)
train = subset(thedataset, splitthisshit == TRUE)
testanigga = subset(thedataset, splitthisshit == FALSE)

#Feature scaling
train[,1:2] = scale(train[,1:2])
testanigga[,1:2] = scale(testanigga[,1:2])
#fit logistic regression
fixthisshitnigga = glm(formula = Purchased ~.,
                       family = binomial,
                       data = train)
#predicting the test set results
pred = predict(fixthisshitnigga, type = 'response', 
               newdata = testanigga[-3])
whatpred = ifelse(pred >.5, 1,0)

#making the confucsion matrix
# cm = table(testanigga[,3],
           # whatpred)
# install.packages('ElemStatLearn')
library(ElemStatLearn)
set = train
thegridofx1 = seq(min(set[,1])-1,max(set[,1])+1, by = .01)
thegridofx2 = seq(min(set[,2])-1,max(set[,2])+1, by = .01)
thegridnigga = expand.grid(thegridofx1,thegridofx2)
colnames(thegridnigga) = c('Age','EstimatedSalary')
probs = predict(fixthisshitnigga, type='response', newdata =thegridnigga)
whatitdohoe = ifelse(probs>.5,1,0)
plot(set[,-3],
     xlim=range(thegridofx1), ylim = range(thegridofx2))
contour(thegridofx1,thegridofx2, matrix(as.numeric(whatpred),length(thegridofx1),length(thegridofx2)), add = TRUE)
points(thegridnigga, pch = '.', col = ifelse(whatitdohoe ==1, 'springgreen3','tomato'))
points(set,pch=21,bg = ifelse(set[,3]==1,'green4','red3'))
