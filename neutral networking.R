#Artificial Neutral network

ourdata = read.csv('Churn_Modelling.csv')
ourdata = ourdata[4:14]

#categorical files to numeric

ourdata$Geography<- as.numeric(factor(ourdata$Geography,
                                      levels = c('France','Spain','Germany'),
                                      labels = c(1,2,3)))
ourdata$Gender<- as.numeric(factor(ourdata$Gender,
                                      levels = c('Male','Female'),
                                      labels = c(1,2)))


library(caTools)
splitthisshit = sample.split(ourdata$Exited, SplitRatio = .8)
trains = subset(ourdata, splitthisshit == TRUE)
testanigga = subset(ourdata, splitthisshit == FALSE)
#Feature scaling
trains[-11] = scale(trains[-11])
testanigga[-11] = scale(testanigga[-11])

#training with ANN
install.packages('h2o')
library(h2o)
h2o.init(nthreads = -1)
class = h2o.deeplearning(y = 'Exited',
                         training_frame = as.h2o(trains),
                         activation = 'Rectifier',
                         hidden = c(6,6),
                         epochs = 100,
                         train_samples_per_iteration = -2)
#predict the test results
pred = h2o.predict(class, newdata = as.h2o(testanigga[-11]))
prob = (pred >.5)
prob = as.vector(prob)


#making the confucsion matrix
cm = table(testanigga[,11],
           prob)
h2o.shutdown()
y