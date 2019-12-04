#multipple linear regression
Ourdata = read.csv('50_Startups.csv')

Ourdata$State = factor(Ourdata$State,
                       levels = c('New York', 'California','Florida'),
                       labels = c(0,1,2))

splitdata = sample.split(Ourdata$Profit, SplitRatio = .75)
thetrainingset = subset(Ourdata, splitdata == TRUE)
thetestset = subset(Ourdata, splitdata == FALSE)
#fit the multiple linear regession to the training set
regress =lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
            data = Ourdata)
summary(regress)
regress =lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend ,
            data = Ourdata)
summary(regress)
regress =lm(formula = Profit ~ R.D.Spend  + Marketing.Spend,
            data = Ourdata)
summary(regress)
#predict our test set
theprediction = predict(regress, newdata = thetestset)
