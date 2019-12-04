#Polynomial Regression
Ourdata = read.csv('Position_Salaries.csv')
Ourdata = Ourdata[2:3]
# our data set is too small to split
# library(caTools)
# Oursplit = sample.split(Ourdata$Salary, SplitRatio = .8)
# traning = subset(Ourdata, Oursplit == TRUE )
# test = subset(Ourdata, Oursplit == FALSE)
#linear
linearRegression = lm(formula = Salary ~ Level, data = Ourdata)
#polynomial
Ourdata$Level2 = Ourdata$Level^2
Ourdata$Level3 = Ourdata$Level^3
Ourdata$Level4 = Ourdata$Level^4
Ourdata$Level5 = Ourdata$Level^5
Polynomial_Regression  = lm(formula = Salary ~., 
                            data = Ourdata)
#plot linear
ggplot()+
  geom_point(aes(x =Ourdata$Level,y=Ourdata$Salary), colour = 'red') +
  geom_line(aes(x = Ourdata$Level, y = predict(linearRegression, newdata = Ourdata)),colour = 'blue')+
  ggtitle('linear regression')+
  xlab('Level')+
  ylab('Salary')
#plot polynomial
ggplot()+
  geom_point(aes(x =Ourdata$Level,y=Ourdata$Salary), colour = 'red') +
  geom_line(aes(x = Ourdata$Level, y = predict(Polynomial_Regression, newdata = Ourdata)),colour = 'blue')+
  ggtitle('Poynomial regression')+
  xlab('Level')+
  ylab('Salary')
#predict a result with linear
pred = predict(linearRegression, data.frame(Level = 6.5))
#predict a result with polynomail
pred = predict(Polynomial_Regression, data.frame(Level = 6.5,
                                                 Level2 = 6.5^2,
                                                 Level3 = 6.5^3,
                                                 Level4 = 6.5^4,
                                                 Level5 = 6.5^5))
