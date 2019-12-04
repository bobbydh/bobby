dataset = read.csv('Salary_Data.csv')
#install.packages('caTools')
#getting our test and train set
#set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
train = subset(dataset, split == TRUE)
test = subset(dataset, split == FALSE)
#to get the best fit line
regress = lm(formula = Salary ~ YearsExperience, 
               data = train)
#to predict our data 
predit = predict(regress, newdata = test)

#install.packages('ggplot2')
library(ggplot2)
ggplot() + 
  geom_point(aes(x= train$YearsExperience, y = train$Salary),
             colour = 'red') + 
  geom_line(aes(x = train$YearsExperience, y = predict(regress, newdata = train)),
            colour = 'green') +
  ggtitle('Salary vs Experience(training set)')+
  xlab('Years of Experience')+
  ylab('Salary')
#to predict our data 
predit = predict(regress, newdata = test)

#install.packages('ggplot2')
library(ggplot2)
#to plot test
ggplot() + 
  geom_point(aes(x= test$YearsExperience, y = test$Salary),
             colour = 'blue') + 
  geom_line(aes(x = train$YearsExperience, y = predict(regress, newdata = train)),
            colour = 'green') +
  ggtitle('Salary vs Experience(training set)')+
  xlab('Years of Experience')+
  ylab('Salary')
