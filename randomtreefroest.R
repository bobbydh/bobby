#random forest
ourdata = read.csv('Position_Salaries.csv')
ourdata = ourdata[2:3]
#fiting random forest regresion
set.seed(1234)
regress = randomForest(x = ourdata[1],
                       y=  ourdata$Salary,
                       ntree = 500)
# install.packages('randomForest')
#install.packages('rpart')
# library(randomForest)
# library(rpart)
# library(caTools)
# library(ggplot2)

thisniggashit = predict(regress, data.frame(Level= 6.5))

#library(ggplot2)
grid = seq(min(ourdata$Level), max(ourdata$Level), 0.01)
ggplot()+
  geom_point(aes(x = ourdata$Level, y = ourdata$Salary),
             colour ='red')+
  geom_line(aes(x = grid, y = predict(regress, newdata = data.frame(Level = grid)),
                colour = 'blue'))+
  ggtitle('dsd')+
  xlab('l')+
  ylab('s')
  