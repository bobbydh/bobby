ourdata = read.csv('Position_Salaries.csv')
ourdata = ourdata[2:3]
#install.packages('rpart')
library(rpart)

regress = rpart(formula =Salary ~., 
                data = ourdata,
                control = rpart.control(minsplit = 1))
thisniggashit = predict(regress, data.frame(Level= 6.5))

#library(ggplot2)
grid = seq(min(ourdata$Level), max(ourdata$Level), 0.1)
ggplot()+
  geom_point(aes(x = ourdata$Level, y = ourdata$Salary),
             colour ='red')+
  geom_line(aes(x = grid, y = predict(regress, newdata = data.frame(Level = grid)),
                colour = 'blue'))+
  ggtitle('dsd')+
  xlab('l')+
  ylab('s')

