ourdata = read.csv('Position_Salaries.csv')
ourdata = ourdata[2:3]
#SVR to the dataset
#install.packages('e1071')
# library(e1071)
# library(ggplot2)
# library(caTools)

regress = svm(formula = Salary ~.,
              data = ourdata,
              type = 'eps-regression')

predicttt = predict(regress, data.frame(Level = 6.5))

ggplot() + 
  geom_point(aes(x = ourdata$Level, y = ourdata$Salary),
             colour ='red')+
  geom_line(aes(x = ourdata$Level, y = predict(regress, newdata = ourdata)))
