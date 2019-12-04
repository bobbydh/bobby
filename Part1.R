#import data
dataset = read.csv('Data.csv');
#dataset = dataset[2:3]
#add in the missing data
# mean(dataset$Salary, na.rm= TRUE)
# dataset$Age = ifelse(is.na(dataset$Age),
#                      ave(dataset$Age, 
#                      FUN = function(x)
#                      mean(x,na.rm = TRUE)), dataset$Age)
# dataset$Salary = ifelse(is.na(dataset$Salary),
#                      ave(dataset$Salary,
#                      FUN = function(x)
#                      mean(x,na.rm = TRUE)), 
#                      dataset$Salary)
# mean(dataset$Salary,na.rm = TRUE)
# dataset$Country = factor(dataset$Country,
#                          levels = c('France','Spain','Germany'),
#                          labels = c(1,2,3))
# dataset$Purchased = factor(dataset$Purchased,
#                          levels = c('Yes','No'),
#                          labels = c(1,0))
#import libraryies
#install.packages('caTools')
library(caTools)
#create training set and test set
set.seed(123)
class(dataset$Purchased)
split = sample.split(dataset$Purchased,SplitRatio = 0.8)
train = subset(dataset, split == TRUE)
test = subset(dataset, split == FALSE)
#scale the data
#train[ 2:3] = scale(train[2:3])
#test[2:3] = scale(test[2:3])
