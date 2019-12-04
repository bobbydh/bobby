#Regression Template
#Import the data

#Split the data

#Feature Scaling


#Fitting the the Regression model to the dataset
#  Create your regressor here

#visualising the result of the regression model
#install ggplot2
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)#higher resolution for smoother cuver
data.frame(Level = x_grid)

#predict a result 