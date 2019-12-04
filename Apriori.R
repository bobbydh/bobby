#Apriori
ourdata = read.csv('Market_Basket_Optimisation.csv',header = FALSE)
#data preprocessing
library(arules)
ourdata = read.transactions('Market_Basket_Optimisation.csv',
                            sep = ',',
                            rm.duplicates = TRUE)
summary(ourdata)
itemFrequencyPlot(ourdata,topN = 10)
#Training Apriori on the dataset
rulesofApriori = apriori(data =ourdata,
                parameter = list(support = .003, confidence = .2))
#visualising the results
inspect(sort(rulesofApriori,by ='lift')[1:10])
