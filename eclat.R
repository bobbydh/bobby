#eclate
ourdata = read.csv('Market_Basket_Optimisation.csv', header = FALSE)

#data preprocessing
library(arules)
ourdata = read.transactions('Market_Basket_Optimisation.csv',
                            sep = ',',
                            rm.duplicates = TRUE)
summary(ourdata)
itemFrequencyPlot(ourdata,topN = 10)
#Training Eclate on the dataset
elcatmodel = eclat(data =ourdata,
                   parameter = list(support = .004,  minlen = 2),
                  )
#visualising the results
inspect(sort(elcatmodel,by ='support')[1:10])
