#Natural Language Prcoessing
ourdata = read.delim('Restaurant_Reviews.tsv', quote = '',
                     stringsAsFactors = FALSE)
#cleaning data
# install.packages('tm')
# install.packages('SnowballC')
# library(SnowballC)
 library(tm)
cor  = VCorpus(VectorSource(ourdata$Review))
cor = tm_map(cor,content_transformer(tolower))
cor = tm_map(cor,removeNumbers)
cor = tm_map(cor,removePunctuation)
cor = tm_map(cor,removeWords,stopwords())
cor = tm_map(cor,stemDocument)
cor = tm_map(cor,stripWhitespace)

dtm = DocumentTermMatrix(cor)
dtm = removeSparseTerms(dtm,.999)
datatomatrix = as.data.frame(as.matrix(dtm))
datatomatrix$Liked = ourdata$Liked
#splitng the data
library(caTools)
splitthisshit = sample.split(datatomatrix$Liked, SplitRatio = .8)
trains = subset(datatomatrix, splitthisshit == TRUE)
testanigga = subset(datatomatrix, splitthisshit == FALSE)

#kernel-svm
library(e1071)
cl = svm(formula = Liked ~.,
         data = trains,
         type = 'C-classification',
         kernel = 'radial')
theprediction  =predict(cl,newdata = testanigga[,-692])

c1 = table(theprediction,testanigga[,692])

#KNN
#the fitting K-NN ot the train set and predict the test set
#install.packages('class')
library(class)
pred = knn(train = trains[,-692],
           test= testanigga[,-692],
           cl = trains[,692],
           k = 5)

#making the confucsion matrix
cm = table(testanigga[,692],
           pred)


#forest tree

#encoding the target feature as factor
datatomatrix$Liked = factor(datatomatrix$Liked,
                           levels = c(0,1))
library(caTools)
splitthisshit = sample.split(datatomatrix$Liked, SplitRatio = .8)
trains = subset(datatomatrix, splitthisshit == TRUE)
testanigga = subset(datatomatrix, splitthisshit == FALSE)


library(rpart)
#install.packages("randomForest")
library(randomForest)
cL = randomForest(x = trains[-692],
                  y = trains$Liked,
                  ntree = 10)
pred = predict(cL, newdata =testanigga[-692])
cm = table(testanigga[,692],pred)
