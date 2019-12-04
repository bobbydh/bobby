#Upper confidence Bound


#importing the dataset
ourdata = read.csv('Ads_CTR_Optimisation.csv')

#implementing UCB
N = 10000
k= 10
number_of_selections = integer(k)
sums = integer(k)
adthatareselected = integer(0)
total = 0
for(n in 1:N){
  ad =0
  upperboundmax = 0
  for(i in 1:k){
    if(number_of_selections[i]>0){
    theaverage = sums[i]/number_of_selections[i]
    delta = sqrt(3/2*log(n)/number_of_selections[i])
    upperbound = theaverage + delta
    }else{
      upperbound = 1e400
    }
   if(upperbound>upperboundmax){
     upperboundmax = upperbound
     ad = i
   }
  }
  adthatareselected = append(adthatareselected, ad)
  number_of_selections[ad] = number_of_selections[ad] +1
  reward = ourdata[n,ad]
  sums[ad] = sums[ad] + reward
  total = total + reward
}
#visuallising the results - histogram
hist(adthatareselected,
     col = 'blue',
     main = 'histogram of ads selections',
     xlab = 'Ads',
     ylab= 'ads selected')
