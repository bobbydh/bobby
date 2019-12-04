#thomas sampling

ourdata = read.csv('Ads_CTR_Optimisation.csv')

#implementing thomas sampling
N = 10000
k= 10
d= 10
adthatareselected = integer(0)
number_of_rewards1 = integer(d)
number_of_rewards0 =integer(d)
total = 0
for(n in 1:N){
  ad =0
  randommax = 0
  for(i in 1:k){
    random_draw_beta =rbeta(n=1,
                            shape1 = number_of_rewards1[i]+1,
                            shape2 = number_of_rewards0[i]+1)
    
    if(random_draw_beta>randommax){
        randommax = random_draw_beta
      ad = i
    }
  }
  adthatareselected = append(adthatareselected, ad)
  reward = ourdata[n,ad]
  if(reward == 1){
    number_of_rewards1[ad] = number_of_rewards1[ad]+1
  }else{
    number_of_rewards0[ad] = number_of_rewards0[ad]+1
  }
  total = total + reward
}
#visuallising the results - histogram
hist(adthatareselected,
     col = 'blue',
     main = 'histogram of ads selections',
     xlab = 'Ads',
     ylab= 'ads selected')
