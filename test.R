library(tidyverse)
ourdata=read_csv('lowbwt (1).csv')
ourdata2 = read_csv('gnp.csv')
ourdata3 = read_csv('lbwi.csv')
k =lm(data= ourdata, formula = sbp~gestage)
# 1)a)
pt(2.9,98, lower.tail = F)
# 1b) 
pt(2.9,98)
# 2)
# H0: B1 = 0 vs H1: B1 !=0
t = b1/se(b1)
t = .80161/.06525
2*pt(t,97,lower.tail = F)
lm(data=ourdata3, formula = headcirc~gestage + momage)
summary(lm(data=ourdata3, formula = headcirc~gestage + momage))
lm(data = ourdata, formula = sbp~gestage)
summary(lm(data = ourdata, formula = sbp~gestage))
# y = b0+b1x1+b2x2
# y = b0 + b1(x1+1)+ b2x2
# ynew = y + b1
# CI(.80161 +-tn-2 alpha/2 * .06525)
# 3)
plot(x = ourdata2$gnp, y = ourdata2$birthrt, xlab = 'GNP', ylab = 'Birth rate', main ='Birth rate vs GNP')
plot(x = log(ourdata2$gnp), y = ourdata2$birthrt,xlab = 'Log(GNP)', ylab = 'Birth rate', main ='Birth rate vs GNP')
lm(data = ourdata2, formula = birthrt~log(gnp) )

predict(l,newdata = ourdata2)
ggplot()+
  geom_point(aes(x =log(ourdata2$gnp),y=ourdata2$birthrt))+
  geom_smooth(aes(y= d,x= log(ourdata2$gnp)))
  