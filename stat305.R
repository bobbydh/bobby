library(tidyverse)
ggplot(data = mpg, aes(x = displ,y =hwy, color =class))+
  geom_point()
ggplot(data=mpg, mapping = aes(x=displ,y=hwy,color=class)) + geom_point() + geom_smooth() + facet_wrap(~ class)
ggplot(data = hiv, aes(x= hiv$year,y = hiv$prevalence, group = hiv$Country))+
  geom_smooth(se = FALSE)+
  geom_smooth(data =hihiv, aes(x = hihiv$year,y=hihiv$prevalence,group = hihiv$Country),color ='red')
hiv<- rename(hiv,Country = Estimated.HIV.Prevalence.....Ages.15.49.)
hiv<- select(hiv,Country,X1990:X2011)
hiv<-arrange(hiv,desc(X2011))
hivcopy %>%rename(Country = Estimated.HIV.Prevalence.....Ages.15.49.)%>%
  select(Country,X1990:X2011)%>%
  arrange(desc(X2011))
k<-read_tsv('MLLT3_small.vcf',comment='##')
k<-read_tsv('MLLT3_small.vcf',comment='##',col_types = cols(
  REF = col_factor(),
  ALT = col_factor()
))
k<-rename(k,chrom = `#CHROM`)
hiv[1]
hiv<-rename(hiv,Country=`Estimated HIV Prevalence% - (Ages 15-49)`)
hiv<-select(hiv,-(`1979`:`1989`))

hiv<-gather(hiv,`1990`:`2011`,key=year,value =prevalance,na.rm = TRUE )
hiv%>%arrange(Country)
hiv<-arrange(hiv,Country)
flights%>% select(year,month,day,tailnum)%>% mutate(row_number())
flights2%>%left_join(airports,by=c('dest'='faa'))
stats %>%rename(station = ID)%>%left_join(station,by=c('station'='ID'))
flights%>% left_join(airports, by= c('dest'='faa'))
k<-flights%>%count(year,month,day,flight,tailnum)%>%filter(n >1,!is.na(tailnum))
flights%>% semi_join(k)%>%select(year:day,carrier,flight,origin,dest)
k<-flights %>% group_by(year,month,day)%>% 
  summarize(total_delay = sum(dep_delay,na.rm=TRUE))
ok%>%gather(`2010`:`2014`,key= year,value=unemployment)
ggplot(data=ok,aes(x=year,y=unemployment,group = `Country Code`))+geom_smooth()
s<-s%>%mutate(`Country Name`=str_replace(`Country Name`,'\\(IDA & IBRD countries\\)$|\\(IDA & IBRD\\)$'," "))
ok%>%filter(ok$`Country Name`,str_detect('^World$'))

hui%>%mutate(HUIDCOG =fct_recode(HUIDCOG,
  'good memory, clear thinking'= 'COG. ATT. LEVE 1'
))
hui%>%group_by(HUIDCOG)%>%
  summarize(wtd.n = sum(WTS_M))
