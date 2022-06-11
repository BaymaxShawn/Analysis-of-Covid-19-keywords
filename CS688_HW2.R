
#cs688_HW2
#Shawn_HU

setwd('/Users/huchangguo/Desktop')
getwd()
dataHW2 <- read.csv('dataHW2.csv')

#1
library(stringr)
KW <- data.frame(KW = c('Covid-19','vaccine','mRNA','booster'))
YE <- data.frame(YE = 2019:2022)
CO <- merge(KW,YE)
CO$frequency= 0
for ( ye in 2020:2022){
  da1 <- subset(dataHW2,dataHW2$year == as.character(ye))
  for (k in c('Covid-19','vaccine','mRNA','booster')){
    for (i in 1:nrow(da1)){
      TI <- str_count(da1[i,'title'],k)
      AB <- str_count(da1[i,'abstract'],k)
      if (is.na(TI)){TI = 0}
      if (is.na(AB)){AB = 0}
      CO[CO$KW == k & CO$YE == ye,]$frequency = CO[CO$KW == k&CO$YE==ye,]$frequency+TI+AB
    }
  }
}
CO


#Gaussian distribution
for (k in c('Covid-19','vaccine','mRNA','booster')){
  qq <- CO[CO$KW==k,]$frequency
  qq <- qq/sum(qq)
  sum(qq) 
  qqnorm(qq,main = k)
}



#density plot 
for (k in c('Covid-19','vaccine','mRNA','booster')){
  pp <- CO[CO$KW==k,]$frequency
  pp <- pp/sum(pp)
  sum(pp) 
  year <- 2019:2022 
  plot(year,pp, type="h",col=4, main=k ,xlab="Year of News",ylab="Density")
  points(year,pp,col=4)
}




#2a
KWse <- data.frame(KWse = c('obesity','cancer'))
YEse <- data.frame(YEse = 2019:2021)
COse <- merge(KWse,YEse)
COse$frequency = 0
for (ye in 2020:2021){
  da2 <- subset(dataHW2,dataHW2$year== as.character(ye))
  for (k in c('obesity','cancer')){
    for (i in 1:nrow(da2)){
      TI <- str_count(da2[i,'title'],k)
      AB <- str_count(da2[i,'abstract'],k)
      if (is.na(TI)){TI = 0}
      if (is.na(AB)){AB = 0}
      COse[COse$KWse == k& COse$YEse == ye,]$frequency = COse[COse$KWse == k&COse$YEse==ye,]$frequency+TI+AB
    }
  }
}
COse

#three test
CAN <- COse[COse$KWse == 'cancer',]$frequency
OBE <- COse[COse$KWse == 'obesity',]$frequency
#one parametric
t.test(OBE,CAN)
#two non-parametric tests 
install.packages("dgof")
library("dgof")
ks.test(OBE,CAN)
wilcox.test(OBE,CAN)



#2b
install.packages("effsize")
library(effsize)
qu = cohen.d(OBE,CAN,return.dm=TRUE)
print(qu)


#3
cor(OBE,CAN,method = "pearson")
cor(OBE,CAN,method = "kendall")
cor(OBE,CAN,method = "spearman")
#越接近1正相关性越强，靠近0就是没有关系，靠近-1就是负相关性

