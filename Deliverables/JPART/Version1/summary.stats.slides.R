setwd('//Users/TScott/Google Drive/quinalt/APPAM_2014')
load('midpoint.7.RData')

library(sp)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
temp = params.spdf@data


temp$



summarise_each(temp[,grep('rsum',names(temp))],funs(mean(.)))*100000

lt = melt(temp,id.vars=c('STATION','MonthYear','YEAR','MONTH'))
lt = lt[grep('rsum',lt$variable),]


sum(temp$rsum.oweb.council.1yr==0)
sum(temp$rsum.oweb.tech.1yr==0)
sum(temp$rsum.oweb.all.1yr==0)
sum(temp$rsum.oweb.ed.1yr==0)
sum(temp$rsum.oweb.rest.1yr==0)


sum(temp$rsum.oweb.council.1yr!=0)
sum(temp$rsum.oweb.tech.1yr!=0)
sum(temp$rsum.oweb.all.1yr!=0)
sum(temp$rsum.oweb.ed.1yr!=0)
sum(temp$rsum.oweb.rest.1yr!=0)

mean(temp$rsum.oweb.council.1yr[temp$rsum.oweb.council.1yr!=0])*100000
mean(temp$rsum.oweb.tech.1yr[temp$rsum.oweb.tech.1yr!=0])*100000
mean(temp$rsum.oweb.all.1yr[temp$rsum.oweb.all.1yr!=0])*100000
mean(temp$rsum.oweb.ed.1yr[temp$rsum.oweb.ed.1yr!=0])*100000
mean(temp$rsum.oweb.rest.1yr[temp$rsum.oweb.rest.1yr!=0])*100000



sum(temp$rsum.oweb.council.3yr==0)
sum(temp$rsum.oweb.council.5yr==0)
sum(temp$rsum.oweb.council.1yr!=0)
sum(temp$rsum.oweb.council.3yr!=0)
sum(temp$rsum.oweb.council.5yr!=0)



sum(lt$value==0)



ggplot(lt)+geom_histogram(aes(x=as.numeric(value),group=variable,fill=variable),position='dodge')


sum(lt$value>300)
summary(as.numeric(lt$value))


head(lt)

names(temp)

ggplot(temp)


               ,


?summarise_each

dim(params.spdf@data)
