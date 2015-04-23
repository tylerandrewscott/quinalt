

rm(list=ls())
getwd()
setwd("/homes/tscott1/win/user/quinalt/")
load('midpoint.7.v2.RData')

temp = params.spdf@data
qual.tab = rbind(
round(table(cut(temp$owqi,breaks = c(0,59,79,84,89,100),right = T))/length(temp$owqi),2),
round(table(cut(temp$owqi*1.02,breaks = c(0,59,79,84,89,max(temp$owqi*1.02)),right = T))/length(temp$owqi),2),
round(table(cut(temp$owqi*1.05,breaks = c(0,59,79,84,89,max(temp$owqi*1.06)),right = T))/length(temp$owqi),2),
round(table(cut(temp$owqi*1.08,breaks = c(0,59,79,84,89,max(temp$owqi*1.08)),right = T))/length(temp$owqi),2))

colnames(qual.tab) = c('Very Poor','Poor','Fair','Good','Excellent')
rownames(qual.tab) = c('Observed','+2%','+5%','+8%')

library(reshape2)
long.qual = melt(qual.tab);colnames(long.qual) = c('Which','Category','Proportion')
library(ggplot2)



temp$owqi.cat.5n = cut(temp$owqi*0.95,breaks = c(0,59,79,84,89,max(temp$owqi*0.94)),right = T);levels(temp$owqi.cat.5n) =c(1:5)
temp$owqi.cat.2n = cut(temp$owqi*0.98,breaks = c(0,59,79,84,89,max(temp$owqi*0.97)),right = T);levels(temp$owqi.cat.2n) =c(1:5)
temp$owqi.cat.obs = cut(temp$owqi,breaks = c(0,59,79,84,89,100),right = T);levels(temp$owqi.cat.obs) =c(1:5)
temp$owqi.cat.2p = cut(temp$owqi*1.02,breaks = c(0,59,79,84,89,max(temp$owqi*1.03)),right = T);levels(temp$owqi.cat.2p) =c(1:5)
temp$owqi.cat.5p = cut(temp$owqi*1.05,breaks = c(0,59,79,84,89,max(temp$owqi*1.06)),right = T);levels(temp$owqi.cat.5p) =c(1:5)


library(plyr)
library(dplyr)
tt = temp %>% select(owqi.cat.5n,owqi.cat.2n,owqi.cat.obs,owqi.cat.2p,owqi.cat.5p)

df = data.frame(Category = c(tt[,1],tt[,2],tt[,3],tt[,4],tt[,5]),
Sensitivity = rep(c(1:5),each=nrow(tt)))


pp = ggplot(df) + geom_histogram(aes(x=as.factor(Sensitivity),fill=as.factor(Category)),position='dodge') + 
  scale_x_discrete(labels=c('-5%','-2%','Observed','+2%','+5%'),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme_tufte(ticks=FALSE)+
  theme(axis.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = 'black',size=20),
        legend.title=element_text(size=22),
        legend.text = element_text(size=20),
       # panel.background=element_rect(fill='transparent',colour = NA),
       panel.background=element_rect(fill='transparent',colour=NA),
       plot.background=element_rect(fill='transparent',colour=NA),
       legend.position = c(.75,.3)
        )+
  scale_fill_manual(name='Designation',labels=c('Very Poor','Poor','Fair','Good','Excellent'),
values=c('#d7191c', '#fdae61', '#ffffbf','#abdda4','#2b83ba'),
guide = guide_legend(reverse=T))+
  coord_flip()

plot(pp)

ggsave(filename = 'sensitivityplot.png',pp)
?ggsave


renderPlot({pp
}, bg="transparent")


?theme


  ?scale_fill_discrete



