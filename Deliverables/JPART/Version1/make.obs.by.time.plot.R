

load('midpoint.7.RData')
library(plyr)
library(dplyr)

data.time.plot = ggplot(temp)+geom_point(aes(x=seasonal,y=as.factor(STATION)))+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),panel.background=element_blank())+
  scale_x_continuous('Month',expand=c(0,0),breaks=c(seq(1,228,12)),labels=c(1995:2013))+
ylab('Station')

ggsave(filename='obs.by.station.png',data.time.plot,width=6,height=4,units='in')


