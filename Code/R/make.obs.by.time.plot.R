
rm(list=ls())
setwd('H:/quinalt')
load('midpoint.7.RData')
library(plyr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(sp)
library(ggthemes)
library(scales)
temp = params.spdf@data

data.time.plot = ggplot(temp)+geom_point(aes(x=seasonal,y=as.factor(STATION)),size=.75)+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),panel.background=element_blank())+
  scale_x_continuous('Month',expand=c(0,0),breaks=c(seq(1,228,24)),labels=c(seq(1995,2013,2)))+
ylab('Station')
plot(data.time.plot)

ggsave(filename='obs.by.station.png',data.time.plot,units='in')

library(reshape2)

test = dcast(data=data.frame(temp[,c('STATION','total.period')],isin=1),formula=STATION~total.period,fill=0,value.var='isin')
test = cbind(test[,1:168],rep(0,nrow(test)),test[,169:ncol(test)])
colnames(test) = c('STATION',seq(1,228,1))
test2 = melt(test,id.vars=c('STATION'))
test2$month = as.numeric(as.character(test2$variable))

#8dd3c7'#ffffb3','#bebada'

rank = data.frame(rank(tapply(test2$value,test2$STATION,sum),ties.method = 'first'))
rank$STATION = rownames(rank)
colnames(rank) = c('rank.nobs','STATION')
test2$rank.nobs = rank$rank.nobs[match(test2$STATION,rank$STATION)]


dataplot = ggplot(test2)+geom_point(aes(x=month,y=rank.nobs,colour=as.character(value)),shape=15)+
  scale_colour_manual('Observed',values=c('#a6cee3','#1f78b4'),labels=c('No','Yes'))+
  theme_tufte(ticks=F)+
  theme(axis.text.y=element_blank(),panel.grid.major = element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        #legend.key=element_rect(colour='black'),
        axis.text=element_text(size=12),axis.title=element_text(size=18),
        legend.direction='horizontal',
        legend.position=c(.3,.05),legend.text=element_text(size=14),legend.title=element_text(size=14),
        legend.background=element_rect(colour='black',fill=alpha('white', 0.8)))+
  scale_y_continuous('Sampling Stations',expand=c(0,0))+
  scale_x_continuous('Month',expand=c(0,0),breaks=c(seq(1,228,24)),labels=c(seq(1995,2013,2)))+
  guides(colour = guide_legend(override.aes = list(size=10)))

ggsave(filename='obsbystation.png',dataplot,units='in')



nobs = data.frame(tapply(test2$value,test2$STATION,sum))
nobs$STATION = rownames(nobs)
colnames(nobs) = c('nobs','STATION')
temp$nobs = nobs$nobs[match(temp$STATION,nobs$STATION)]

fbin = data.frame(tapply(temp$i.all.1yr,temp$STATION,sum))
fbin$STATION = rownames(fbin)
colnames(fbin) = c('fbin','STATION')
temp$fbin = fbin$fbin[match(temp$STATION,fbin$STATION)]


fbin0 = data.frame(tapply(((temp$i.all.1yr-1)*-1),temp$STATION,sum))
fbin0$STATION = rownames(fbin0)
colnames(fbin0) = c('fbin0','STATION')
temp$fbin0 = fbin0$fbin0[match(temp$STATION,fbin0$STATION)]

oregon = readOGR(dsn="H:/quinalt/government_units", layer="state_nrcs_a_or")
oregon@data$id = rownames(oregon@data)
oregon.points = fortify(oregon, region="id")
oregon.df = join(oregon.points, oregon@data, by="id")
plot(oregon)

oregon.huc8 = readOGR(dsn="H:/quinalt/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)
oregon.huc8.points = fortify(oregon.huc8, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")

ggplot()+geom_polygon(aes(x=long,y=lat,group=id),data=oregon.df) +
geom_point(aes(x=DECIMAL_LONG,y=DECIMAL_LAT,size=nobs),data=temp)+
  scale_size_continuous("N",range = c(4,8))+theme_tufte(ticks=F)+
  theme(axis.text=element_blank(),axis.title=element_blank())

class(test)
plot(test[[2]])
test

obox = bbox(oregon.huc8)
obox[1,1] = -125
obox[1,2]= -116
obox[2,1] = 41.5
obox[2,2] = 46.5

test = get_map(location = obox,maptype = 'toner',source='osm')
tmap = ggmap(test) +geom_point(aes(x=DECIMAL_LONG,y=DECIMAL_LAT,size=nobs),
                               colour='blue',data=temp,shape=21)+
  scale_size_continuous("N",range = c(4,8),breaks=seq(10,210,50))+theme_tufte(ticks=F)+
  theme(axis.text=element_blank(),axis.title=element_blank(),
        legend.direction='horizontal',legend.title=element_text(size=18),
        legend.text = element_text(size=14),
        legend.background=element_rect(fill = 'grey80',colour='black'),
        legend.position = c(.5,.035))


plot(test)
library(ggmap)
library(spplot)
library(maps)
library(mapdata)


