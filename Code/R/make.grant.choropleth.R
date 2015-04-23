
rm(list = ls(all = TRUE))
setwd('H:/quinalt')
load('midpoint.6.RData')
require(xtable)
library(INLA)
library(rgdal);library(rgeos);library(maptools)
library(plyr);library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
oregon.huc8 = readOGR(dsn="H:/quinalt/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)
oregon.huc8.points = fortify(oregon.huc8, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")

huc8.jul03 = filter(huc8.database,end.date==mdy('7/15/03'))

huc8.jul03 = join(oregon.huc8.df,huc8.jul03)

library(scales)


fund.plot.jul03 = ggplot(huc8.jul03) + geom_polygon(aes(x=long,y=lat,group=id,fill=rsum.oweb.all.1yr),colour='black')+
  theme_tufte(ticks=F) + 
  theme(axis.text=element_blank(),axis.title=element_blank(),legend.title=element_text(size=18),legend.text=element_text(size=14),
        legend.direction='horizontal',legend.position=c(.5,.1),
        legend.background = element_rect(fill='white',colour='black'))+
  scale_fill_gradient(name='OWEB Funding: July 02-June 03  ',high='red',low='white',breaks=c(seq(0,700000,50000)),
                     labels=c('$0','','','','','','$300k','',
                              '','','','','$600k','',''))

plot(fund.plot.jul03)

