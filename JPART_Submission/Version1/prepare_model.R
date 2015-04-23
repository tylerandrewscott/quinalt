#prepare model
rm(list=ls())
setwd('H:/quinalt/')
load('midpoint.6.RData')

library(plyr);library(dplyr);library(ggplot2);library(INLA);library(lubridate)
#source("http://www.math.ntnu.no/inla/givemeINLA.R") 
huc8.database = dplyr::select(huc8.database,-uq,-end.date,-owri.yearmonth.spend)
all.params.spdf@data = join(all.params.spdf@data,huc8.database,type='left')

params.spdf = all.params.spdf[all.params.spdf$YEAR>=1995,]

date.ranks = data.frame(end.date = seq(min(params.spdf@data$end.date),max(params.spdf@data$end.date),by='months'),
total.period = 1:length(seq(min(params.spdf@data$end.date),max(params.spdf@data$end.date),by='months')))

params.spdf@data = join(params.spdf@data,date.ranks,type='left')
names(params.spdf@data)
k = 100000
years.dropped = 5
params.spdf@data = params.spdf@data %>% 
  mutate(rsum.owri.1yr = rsum.owri.1yr/k)%>%
  mutate(rsum.owri.3yr = rsum.owri.3yr/k)%>%
  mutate(rsum.owri.5yr = rsum.owri.5yr/k)%>%
  mutate(rsum.oweb.rest.1yr = rsum.oweb.rest.1yr/k)%>%
  mutate(rsum.oweb.rest.3yr = rsum.oweb.rest.3yr/k)%>%
  mutate(rsum.oweb.rest.5yr = rsum.oweb.rest.5yr/k)%>%
  mutate(rsum.oweb.ed.1yr = rsum.oweb.ed.1yr/k)%>%
  mutate(rsum.oweb.ed.3yr = rsum.oweb.ed.3yr/k)%>%
  mutate(rsum.oweb.ed.5yr = rsum.oweb.ed.5yr/k)%>%
  mutate(rsum.oweb.all.1yr = rsum.oweb.all.1yr/k)%>%
  mutate(rsum.oweb.all.3yr = rsum.oweb.all.3yr/k)%>%
  mutate(rsum.oweb.all.5yr = rsum.oweb.all.5yr/k)%>%
  mutate(rsum.oweb.non.rest.1yr = rsum.oweb.non.rest.1yr/k)%>%
  mutate(rsum.oweb.non.rest.3yr = rsum.oweb.non.rest.3yr/k)%>%
  mutate(rsum.oweb.non.rest.5yr = rsum.oweb.non.rest.5yr/k)%>%
  mutate(rsum.oweb.tech.1yr = rsum.oweb.tech.1yr/k)%>%
  mutate(rsum.oweb.tech.3yr = rsum.oweb.tech.3yr/k)%>%
  mutate(rsum.oweb.tech.5yr = rsum.oweb.tech.5yr/k)%>%
  mutate(rsum.oweb.council.1yr = rsum.oweb.council.1yr/k)%>%
  mutate(rsum.oweb.council.3yr = rsum.oweb.council.3yr/k)%>%
  mutate(rsum.oweb.council.5yr = rsum.oweb.council.5yr/k)%>%  
  mutate(l.owri.1yr = log(rsum.owri.1yr+0.001)) %>%
  mutate(l.owri.3yr = log(rsum.owri.3yr+0.001)) %>%
  mutate(l.owri.5yr = log(rsum.owri.5yr+0.001)) %>%
  mutate(l.rest.1yr = log(rsum.oweb.rest.1yr+0.001)) %>%
  mutate(l.rest.3yr = log(rsum.oweb.rest.3yr+0.001)) %>%
  mutate(l.rest.5yr = log(rsum.oweb.rest.5yr+0.001)) %>%
  mutate(l.ed.1yr = log(rsum.oweb.ed.1yr+0.001)) %>%
  mutate(l.ed.3yr = log(rsum.oweb.ed.3yr+0.001)) %>%
  mutate(l.ed.5yr = log(rsum.oweb.ed.5yr+0.001)) %>%
  mutate(l.non.rest.1yr = log(rsum.oweb.non.rest.1yr+0.001)) %>%
  mutate(l.non.rest.3yr = log(rsum.oweb.non.rest.3yr+0.001)) %>%
  mutate(l.non.rest.5yr = log(rsum.oweb.non.rest.5yr+0.001)) %>%
  mutate(l.all.1yr = log(rsum.oweb.all.1yr+0.001)) %>%
  mutate(l.all.3yr = log(rsum.oweb.all.3yr+0.001)) %>%
  mutate(l.all.5yr = log(rsum.oweb.all.5yr+0.001)) %>%
  mutate(l.tech.1yr = log(rsum.oweb.tech.1yr+0.001)) %>%
  mutate(l.tech.3yr = log(rsum.oweb.tech.3yr+0.001)) %>%
  mutate(l.tech.5yr = log(rsum.oweb.tech.5yr+0.001)) %>%
  mutate(l.council.1yr = log(rsum.oweb.council.1yr+0.001)) %>%
  mutate(l.council.3yr = log(rsum.oweb.council.3yr+0.001)) %>%
  mutate(l.council.5yr = log(rsum.oweb.council.5yr+0.001)) %>%
  mutate(i.owri.1yr = ifelse(rsum.owri.1yr==0,0,1)) %>%
  mutate(i.owri.3yr = ifelse(rsum.owri.3yr==0,0,1)) %>%
  mutate(i.owri.5yr = ifelse(rsum.owri.5yr==0,0,1)) %>%
  mutate(i.rest.1yr = ifelse(rsum.oweb.rest.1yr==0,0,1)) %>%
  mutate(i.rest.3yr = ifelse(rsum.oweb.rest.3yr==0,0,1)) %>%
  mutate(i.rest.5yr = ifelse(rsum.oweb.rest.5yr==0,0,1)) %>%
  mutate(i.ed.1yr = ifelse(rsum.oweb.ed.1yr==0,0,1)) %>%
  mutate(i.ed.3yr = ifelse(rsum.oweb.ed.3yr==0,0,1)) %>%
  mutate(i.ed.5yr = ifelse(rsum.oweb.ed.5yr==0,0,1)) %>%
  mutate(i.non.rest.1yr = ifelse(rsum.oweb.non.rest.1yr==0,0,1)) %>%
  mutate(i.non.rest.3yr = ifelse(rsum.oweb.non.rest.3yr==0,0,1)) %>%
  mutate(i.non.rest.5yr = ifelse(rsum.oweb.non.rest.5yr==0,0,1)) %>%
  mutate(i.all.1yr = ifelse(rsum.oweb.all.1yr==0,0,1)) %>%
  mutate(i.all.3yr = ifelse(rsum.oweb.all.3yr==0,0,1)) %>%
  mutate(i.all.5yr = ifelse(rsum.oweb.all.5yr==0,0,1)) %>%
  mutate(i.tech.1yr = ifelse(rsum.oweb.tech.1yr==0,0,1)) %>%
  mutate(i.tech.3yr = ifelse(rsum.oweb.tech.3yr==0,0,1)) %>%
  mutate(i.tech.5yr = ifelse(rsum.oweb.tech.5yr==0,0,1)) %>%
  mutate(i.council.1yr = ifelse(rsum.oweb.council.1yr==0,0,1)) %>%
  mutate(i.council.3yr = ifelse(rsum.oweb.council.3yr==0,0,1)) %>%
  mutate(i.council.5yr = ifelse(rsum.oweb.council.5yr==0,0,1)) %>%  
  mutate(elevation = elevation  / 100) %>%
  mutate(l.elevation = log(elevation)) %>%
  mutate(ag.huc8 = ag.huc8*100) %>%
  mutate(l.ag = log(ag.huc8+0.1)) %>%
  mutate(dev.huc8 = dev.huc8*100) %>%
  mutate(l.dev = log(dev.huc8+0.1)) %>%
  mutate(wet.huc8 = wet.huc8*100) %>%
  mutate(l.wet = log(wet.huc8+0.1)) %>%
  mutate(forst.huc8 = forst.huc8*100) %>%
  mutate(l.forst = log(forst.huc8+0.1)) %>%
  mutate(Ag = Ag * 100) %>%
  mutate(Dev = Dev * 100) %>%
  mutate(Wetl = Wetl * 100) %>%
  mutate(Forst = Forst * 100) %>%
  mutate(l.owqi = log(owqi)) %>%
  mutate(SEASON = ifelse(MONTH.NUM>=6&MONTH.NUM<=9,'SUM','FWS')) %>%
  mutate(seaDist = seaDist/10) %>% 
  mutate(seasonal = total.period)
  

dat = params.spdf@data
dat = arrange(dat,STATION,total.period)

require(INLA)
require(fields)
require(abind)
library(maptools)
library(splancs)
library(rgdal);library(rgeos);library(ggplot2)
#load oregon huc8 shapefile
oregon.huc8 = readOGR(dsn="H:/quinalt/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)
#join into one polygon

reg <- unionSpatialPolygons(oregon.huc8, rep(1,nrow(oregon.huc8)),threshold=10000)
oregon.huc8.points = fortify(reg, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")
oregon.huc8.df = filter(oregon.huc8.df,hole==FALSE)


stat.count  = as.data.frame(table(dat$STATION))
stat.count$row = rownames(stat.count)

id = NULL
for (i in 1:nrow(stat.count))
{
  id = append(id,rep(stat.count$row[i],stat.count$Freq[i]))
}

dat$STATION.ID = id
 

borders.or = cbind(oregon.huc8.df$long,oregon.huc8.df$lat)
coordinates.or = data.frame(Station.ID = dat$STATION.ID[!duplicated(dat$STATION.ID)],
                            LONG = dat$DECIMAL_LONG[!duplicated(dat$STATION)],
                            LAT = dat$DECIMAL_LAT[!duplicated(dat$STATION)])
mesh.or = inla.mesh.2d(loc=cbind(coordinates.or$LONG,coordinates.or$LAT),
                       loc.domain = borders.or,
                      # offset=c(5),
                       max.edge=c(10,20),
                       min.angle=c(26,21),
                       cutoff=0,
                        n=16,
                       plot.delay=NULL)

in.stack = 
  c(grep('l.owri',names(dat),value=T),grep('l.ed',names(dat),value=T),
    grep('l.non',names(dat),value=T),grep('l.council',names(dat),value=T),
    grep('l.tech',names(dat),value=T),grep('l.all',names(dat),value=T),
    grep('l.rest',names(dat),value=T),grep('i.rest',names(dat),value=T),
    grep('i.owri',names(dat),value=T),grep('i.ed',names(dat),value=T),
    grep('i.non',names(dat),value=T),grep('i.council',names(dat),value=T),
    grep('i.tech',names(dat),value=T),grep('i.all',names(dat),value=T),
    'l.ag','l.dev','l.forst','l.wet',
    'wet.huc8','dev.huc8','ag.huc8','forst.huc8','elevation','Ag','Wetl','Forst','Dev',
    'seaDist','l.elevation','total.period','HUC8','MONTH','YEAR','MONTH.NUM',
    'COUNTY','DECIMAL_LONG','DECIMAL_LAT','STATION','STATION.ID','seasonal','SEASON')


rm(list=ls()['params.spdf'!=ls()])
save.image('midpoint.7.RData')
