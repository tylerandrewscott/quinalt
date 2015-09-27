setwd('H:/quinalt/')
rm(list=ls())
load('midpoint.5.RData')

dat = all.params.spdf@data

library(shapefiles)
library(rgdal)
library(raster)

R1.ag = raster('H:/duckabush/tf_ag_1992')
R1.dev = raster('H:/duckabush/tf_dev_1992')
R1.wet = raster('H:/duckabush/tf_wetl_1992')
R1.forst = raster('H:/duckabush/tf_forst_1992')
R2.crop = raster('H:/duckabush/tf_crop_2001')
R2.past = raster('H:/duckabush/tf_past_2001')
R2.dev = raster('H:/duckabush/tf_dev_2001')
R2.wet = raster('H:/duckabush/tf_wetl_2001')
R2.forst = raster('H:/duckabush/tf_forst_2001')
R3.crop = raster('H:/duckabush/tf_crop_2006')
R3.past = raster('H:/duckabush/tf_past_2006')
R3.dev = raster('H:/duckabush/tf_dev_2006')
R3.wet = raster('H:/duckabush/tf_wetl_2006')
R3.forst = raster('H:/duckabush/tf_forst_2006')
R4.crop = raster('H:/duckabush/tf_crop_2011')
R4.past = raster('H:/duckabush/tf_past_2011')
R4.dev = raster('H:/duckabush/tf_dev_2011')
R4.wet = raster('H:/duckabush/tf_wetl_2011')
R4.forst = raster('H:/duckabush/tf_forst_2011')


uq = dat[!duplicated(dat$STATION),]
uq.coords = as.matrix(cbind(uq$DECIMAL_LONG,uq$DECIMAL_LAT))
uq.points = spTransform(all.params.spdf[!duplicated(all.params.spdf$STATION),],CRSobj=CRS(proj4string(R1.ag)))


R1.ag.v = raster::extract(R1.ag,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R1.dev.v = raster::extract(R1.dev,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R1.wet.v = raster::extract(R1.wet,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R1.forst.v = raster::extract(R1.forst,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)

R2.crop.v = raster::extract(R2.crop,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R2.past.v = raster::extract(R2.past,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R2.dev.v = raster::extract(R2.dev,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R2.wet.v = raster::extract(R2.wet,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R2.forst.v = raster::extract(R2.forst,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R2.ag.v = data.frame(ID = R2.crop.v$ID,tf_ag_2001 = R2.crop.v$tf_crop_2001+R2.past.v$tf_past_2001)

R3.crop.v = raster::extract(R3.crop,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R3.past.v = raster::extract(R3.past,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R3.dev.v = raster::extract(R3.dev,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R3.wet.v = raster::extract(R3.wet,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R3.forst.v = raster::extract(R3.forst,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R3.ag.v = data.frame(ID = R3.crop.v$ID,tf_ag_2006 = R3.crop.v$tf_crop_2006+R3.past.v$tf_past_2006)

R4.crop.v = raster::extract(R4.crop,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R4.past.v = raster::extract(R4.past,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R4.dev.v = raster::extract(R4.dev,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R4.wet.v = raster::extract(R4.wet,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R4.forst.v = raster::extract(R4.forst,uq.points,fun=mean,df=T,buffer=1000,na.rm=TRUE)
R4.ag.v = data.frame(ID = R4.crop.v$ID,tf_ag_2011 = R4.crop.v$tf_crop_2011+R4.past.v$tf_past_2011)

R1 = join_all(list(R1.ag.v,R1.dev.v,R1.wet.v,R1.forst.v))
R2 = join_all(list(R2.ag.v,R2.dev.v,R2.wet.v,R2.forst.v))
R3 = join_all(list(R3.ag.v,R3.dev.v,R3.wet.v,R3.forst.v))
R4 = join_all(list(R4.ag.v,R4.dev.v,R4.wet.v,R4.forst.v))

cov1992 = rbind(R1,R1,R1,R1,R1,R1);colnames(cov1992) = c('ID','Ag','Dev','Wetl','Forst')
cov1992$YEAR = rep(1995:2000,each=141)
cov1992$STATION = uq$STATION

cov2001 = rbind(R2,R2,R2,R2,R2);colnames(cov2001) = c('ID','Ag','Dev','Wetl','Forst')
cov2001$YEAR = rep(2001:2005,each=141)
cov2001$STATION = uq$STATION

cov2006 = rbind(R3,R3,R3,R3,R3);colnames(cov2006) = c('ID','Ag','Dev','Wetl','Forst')
cov2006$YEAR = rep(2006:2010,each=141)
cov2006$STATION = uq$STATION

cov2011 = rbind(R4,R4,R4);colnames(cov2011) = c('ID','Ag','Dev','Wetl','Forst')
cov2011$YEAR = rep(2011:2013,each=141)
cov2011$STATION = uq$STATION

cov = rbind(cov1992,cov2001,cov2006,cov2011)

dat = join(dat,cov,type='left')

all.params.spdf@data = dat

rm(list=ls()[intersect(grep('huc8.database',ls(),invert=TRUE),grep('all.params.spdf',ls(),invert=TRUE))])

setwd('H:/quinalt')
save.image('midpoint.6.RData')


