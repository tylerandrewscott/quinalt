rm(list=ls())
setwd('/homes/tscott1/win/user/quinalt')

require(RODBC)
require(plyr)
require(rgdal)
require(reshape2)
require(maptools)
require(lubridate)
require(sp)
require(gridExtra)
require(lattice)
require(ggplot2)
require(splancs)
require(fields)
library(raster)
require(ggplot2)
#source("http://www.math.ntnu.no/inla/givemeINLA.R")
require(sp)
require(rgdal)
require(rgeos)
require(ggplot2)
require(maptools)
require(plyr)
require(dplyr)


library(shapefiles)
library(rgdal)
library(raster)


wq.dat = read.csv('Input/Scott_OWQI_1980_2013.csv',header=T,skip=1)
station.locs = read.csv('Input/oregon_wq_stations.csv')

wq.dat = join(wq.dat,station.locs)

wq.dat = wq.dat[!is.na(wq.dat$Decimal_Lat),]

all.params.spdf = SpatialPointsDataFrame(coords = matrix(cbind(wq.dat$Decimal_long,
                                                               wq.dat$Decimal_Lat),ncol=2),
                                         data=wq.dat,proj4string=CRS("+datum=NAD83 +proj=longlat"))

foo <- function(x, year=1968){
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}

all.params.spdf@data$DATE = mdy(all.params.spdf$Date)
all.params.spdf@data$Month.Num = month(all.params.spdf$DATE)
all.params.spdf@data$YEAR = year(foo(all.params.spdf$DATE))
all.params.spdf@data$DATE = foo(all.params.spdf$DATE)


all.params.spdf = all.params.spdf[all.params.spdf@data$YEAR>=1992,]


uq = all.params.spdf[!duplicated(all.params.spdf@data$Station),]

uq.coords = as.matrix(cbind(uq$Decimal_long,uq$Decimal_Lat))

######ADD County location
# oregon.eco = readOGR(dsn="SpatialData/ecoregion", layer="ecoregion")
# oregon.eco@data$id = rownames(oregon.eco@data)
# oregon.eco = spTransform(x = oregon.eco,CRSobj = CRS(proj4string(all.params.spdf)),)
# oregon.eco.points = fortify(oregon.eco, region="id")
# oregon.eco.df = join(oregon.eco.points, oregon.eco@data, by="id")
# 
# which.eco = over(all.params.spdf,oregon.eco)
# all.params.spdf@data$ECOREG3 = which.eco$LEV3_NAME
#oregon.prec.aug = readOGR(dsn="H:/quinalt/climate/precipitation", layer="precip1981_2010aug_a_or")
#oregon.prec.aug@data$id = rownames(oregon.prec.aug@data)

m = data.frame(lon = uq$Decimal_long,lat = uq$Decimal_Lat)
us.alt = getData('alt',country='US')
elevation.df = cbind(m, alt = raster::extract(us.alt[[1]], m, method = "bilinear"))
uq@data$elevation = elevation.df$alt

rm(elevation.df)
rm(us.alt)


#load oregon boundary shapefile
oregon = readOGR(dsn="SpatialData/government_units", layer="state_nrcs_a_or")
oregon@data$id = rownames(oregon@data)
oregon.points = fortify(oregon, region="id")
oregon.df = join(oregon.points, oregon@data, by="id")

#load oregon huc8 shapefile
oregon.huc8 = readOGR(dsn="SpatialData/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)
#join into one polygon
reg <- unionSpatialPolygons(oregon.huc8, rep(1,91),100)
oregon.huc8.points = fortify(reg, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")
#create oregon coastline dataframe
oregon.coast.df = subset(oregon.huc8.df,long<(-124)&lat>=min(oregon.df[,2]))

mat.dists <- spDists(uq.coords, as.matrix(oregon.coast.df[,1:2]), longlat=TRUE)
uq@data$seaDist <-  apply(mat.dists, 1, min)

rm(mat.dists)
rm(oregon.coast.df)
rm(oregon.huc8.df)
rm(oregon.df)
rm(oregon)
rm(oregon.huc8)

R1.ag = raster('SpatialData/tf_rasters/tf_ag_1992')
R1.dev = raster('SpatialData/tf_rasters/tf_dev_1992')
R1.wet = raster('SpatialData/tf_rasters/tf_wetl_1992')
R1.forst = raster('SpatialData/tf_rasters/tf_forst_1992')
R2.crop = raster('SpatialData/tf_rasters/tf_crop_2001')
R2.past = raster('SpatialData/tf_rasters/tf_past_2001')
R2.dev = raster('SpatialData/tf_rasters/tf_dev_2001')
R2.wet = raster('SpatialData/tf_rasters/tf_wetl_2001')
R2.forst = raster('SpatialData/tf_rasters/tf_forst_2001')
R3.crop = raster('SpatialData/tf_rasters/tf_crop_2006')
R3.past = raster('SpatialData/tf_rasters/tf_past_2006')
R3.dev = raster('SpatialData/tf_rasters/tf_dev_2006')
R3.wet = raster('SpatialData/tf_rasters/tf_wetl_2006')
R3.forst = raster('SpatialData/tf_rasters/tf_forst_2006')
R4.crop = raster('SpatialData/tf_rasters/tf_crop_2011')
R4.past = raster('SpatialData/tf_rasters/tf_past_2011')
R4.dev = raster('SpatialData/tf_rasters/tf_dev_2011')
R4.wet = raster('SpatialData/tf_rasters/tf_wetl_2011')
R4.forst = raster('SpatialData/tf_rasters/tf_forst_2011')


uq = spTransform(uq,CRSobj=CRS(proj4string(R1.ag)))

R1.ag.v = raster::extract(R1.ag,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R1.ag.v = raster::extract(R1.ag,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R1.dev.v = raster::extract(R1.dev,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R1.wet.v = raster::extract(R1.wet,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R1.forst.v = raster::extract(R1.forst,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)

R2.crop.v = raster::extract(R2.crop,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R2.past.v = raster::extract(R2.past,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R2.dev.v = raster::extract(R2.dev,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R2.wet.v = raster::extract(R2.wet,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R2.forst.v = raster::extract(R2.forst,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R2.ag.v = data.frame(ID = R2.crop.v$ID,tf_ag_2001 = R2.crop.v$tf_crop_2001+R2.past.v$tf_past_2001)

R3.crop.v = raster::extract(R3.crop,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R3.past.v = raster::extract(R3.past,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R3.dev.v = raster::extract(R3.dev,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R3.wet.v = raster::extract(R3.wet,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R3.forst.v = raster::extract(R3.forst,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R3.ag.v = data.frame(ID = R3.crop.v$ID,tf_ag_2006 = R3.crop.v$tf_crop_2006+R3.past.v$tf_past_2006)

R4.crop.v = raster::extract(R4.crop,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R4.past.v = raster::extract(R4.past,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R4.dev.v = raster::extract(R4.dev,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R4.wet.v = raster::extract(R4.wet,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R4.forst.v = raster::extract(R4.forst,uq,fun=mean,df=T,buffer=100,na.rm=TRUE)
R4.ag.v = data.frame(ID = R4.crop.v$ID,tf_ag_2011 = R4.crop.v$tf_crop_2011+R4.past.v$tf_past_2011)


R1 = join_all(list(R1.ag.v,R1.dev.v,R1.wet.v,R1.forst.v))
R2 = join_all(list(R2.ag.v,R2.dev.v,R2.wet.v,R2.forst.v))
R3 = join_all(list(R3.ag.v,R3.dev.v,R3.wet.v,R3.forst.v))
R4 = join_all(list(R4.ag.v,R4.dev.v,R4.wet.v,R4.forst.v))


cov1992 = rbind(R1,R1,R1,R1,R1,R1,R1,R1,R1);colnames(cov1992) = c('ID','Ag','Dev','Wetl','Forst')
cov1992$YEAR = rep(1992:2000,each=dim(uq)[1])
cov1992$Station = uq$Station

cov2001 = rbind(R2,R2,R2,R2,R2);colnames(cov2001) = c('ID','Ag','Dev','Wetl','Forst')
cov2001$YEAR = rep(2001:2005,each=dim(uq)[1])
cov2001$Station = uq$Station

cov2006 = rbind(R3,R3,R3,R3,R3);colnames(cov2006) = c('ID','Ag','Dev','Wetl','Forst')
cov2006$YEAR = rep(2006:2010,each=dim(uq)[1])
cov2006$Station = uq$Station

cov2011 = rbind(R4,R4,R4);colnames(cov2011) = c('ID','Ag','Dev','Wetl','Forst')
cov2011$YEAR = rep(2011:2013,each=dim(uq)[1])
cov2011$Station = uq$Station

cov = rbind(cov1992,cov2001,cov2006,cov2011)

class(uq)
tempdat = join(uq@data,cov)

uq@data = tempdat



library(rvest)



