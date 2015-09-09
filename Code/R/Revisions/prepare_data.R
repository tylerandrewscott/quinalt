
##GENERATE DATA FOR MODELING

######### SETUP BASICS ##########

remote = TRUE
if(remote)
{setwd('/homes/tscott1/win/user/quinalt')}
if(!remote)
{setwd('//Users/TScott/Google Drive/quinalt/')}


First.Year = 1990
Last.Year = 2035


######### NEEDED PACKAGES #############

require(foreign)
require(plyr)
require(dplyr)
require(rgdal)
require(sp)
require(rgeos)
require(maptools)
require(ggplot2)
require(reshape2)
library(RcppRoll)
library(devtools)
library(RCurl)
library(gdata)
require(proj4)
library(lubridate)
require(RODBC)
require(gridExtra)
require(lattice)
require(splancs)
require(fields)
library(raster)
library(shapefiles)
library(raster)
library(rasterVis)  # raster visualisation
library(rWBclimate)
library(stargazer)
library(texreg)
library(xtable)
oregon.outline = readOGR(dsn='SpatialData/government_units',
                         layer = 'state_nrcs_a_or')
#source("http://www.math.ntnu.no/inla/givemeINLA.R")

####### MAKE YEAR.MONTH REFERENCE DF ###########
#make sequence for years
YEAR = data.frame(YEAR = seq(First.Year,Last.Year,1))
MONTH = data.frame(MONTH = month.name,MONTH.ABB = month.abb)
Year.Month = merge(YEAR,MONTH, type='full')

Year.Month$Month.Num = match(Year.Month$MONTH,month.name)
Year.Month$Year.Num = Year.Month$YEAR - First.Year + 1
Year.Month$Abs.Month = Year.Month$Month.Num  + (Year.Month$Year.Num-1) *12

###### MAKE HUC8 DATAFRAME ##########
localDir <- 'TempData'
if (!file.exists(localDir)) {
  dir.create(localDir)
}

url <- 'http://oregonexplorer.info/ExternalContent/SpatialDataForDownload/ORE_WBD_HUC8.zip'
file <- paste(localDir,basename(url),sep='/')
if (!file.exists(file)) {
  download.file(url, file)
  unzip(file,exdir=localDir)
}
# layerName is the name of the unzipped shapefile without file type extensions 
layerName <- "ORE_WBD_HUC8"  
# Read in the data
oregon.huc8 <- readOGR(dsn=localDir, layer=layerName) 
oregon.huc8@data$id = rownames(oregon.huc8@data)
oregon.huc8.df = as.data.frame(oregon.huc8)

#full merge to replicated huc8 by year and month
oregon.huc8.df = merge(oregon.huc8.df,Year.Month,type='full')
colnames(oregon.huc8.df)[which(colnames(oregon.huc8.df)=='HUC_8')] = 'HUC8'
oregon.huc8.df$uq.tid = paste(oregon.huc8.df$HUC8,oregon.huc8.df$Abs.Month,sep='_')

###### MAKE OREGON WC DATAFRAME ##########
localDir <- 'TempData'
if (!file.exists(localDir)) {
  dir.create(localDir)
}

url <- 'http://oe.oregonexplorer.info/ExternalContent/SpatialDataforDownload/Oregon_Watershed_Councils_2014.zip'
file <- paste(localDir,basename(url),sep='/')
if (!file.exists(file)) {
  download.file(url, file)
  unzip(file,exdir=localDir)
}
# layerName is the name of the unzipped shapefile without file type extensions 
layerName <- "Oregon_Watershed_Councils_2014"  
# Read in the data
oregon.wc <- readOGR(dsn=localDir, layer=layerName) 
oregon.wc@data$id = rownames(oregon.wc@data)

oregon.wc@data$altName = gsub('WS Coalition','WSC',oregon.wc@data$altName)
oregon.wc@data$altName = gsub('Applegate River WC','Applegate WC',oregon.wc@data$altName)
oregon.wc@data$altName = gsub('Watershed','WS',oregon.wc@data$altName)
oregon.wc@data$altName = gsub('WS Association','WSA',oregon.wc@data$altName)
oregon.wc@data$altName = gsub('WS Partnership','WSP',oregon.wc@data$altName)
oregon.wc@data$altName = gsub('McKenzie River WC','McKenzie WC',oregon.wc@data$altName)
wc.in.huc8 = over(spTransform(sp::SpatialPoints(oregon.wc,CRS(proj4string(oregon.wc))),CRS=CRS(proj4string(oregon.huc8))),oregon.huc8)
wc.in.huc8$altName = oregon.wc@data$altName
wc.in.huc8 = wc.in.huc8 %>% dplyr::select(-id)
oregon.wc@data = join(oregon.wc@data,wc.in.huc8)
oregon.wc.df = as.data.frame(oregon.wc)

#full merge to replicated huc8 by year and month
oregon.wc.df = merge(oregon.wc.df,Year.Month,type='full')


###### MAKE OREGON SWCD DATAFRAME ##########

# Read in the data
oregon.swcd = readOGR(dsn='SpatialData/swcd', layer="oregon_swcd")
oregon.swcd@data$id = rownames(oregon.swcd@data)
oregon.swcd@data$SWCD_Name = gsub('Clastop','Clatsop',oregon.swcd@data$SWCD_Name)
oregon.swcd@data$SWCD_Name = gsub(' County','',oregon.swcd@data$SWCD_Name)
oregon.swcd@data$SWCD_Name[oregon.swcd@data$SWCD_Name =='Clackamas'] = 'Clackamas SWCD'
oregon.swcd@data$SWCD_Name = gsub('Gillam','Gilliam',oregon.swcd@data$SWCD_Name)
oregon.swcd@data$SWCD_Name[grep('Rock',oregon.swcd@data$SWCD_Name)] = 'Fort Rock/Silver Lake SWCD'

swcd.in.huc8 = over(spTransform(sp::SpatialPoints(oregon.swcd,CRS(proj4string(oregon.swcd))),CRS=CRS(proj4string(oregon.huc8))),oregon.huc8)
swcd.in.huc8$SWCD_Name = oregon.swcd@data$SWCD_Name
swcd.in.huc8 = swcd.in.huc8 %>% dplyr::select(-id)
oregon.swcd@data = join(oregon.swcd@data,swcd.in.huc8)
oregon.swcd.df = as.data.frame(oregon.swcd)
#full merge to replicated huc8 by year and month
oregon.swcd.df = merge(oregon.swcd.df,Year.Month,type='full')




###### ADD LANDCOVER DATA TO SPATIAL AREAS ############

#1992 nlcd
ag.huc8.1992 = read.dbf('LandUse_RasterData/or_agr_huc8_1992.dbf');colnames(ag.huc8.1992$dbf)[5] = 'ag.huc8'
wet.huc8.1992 = read.dbf('LandUse_RasterData/or_wetl_huc8_1992.dbf');colnames(wet.huc8.1992$dbf)[5] = 'wet.huc8'
forst.huc8.1992 = read.dbf('LandUse_RasterData/or_forst_huc8_1992.dbf');colnames(forst.huc8.1992$dbf)[5] = 'forst.huc8'
dev.huc8.1992 = read.dbf('LandUse_RasterData/or_dev_huc8_1992.dbf');colnames(dev.huc8.1992$dbf)[5] = 'dev.huc8'

cover.1992 = join_all(list(ag.huc8.1992$dbf,wet.huc8.1992$dbf,forst.huc8.1992$dbf,dev.huc8.1992$dbf))
cover.1992$YEAR = 1992;cover.1992 = dplyr::select(cover.1992,-c(COUNT,AREA,ZONE_CODE))
cover.1990 = cover.1992; cover.1990$YEAR = 1990
cover.1991 = cover.1992; cover.1991$YEAR = 1991
cover.1993 = cover.1992; cover.1993$YEAR = 1993
cover.1994 = cover.1992; cover.1994$YEAR = 1994
cover.1995 = cover.1992; cover.1995$YEAR = 1995
cover.1996 = cover.1992; cover.1996$YEAR = 1996
cover.1997 = cover.1992; cover.1997$YEAR = 1997
cover.1998 = cover.1992; cover.1998$YEAR = 1998
cover.1999 = cover.1992; cover.1999$YEAR = 1999
cover.2000 = cover.1992; cover.2000$YEAR = 2000


#2001 nlcd
crop.huc8.2001 = read.dbf('LandUse_RasterData/crop_huc8_2001.dbf');colnames(crop.huc8.2001$dbf)[5] = 'crop.huc8'
past.huc8.2001 = read.dbf('LandUse_RasterData/past_huc8_2001.dbf');colnames(past.huc8.2001$dbf)[5] = 'past.huc8'
wet.huc8.2001 = read.dbf('LandUse_RasterData/wetl_huc8_2001.dbf');colnames(wet.huc8.2001$dbf)[5] = 'wet.huc8'
forst.huc8.2001 = read.dbf('LandUse_RasterData/forst_huc8_2001.dbf');colnames(forst.huc8.2001$dbf)[5] = 'forst.huc8'
dev.huc8.2001 = read.dbf('LandUse_RasterData/dev_huc8_2001.dbf');colnames(dev.huc8.2001$dbf)[5] = 'dev.huc8'
ag.huc8.2001 = crop.huc8.2001
ag.huc8.2001$dbf$ag.huc8 = crop.huc8.2001$dbf$crop.huc8+
  past.huc8.2001$dbf$past.huc8[match(crop.huc8.2001$dbf$HUC8,past.huc8.2001$dbf$HUC8)]


cover.2001 = join_all(list(ag.huc8.2001$dbf,wet.huc8.2001$dbf,forst.huc8.2001$dbf,dev.huc8.2001$dbf))
cover.2001$YEAR = 2001
cover.2001 = dplyr::select(cover.2001,-c(COUNT,AREA,ZONE_CODE,crop.huc8))
cover.2002 = cover.2001; cover.2002$YEAR = 2002
cover.2003 = cover.2001; cover.2003$YEAR = 2003
cover.2004 = cover.2001; cover.2004$YEAR = 2004
cover.2005 = cover.2001; cover.2005$YEAR = 2005

#2006 nlcd
crop.huc8.2006 = read.dbf('LandUse_RasterData/crop_huc8_2006.dbf');colnames(crop.huc8.2006$dbf)[5] = 'crop.huc8'
past.huc8.2006 = read.dbf('LandUse_RasterData/past_huc8_2006.dbf');colnames(past.huc8.2006$dbf)[5] = 'past.huc8'
wet.huc8.2006 = read.dbf('LandUse_RasterData/wetl_huc8_2006.dbf');colnames(wet.huc8.2006$dbf)[5] = 'wet.huc8'
forst.huc8.2006 = read.dbf('LandUse_RasterData/forst_huc8_2006.dbf');colnames(forst.huc8.2006$dbf)[5] = 'forst.huc8'
dev.huc8.2006 = read.dbf('LandUse_RasterData/dev_huc8_2006.dbf');colnames(dev.huc8.2006$dbf)[5] = 'dev.huc8'
ag.huc8.2006 = crop.huc8.2006
ag.huc8.2006$dbf$ag.huc8 = crop.huc8.2006$dbf$crop.huc8+
  past.huc8.2006$dbf$past.huc8[match(crop.huc8.2006$dbf$HUC8,past.huc8.2006$dbf$HUC8)]


cover.2006 = join_all(list(ag.huc8.2006$dbf,wet.huc8.2006$dbf,forst.huc8.2006$dbf,dev.huc8.2006$dbf))
cover.2006$YEAR = 2006
cover.2006 = dplyr::select(cover.2006,-c(COUNT,AREA,ZONE_CODE,crop.huc8))
cover.2007 = cover.2006; cover.2007$YEAR = 2007
cover.2008 = cover.2006; cover.2008$YEAR = 2008
cover.2009 = cover.2006; cover.2009$YEAR = 2009
cover.2010 = cover.2006; cover.2010$YEAR = 2010

#2011 nlcd
crop.huc8.2011 = read.dbf('LandUse_RasterData/crop_huc8_2011.dbf');colnames(crop.huc8.2011$dbf)[5] = 'crop.huc8'
past.huc8.2011 = read.dbf('LandUse_RasterData/past_huc8_2011.dbf');colnames(past.huc8.2011$dbf)[5] = 'past.huc8'
wet.huc8.2011 = read.dbf('LandUse_RasterData/wetl_huc8_2011.dbf');colnames(wet.huc8.2011$dbf)[5] = 'wet.huc8'
forst.huc8.2011 = read.dbf('LandUse_RasterData/forst_huc8_2011.dbf');colnames(forst.huc8.2011$dbf)[5] = 'forst.huc8'
dev.huc8.2011 = read.dbf('LandUse_RasterData/dev_huc8_2011.dbf');colnames(dev.huc8.2011$dbf)[5] = 'dev.huc8'
ag.huc8.2011 = crop.huc8.2011
ag.huc8.2011$dbf$ag.huc8 = crop.huc8.2011$dbf$crop.huc8+past.huc8.2011$dbf$past.huc8[match(crop.huc8.2011$dbf$HUC8,past.huc8.2011$dbf$HUC8)]

cover.2011 = join_all(list(ag.huc8.2011$dbf,wet.huc8.2011$dbf,forst.huc8.2011$dbf,dev.huc8.2011$dbf))


cover.2011$YEAR = 2011
cover.2011 = dplyr::select(cover.2011,-c(COUNT,AREA,ZONE_CODE,crop.huc8))
cover.2012 = cover.2011; cover.2012$YEAR = 2012
cover.2013 = cover.2011; cover.2013$YEAR = 2013
cover.2014 = cover.2011; cover.2014$YEAR = 2014
cover.2015 = cover.2011; cover.2015$YEAR = 2015
cover.2016 = cover.2011; cover.2016$YEAR = 2016

land.cover.huc8 = join_all(list(cover.1990,cover.1991,cover.1992,cover.1993,cover.1994,
                                cover.1995,cover.1996,cover.1997,cover.1998,cover.1999,
                                cover.2000,cover.2001,cover.2002,cover.2003,cover.2004,
                                cover.2005,cover.2006,cover.2007,cover.2008,cover.2009,
                                cover.2010,cover.2011,cover.2012,cover.2013,cover.2014,cover.2015,cover.2016),
                           type='full')

names(oregon.huc8.df)[1] = 'HUC8'

oregon.huc8.df = join(oregon.huc8.df,land.cover.huc8,type='left')



######### MAKE OBS STATION DATAFRAME ##########


wq.dat = read.csv('Input/Scott_OWQI_1980_2013.csv',header=T,skip=1)
wq.dat = wq.dat[!duplicated(wq.dat),]

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
oregon.huc8.reference = join(oregon.huc8.points, oregon.huc8@data, by="id")
#create oregon coastline dataframe
oregon.coast.df = subset(oregon.huc8.reference,long<(-124)&lat>=min(oregon.df[,2]))

mat.dists <- spDists(uq.coords, as.matrix(oregon.coast.df[,1:2]), longlat=TRUE)
uq@data$seaDist <-  apply(mat.dists, 1, min)

rm(mat.dists)
rm(oregon.coast.df)
rm(oregon.huc8.reference)
rm(oregon.df)
rm(oregon)


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

R1.ag.v = raster::extract(R1.ag,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R1.dev.v = raster::extract(R1.dev,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R1.wet.v = raster::extract(R1.wet,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R1.forst.v = raster::extract(R1.forst,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)

R2.crop.v = raster::extract(R2.crop,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R2.past.v = raster::extract(R2.past,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R2.dev.v = raster::extract(R2.dev,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R2.wet.v = raster::extract(R2.wet,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R2.forst.v = raster::extract(R2.forst,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R2.ag.v = data.frame(ID = R2.crop.v$ID,tf_ag_2001 = R2.crop.v$tf_crop_2001+R2.past.v$tf_past_2001)

R3.crop.v = raster::extract(R3.crop,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R3.past.v = raster::extract(R3.past,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R3.dev.v = raster::extract(R3.dev,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R3.wet.v = raster::extract(R3.wet,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R3.forst.v = raster::extract(R3.forst,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R3.ag.v = data.frame(ID = R3.crop.v$ID,tf_ag_2006 = R3.crop.v$tf_crop_2006+R3.past.v$tf_past_2006)

R4.crop.v = raster::extract(R4.crop,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R4.past.v = raster::extract(R4.past,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R4.dev.v = raster::extract(R4.dev,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R4.wet.v = raster::extract(R4.wet,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
R4.forst.v = raster::extract(R4.forst,uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE)
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

all.params.spdf@data = join(all.params.spdf@data,uq@data[,c('Station','elevation','seaDist')])

all.params.spdf@data = join(all.params.spdf@data,cov)


which.huc8 = over(spTransform(all.params.spdf,CRS(proj4string(oregon.huc8))),oregon.huc8)

all.params.spdf@data$HUC8 = which.huc8$HUC8

all.params.spdf@data = join(all.params.spdf@data,Year.Month)


all.params.spdf@data$uq.tid = paste(all.params.spdf@data$HUC8, all.params.spdf@data$Abs.Month,sep='_')



#write.csv(all.params.spdf@data,'Input/site_obs_data.csv')

####### ADD GRANT DATA ##########

proj.info = read.csv('Input/owri_project_info.csv')
proj.info = proj.info %>% filter(StartYear!=0,CompleteYear!=0,StartMonth!=0,CompleteMonth!=0)

convertCurrency <- function(currency) {
  currency1 <- sub('$','',as.character(currency),fixed=TRUE)
  currency2 <- as.numeric(gsub('\\,','',as.character(currency1))) 
  currency2
}
proj.info$TotalCash <- convertCurrency(proj.info$TotalCash)
proj.info$TotalInKind <- convertCurrency(proj.info$TotalInKind)
proj.info$TotalBoth <- proj.info$TotalCash + proj.info$TotalInKind

proj.info$Start.Abs = Year.Month$Abs.Month[match(paste(proj.info$StartMonth,proj.info$StartYear),paste(Year.Month$Month.Num,Year.Month$YEAR))]
proj.info$Complete.Abs = Year.Month$Abs.Month[match(paste(proj.info$CompleteMonth,proj.info$CompleteYear),paste(Year.Month$Month.Num,Year.Month$YEAR))]


proj.info$Project.Number = proj.info$drvdOwebNum
proj.info$HUC8 = proj.info$drvdHUC4thField
proj.info = proj.info %>% filter(StartYear!=0,CompleteYear!=0,StartMonth!=0,CompleteMonth!=0)

### proj.info = join(proj.info,all.points.uq.df,type='left')
proj.info$YEAR = proj.info$CompleteYear
proj.info$Month.Num = proj.info$CompleteMonth

proj.info = join(proj.info,Year.Month)
proj.info$uq.tid = paste(proj.info$HUC8,proj.info$Abs.Month,sep='_')



#   proj.partners= read.csv('Input/owri_project_participants.csv')
#   public.list = c('county','city','state agency or jobs program (or ownership), state universities',
#                   'Soil & Water Conservation District',
#                   'federal agency or jobs program (or ownership)',
#                   'Extension Service (e.g. OSU Extension)')
#   wc.list = 'watershed council'


#   proj.partners$Category = ifelse(proj.partners$ParticipantType %in% public.list,'Public',
#                                   ifelse(proj.partners$ParticipantType %in% wc.list,'WC','Other'))
#   temp  = (table(proj.partners$PROJNUM,proj.partners$Category))
#   t1 = data.frame((ifelse(temp[,3] > 0,'WC',ifelse(temp[,2]>0,'Public','Other'))))
#   colnames(t1) = 'Lead.Type'
#   t1$PROJNUM = rownames(t1)
#   
#   t2 = data.frame(rowSums(temp))
#   colnames(t2) = 'Num.Partners'
#   t2$PROJNUM = rownames(t2)
#   t3 = join(t1,t2)

#  proj.info = join(proj.info,t3)


#binary true/false: OWEB project?
proj.info$OWRI.OWEB.Grant = ifelse(proj.info$drvdOwebNum=='','NOT_OWEB_OWRI','OWEB_OWRI')

#proj.info$activity_t = tolower(proj.info$activity_t)
proj.info$drvdProjDesc = tolower(proj.info$drvdProjDesc)

###Create true/false for project about water quality

proj.info$about_wq = 'not_wq'
proj.info$about_wq[
  unique(c(grep('restoration',proj.info$drvdProjDesc),
           grep('riparian',proj.info$drvdProjDesc),
           grep('wetland',proj.info$drvdProjDesc),
           grep('planting',proj.info$drvdProjDesc),
           grep('stabilization',proj.info$drvdProjDesc),
           grep('fencing',proj.info$drvdProjDesc),
           grep('livestock',proj.info$drvdProjDesc),
           grep('grazing',proj.info$drvdProjDesc),
           grep('stabilize',proj.info$drvdProjDesc),
           grep('manure',proj.info$drvdProjDesc),
           grep('effluent',proj.info$drvdProjDesc),
           grep('seed',proj.info$drvdProjDesc),
           grep('sediment',proj.info$drvdProjDesc),
           grep('silt',proj.info$drvdProjDesc),
           grep('sidecast',proj.info$drvdProjDesc),
           grep('setback',proj.info$drvdProjDesc),
           grep('irrigation',proj.info$drvdProjDesc),
           grep('erosion',proj.info$drvdProjDesc),
           grep('tailings',proj.info$drvdProjDesc),
           grep('nutrient',proj.info$drvdProjDesc),
           grep('water quality',proj.info$drvdProjDesc),
           grep('air diffuser',proj.info$drvdProjDesc),
           grep('waterbar',proj.info$drvdProjDesc)
  ))
  ] = 'wq'


temp.huc8 = oregon.huc8.df
temp = proj.info %>% dplyr::group_by(uq.tid,about_wq,OWRI.OWEB.Grant) %>% 
  dplyr::summarise_each(funs(sum),TotalCash)
tt = melt(temp,id.vars=c('uq.tid','about_wq','OWRI.OWEB.Grant'))
tt$var.id = paste(tt$OWRI.OWEB.Grant,tt$about_wq,tt$variable,sep='.')
temp.huc8[,unique(tt$var.id)] = NA


#place subset values into proper column
for (i in 1:ncol(temp.huc8))
{
  if (colnames(temp.huc8)[i] %in% unique(tt$var.id))
  {
    t1 = tt[tt$var.id == colnames(temp.huc8)[i],]
    temp.huc8[colnames(temp.huc8)[i]] = t1$value[match(temp.huc8$uq.tid,t1$uq.tid)]
  }
}

temp.huc8[,grep('Total',colnames(temp.huc8))][is.na(temp.huc8[,grep('Total',colnames(temp.huc8))])] = 0

huc8_data = temp.huc8

####### ADD PRECIP VARIABLE ###########

oregon.outline = readOGR(dsn='SpatialData/government_units',
                         layer = 'state_nrcs_a_or')

#setwd('/homes/tscott1/win/user/quinalt/')
# create a list of .bil files that exist in the wd
files <- list.files('SpatialData/precip_rasters/',pattern='\\.bil$')
files = files[nchar(files) != min(nchar(files))]

# 
# for each of vars, create raster object for each tile and merge
# (this is a bit messy, but all I could think of for now...)
# grids will be a list of rasters, each of which is the merged tiles for a BC var.

if(remote)
{setwd('/homes/tscott1/win/user/quinalt/SpatialData/precip_rasters/')}
if(!remote)
{setwd('//Users/TScott/Google Drive/quinalt/SpatialData/precip_rasters/')}


huc8_data$pull_raster = paste0(huc8_data$YEAR,ifelse(nchar(huc8_data$Month.Num)==2,huc8_data$Month.Num,paste0(0,huc8_data$Month.Num)))
huc8_data = huc8_data[huc8_data$HUC8!='17070106',]
huc8_data$monthly.precip.median = NA


which.file = sapply(huc8_data$pull_raster,FUN = grep,x=files)
which.file = as.character(which.file)
which.file = ifelse(which.file=='integer(0)',NA,which.file)
which.file = unlist(which.file)
print.list = seq(0,100000,1000)

for (i in 1:nrow(huc8_data))
{
  if (!is.na(which.file[i]))
  {
    huc8_data$monthly.precip.median[i] = raster::extract(crop(raster(files[as.numeric(which.file[i])]),oregon.outline),
                                                         oregon.huc8[as.character(oregon.huc8@data$HUC8)==
                                                                       as.character(huc8_data$HUC8[i]),],fun=median,na.rm=TRUE)[1]
  }
  if(i %in% print.list){print(i)}
}


# 
# select1990 = files[grep('1990',files)]
# grids1990<- sapply(select1990 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select1990
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select1991 = files[grep('1991',files)]
# grids1991<- sapply(select1991 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select1991
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select1992 = files[grep('1992',files)]
# grids1992<- sapply(select1992 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select1992
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select1993 = files[grep('1993',files)]
# grids1993<- sapply(select1993 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select1993
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select1994 = files[grep('1994',files)]
# grids1994<- sapply(select1994 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select1994
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select1995 = files[grep('1995',files)]
# grids1995<- sapply(select1995 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select1995
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select1996 = files[grep('1996',files)]
# grids1996<- sapply(select1996 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select1996
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select1997 = files[grep('1997',files)]
# grids1997<- sapply(select1997 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select1997
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select1998 = files[grep('1998',files)]
# grids1998<- sapply(select1998 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select1998
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select1999 = files[grep('1999',files)]
# grids1999<- sapply(select1999 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select1999
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select2000 = files[grep('2000',files)]
# grids2000<- sapply(select2000 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select2000
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select2001 = files[grep('2001',files)]
# grids2001<- sapply(select2001 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select2001
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select2002 = files[grep('2002',files)]
# grids2002<- sapply(select2002 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select2002
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select2003 = files[grep('2003',files)]
# grids2003<- sapply(select2003 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select2003
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select2004 = files[grep('2004',files)]
# grids2004<- sapply(select2004 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select2004
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select2005 = files[grep('2005',files)]
# grids2005<- sapply(select2005 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select2005
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select2006 = files[grep('2006',files)]
# grids2006<- sapply(select2006 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select2006
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select2007 = files[grep('2007',files)]
# grids2007<- sapply(select2007 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select2007
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select2008 = files[grep('2008',files)]
# grids2008<- sapply(select2008 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select2008
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# 
# select2009 = files[grep('2009',files)]
# grids2009<- sapply(select2009 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select2009
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select2010 = files[grep('2010',files)]
# grids2010<- sapply(select2010 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select2010
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# 
# select2011 = files[grep('2011',files)]
# grids2011<- sapply(select2011 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select2011
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select2012 = files[grep('2012',files)]
# grids2012<- sapply(select2012 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select2012
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# 
# select2013 = files[grep('2013',files)]
# grids2013<- sapply(select2013 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select2013
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# select2014 = files[grep('2014',files)]
# grids2014<- sapply(select2014 , function(x) {
#   #patt <- paste('precip', x, '_', sep='')
#   tiles <- select2014
#   merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
#                                                            sep='"')), ')', sep='')))
# })
# 
# 
# all.grids = c(grids1990,grids1991,grids1992,grids1993,grids1994,grids1995,grids1996,grids1997,grids1998,grids1999,
#               grids2000,grids2001,grids2002,grids2003,grids2004,grids2005,grids2006,grids2007,grids2008,grids2009,
#               grids2010,grids2011,grids2012,grids2013,grids2014)
# 
# 
# # give the list elements names
# which.month = gsub('_bil','',
#                    gsub('.bil','',gsub('PRISM_ppt_stable_4kmM2_[0123456789]{4}','',files)))
# 
# which.year = gsub('[0123456789]{2}$','',gsub('_bil','',
#                                              gsub('.bil','',gsub('PRISM_ppt_stable_4kmM2_','',files))))
# 
# which.year.month = paste(which.year,which.month,sep='_')
# names(all.grids) = which.year.month
# 
# #combine all list elements into a stack
# s <- stack(all.grids)
# s.crop <- crop(s, oregon.outline)
# 
# all.params.spdf@data$monthly.precip = NA
# precip.data = data.frame(NULL)
# empty = list(NULL)
# for (i in 1:dim(s.crop)[3])
# {
#   empty[[i]] = raster::extract(s.crop[[i]],all.params.spdf,method='bilinear')
# }
# precip.list = as.data.frame(empty)
# names(precip.list) = names(s.crop)
# 
# empty = list(NULL)
# for (i in 1:dim(s.crop)[3])
# {
#   empty[[i]] = raster::extract(s.crop[[i]],oregon.huc8,fun=median,na.rm=T)
# }
# 
# names(empty) = names(s.crop)
# 
# 
# which.grab =  match(paste0('X',all.params.spdf@data$YEAR,'_',
#                            ifelse(all.params.spdf@data$Month.Num<10,paste0(0,all.params.spdf@data$Month.Num),
#                                   all.params.spdf@data$Month.Num)),names(precip.list))
# 
# all.params.spdf@data$monthly.precip.site = NA
# 
# for (i in 1:nrow(all.params.spdf@data))
# {
#   all.params.spdf@data$monthly.precip.site[i] = 
#     precip.list[i,match(paste0('X',all.params.spdf@data$YEAR[i],'_',
#                                ifelse(all.params.spdf@data$Month.Num[i]<10,paste0(0,all.params.spdf@data$Month.Num[i]),
#                                       all.params.spdf@data$Month.Num[i])),names(precip.list))]
# }


######### ADD COUNTY POP ##########
 if(remote)
 {setwd('/homes/tscott1/win/user/quinalt')}
 if(!remote)
 {setwd('//Users/TScott/Google Drive/quinalt')}
# 
# county.pop = read.csv('Input/oregon_county_populations.csv')
# county.pop  = filter(county.pop, County != '')
# county.pop$County = gsub(' County','',county.pop$County)
# 
# county.pop$G1980 = round(100 * (county.pop[,3] - county.pop[,2])/county.pop[,2] / 10,1)
# county.pop$G1990 = round(100 * (county.pop[,4] - county.pop[,3])/county.pop[,3] / 10,1)
# county.pop$G2000 = round(100 * (county.pop[,5] - county.pop[,4])/county.pop[,4] / 10,1)
# county.pop$County = toupper(county.pop$County)
# recent.pop = read.csv('Input/oregon_county_populations2010_2013.csv')
# county.pop$G2010 = gsub('%','',recent.pop$Percent.Change.2010.13)[match(county.pop$County, recent.pop$County)]
# county.pop$G2010 = round(as.numeric(county.pop$G2010)/3,1)
# 
# cpop = melt(county.pop,id.vars = 'County')
# cpop = cpop[grep('Year_',cpop$variable,invert=T),]
# cpop$YEAR = as.numeric(gsub('G','',cpop$variable))
# 
# all.params.spdf@data$county.pop.growthrate = NA
# 
# for (i in 1:nrow(all.params.spdf@data))
# {
#   t1 = cpop[cpop$YEAR < all.params.spdf@data$YEAR[i]&cpop$County==all.params.spdf@data$CountyName[i],]
#   all.params.spdf@data$county.pop.growthrate[i] = t1$value[t1$YEAR == min(t1$YEAR)]
# }
# 


all.params.spdf@data = join(all.params.spdf@data,dplyr::select(huc8_data,HUC8,Abs.Month,
                                                        uq.tid))


########### CLEAN OWEB WC GRANTS ############

oweb.grants = readOGR(dsn="SpatialData/OWEB_grants", layer="OWEB_Grants_8-4-2014")
oweb.grants@data$id = rownames(oweb.grants@data)
or.huc8 = readOGR(dsn="SpatialData/hydrologic_units", layer="wbdhu8_a_or")
which.huc8 = over(oweb.grants,spTransform(or.huc8,CRS(proj4string(oweb.grants))))
oweb.grants@data = cbind(oweb.grants@data,which.huc8)

oweb.grants@data = oweb.grants@data %>% dplyr::rename(Project.ID = gr_project,Project.Number = applicatio)
grant.gis = oweb.grants@data

oweb.cancelled = read.csv('Input/oweb_cancelled.csv')
oweb.notawarded = read.csv('Input/oweb_notawarded.csv')
oweb.withdrawn =  read.csv('Input/oweb_withdrawn.csv')
oweb.monitoring = read.csv('Input/oweb_monitoring.csv')
oweb.completed = read.csv('Input/oweb_completed.csv')
oweb.funded =  read.csv('Input/oweb_funded.csv')
oweb.pending = read.csv('Input/oweb_pending.csv')
oweb.open = read.csv('Input/oweb_open.csv')
oweb.ineligible = read.csv('Input/oweb_ineligible.csv')



oweb.all = join_all(list(oweb.completed,oweb.cancelled,oweb.notawarded,oweb.monitoring,oweb.funded,
                         oweb.open,oweb.ineligible,oweb.withdrawn,oweb.pending),type='full')

oweb.all[oweb.all==''] = NA

oweb.all = oweb.all %>% filter(!is.na(Grantee)) %>% filter(Project.Amount!=0) %>% filter(Region!='SW') %>% filter(Project.Status!='Cancelled') %>%
  filter(Project.Status!='Withdrawn') %>% filter(Project.Status != 'Ineligible') %>% filter(Project.Status !='Not Awarded')

oweb.all =  join(oweb.all,grant.gis)

# 
# not.mapped = read.csv('Input/Grants_not_Mapped_August_2014.csv')
# 
# not.mapped = not.mapped %>% dplyr::rename(Project.Number = APPLICATION.NUMBER,
#                                           Project.ID = GRANT.PROJECT.ID,
#                                           Project.Type = Grant.Type)

oweb.all = oweb.all[!is.na(oweb.all$Project.End.Date),]

oweb.all = oweb.all %>% filter(Project.Status != 'Cancelled') 

oweb.all$End.Date = as.Date(oweb.all$Project.End.Date, format = "%m/%d/%y")
oweb.all$Start.Date = as.Date(oweb.all$Project.Start.Date, format = "%m/%d/%y")

#oweb.all = oweb.all[year(oweb.all$End.Date)<=2013,]

oweb.all$START.MONTH = month(oweb.all$Start.Date)
oweb.all$END.MONTH = month(oweb.all$End.Date)
oweb.all$END.YEAR = (year(oweb.all$End.Date))
oweb.all$START.YEAR = (year(oweb.all$Start.Date))

oweb.all$Abs.Month = Year.Month$Abs.Month[match(paste(oweb.all$START.YEAR,oweb.all$START.MONTH),paste(Year.Month$YEAR,Year.Month$MONTH))]
public.values = c('City','Federal Agency','State Agency','County','Tribe')

oweb.all$which.group = NA
oweb.all$which.group[grep('SWCD',oweb.all$Grantee)] = 'SWCD'
oweb.all$which.group[grep('Watershed Council',oweb.all$Grantee.Type)] = 'WC'
oweb.all$which.group[oweb.all$Grantee.Type %in%  public.values] = 'Public'
oweb.all$which.group[is.na(oweb.all$which.group)] = 'Other'

oweb.restoration = oweb.all[oweb.all$Project.Type=='Restoration',]
oweb.all$Project.Type = as.character(oweb.all$Project.Type)

oweb.all$Grantee.Type[intersect(which(oweb.all$Project.Type=='Council Support'&oweb.all$Grantee.Type!='Watershed Council'),
                                intersect(grep('coordinator|Coordinator',oweb.all$Project.Name),grep('Fund|fund',oweb.all$Project.Summary)))] = 'Watershed Council'

oweb.all$Grantee.Type[intersect(which(oweb.all$Project.Type=='Council Support'&oweb.all$Grantee.Type!='Watershed Council'),
                                intersect(grep('Support|support',oweb.all$Project.Summary),grep('Council|council',oweb.all$Project.Summary)))] = 'Watershed Council'

oweb.all$Grantee.Type[intersect(which(oweb.all$Project.Type=='Council Support'&oweb.all$Grantee.Type!='Watershed Council'),
                                intersect(grep('Support|support',oweb.all$Project.Summary),grep('coordinator|Coordinator',oweb.all$Project.Summary)))] = 'Watershed Council'

oweb.all$Grantee.Type[intersect(which(oweb.all$Project.Type=='Council Support'&oweb.all$Grantee.Type!='Watershed Council'),
                                intersect(grep('Council|council',oweb.all$Project.Summary),grep('coordinator|Coordinator',oweb.all$Project.Summary)))] = 'Watershed Council'

oweb.all$Grantee.Type[intersect(which(oweb.all$Project.Type=='Council Support'&oweb.all$Grantee.Type!='Watershed Council'),
                                grep('WSC',oweb.all$Project.Summary))] = 'Watershed Council'

oweb.all$Grantee.Type[intersect(which(oweb.all$Project.Type=='Council Support'&oweb.all$Grantee.Type!='Watershed Council'),
                                grep('WSC|Council Coordinator',oweb.all$Project.Name))] = 'Watershed Council'

oweb.all$Grantee.Type[intersect(which(oweb.all$Project.Type=='Council Support'&oweb.all$Grantee.Type!='Watershed Council'),
                                grep('WS Coord|Council for WS',oweb.all$Project.Name))] = 'Watershed Council'

oweb.all$Project.Type[intersect(which(oweb.all$Project.Type=='Council Support'&oweb.all$Grantee.Type!='Watershed Council'),
                                grep('Engineer',oweb.all$Project.Name))] = 'Tech'

oweb.all$Project.Type[intersect(which(oweb.all$Project.Type=='Council Support'&oweb.all$Grantee.Type!='Watershed Council'),
                                grep('Educat',oweb.all$Project.Name))] = 'Education'

oweb.all$Project.Type[oweb.all$Project.Type=='Council Support'] = 'Capacity'
oweb.all$Project.Type[oweb.all$Project.Type=='SWCD'] = 'Capacity'
oweb.all$Project.Type[oweb.all$Project.Type=='Education'] = 'Outreach'
oweb.all$Project.Type[oweb.all$Project.Type=='Assessment'] = 'Tech'
oweb.all$Project.Type[oweb.all$Project.Type=='Monitoring'] = 'Tech'
oweb.all$Project.Type[oweb.all$Project.Type=='Technical Assistance'] = 'Tech'


oweb.all$sabs = Year.Month$Abs.Month[match(paste(oweb.all$START.YEAR,oweb.all$START.MONTH,sep='_'),paste(Year.Month$YEAR,Year.Month$Month.Num,sep='_'))]
oweb.all$eabs = Year.Month$Abs.Month[match(paste(oweb.all$END.YEAR,oweb.all$END.MONTH,sep='_'),paste(Year.Month$YEAR,Year.Month$Month.Num,sep='_'))]
oweb.all$abs.length = abs(oweb.all$eabs-oweb.all$sabs)+1
oweb.all$abs.length = ifelse(oweb.all$abs.length==0,1,oweb.all$abs.length)

oweb.all = filter(oweb.all,Project.Type != 'Acquisition',Project.Type!='Data Development',Project.Type !='Law Enforcement',
                  Project.Type !='Education',Project.Type !='SWCD',
                  Project.Type !='Holding',Project.Type != 'OWEB Administration')
oweb.all$Project.Type = as.character(oweb.all$Project.Type)


temp = (oweb.all %>% 
          filter(which.group == 'WC' |which.group=='SWCD') %>% group_by(Project.Type,which.group) %>%
          summarise_each(funs(nobs,mean),Project.Amount,abs.length))[,-3]
temp[,1] = ifelse(duplicated(data.frame(temp)[,1]),'',data.frame(temp)[,1])
names(temp) = c('Project','Grantee','N','Average cost','Average length (months)')


stargazer(data.frame(temp),summary=F,rownames = F,no.space = T,digits=0,type='latex',
          out='/homes/tscott1/win/user/quinalt/JPART_Submission/Version2/grantsummarytable.tex',
          title = 'OWEB grant summary statistics (1997-2013)',
          label='table:grantsummary')

# nm = is.mapped[is.na(test$HUC8),]
# mapped  = is.mapped[!is.na(test$HUC8),]
# nm$HUC8 = mapped$HUC8[match(nm$Grantee,mapped$Grantee)]

# counties = readOGR(dsn="SpatialData/government_units", layer="county_nrcs_a_or")
# counties@data$id = rownames(counties@data)
# counties@data$center_long = coordinates(counties)[,1]
# counties@data$center_lat = coordinates(counties)[,2]
# or.county = SpatialPointsDataFrame(coords = coordinates(counties),data = counties@data,proj4string = CRS(proj4string(counties)))
# which.place = over(or.county,spTransform(oregon.huc8,CRS=CRS(proj4string(counties))))
# or.county@data = cbind(or.county@data,which.place)


oweb.all$Grantee = gsub('Watershed Council','WC',oweb.all$Grantee)
oweb.all$Grantee[grep('Applegate',oweb.all$Grantee)] = 'Applegate WC'
oweb.all$Grantee = gsub('Model WS Program','Model WS',oweb.all$Grantee)
oweb.all$Grantee = gsub('Watershed Association','WSA',oweb.all$Grantee)
oweb.all$Grantee = gsub('Harney WC','Harney County WC',oweb.all$Grantee)
oweb.all$Grantee = gsub('Watersheds Council','WC',oweb.all$Grantee)
oweb.all$Grantee = gsub('Watershed Partnership','WSP',oweb.all$Grantee)
oweb.all$Grantee[grep('Lake County',oweb.all$Grantee)] = 'Lake County WC'
oweb.all$Grantee = gsub('Molalla River Watch Inc','Molalla River Watch',oweb.all$Grantee)
oweb.all$Grantee[grep('Walla Walla Basin WC',oweb.all$Grantee)] = 'Walla Walla WC'
oweb.all$Grantee[grep('Wasco Area WCs',oweb.all$Grantee)] = 'Wasco County WC'
oweb.all$Grantee[grep('Upper South Fork John Day WC',oweb.all$Grantee)] = 'Upper South Fork John Day River WC'
oweb.all$Grantee[grep('Upper Chewaucan WC',oweb.all$Grantee)] = 'Upper Chewaucan River WC'
oweb.all$Grantee[grep('Silver Lake Community WC',oweb.all$Grantee)] = 'Silver Lake WC'
oweb.all$Grantee[grep('Tualatin River WC',oweb.all$Grantee)] = 'Tualatin WC'
oweb.all$Grantee[grep('Salmon Drift Cr WC',oweb.all$Grantee)] = 'Salmon-Drift WC'
oweb.all$Grantee[grep('Skipanon River WC',oweb.all$Grantee)] = 'Skipanon WC'
oweb.all$Grantee[grep('Sherman County Area WC',oweb.all$Grantee)] = 'Sherman County WC'
oweb.all$Grantee[grep("Grass Valley WC" ,oweb.all$Grantee)] = "Sherman County WC"
oweb.all$Grantee[grep("Pedee Creek WC" ,oweb.all$Grantee)] = "Luckiamute WC" 
oweb.all$Grantee[grep("Nestucca-Neskowin WC" ,oweb.all$Grantee)] = "Nestucca, Neskowin and Sand Lake WC"
oweb.all$Grantee[grep("Lost Creek Watershed Group" ,oweb.all$Grantee)] = "Middle Fork Willamette WC"
oweb.all$Grantee[grep("Middle Rogue WC" ,oweb.all$Grantee)] = "Stream Restoration Alliance of the Middle Rogue"
oweb.all$Grantee[grep("Rogue River WC" ,oweb.all$Grantee)] = "Upper Rogue WC"
oweb.all$Grantee[grep("Goose Lake Fishes Working Group" ,oweb.all$Grantee)] = "Lake County WC"
oweb.all$Grantee[grep("Warner Valley WC" ,oweb.all$Grantee)] = "Lake County WC"
oweb.all$Grantee[grep('The Dalles Area WC',oweb.all$Grantee)] = 'Wasco County WC'
oweb.all$Grantee[grep('Yamhill Basin WC',oweb.all$Grantee)] = 'Greater Yamhill WC'
oweb.all$Grantee[grep('Alsea WC',oweb.all$Grantee)] = 'Alsea Basin WC'
oweb.all$Grantee[grep('Mid John Day-Bridge Creek WC',oweb.all$Grantee)] = 'Mid John Day Bridge Creek WC'
oweb.all$Grantee[grep('Wheeler County WS Groups',oweb.all$Grantee)] = 'Mid John Day Bridge Creek WC'
oweb.all$Grantee[oweb.all$Grantee == 'Mid John Day WC'] = 'Mid John Day Bridge Creek WC'
oweb.all$Grantee[oweb.all$Grantee=='Bridge Creek WC'] = 'Mid John Day Bridge Creek WC'
oweb.all$Grantee[oweb.all$Grantee=='Middle Deschutes WS Councils'] = 'Middle Deschutes'
oweb.all$Grantee[oweb.all$Grantee=='Trout Creek WC'] = 'Middle Deschutes'
oweb.all$Grantee[oweb.all$Grantee=='Willow Creek WC'] = 'Middle Deschutes'
oweb.all$Grantee[oweb.all$Grantee=='Glenn/Gibson Creeks WC - Bierly'] = 'Glenn-Gibson WC'
oweb.all$Grantee[oweb.all$Grantee=='North Sherman WC'] = 'Sherman County WC'
oweb.all$Grantee[oweb.all$Grantee=='Mosier WC'] = 'Wasco County WC'
oweb.all$Grantee[oweb.all$Grantee=='Deep Creek WC'] = 'Clackamas River Basin Council'
oweb.all$Grantee[oweb.all$Grantee=='Evans Creek WC'] = 'Applegate WC'
oweb.all$Grantee[oweb.all$Grantee=='Mohawk WS Partnership'] = 'McKenzie WC'

replace = rbind(oweb.all[grep('Malheur-Owyhee',oweb.all$Grantee),],
                oweb.all[grep('Malheur-Owyhee',oweb.all$Grantee),],
                oweb.all[grep('Malheur-Owyhee',oweb.all$Grantee),])

replace$Grantee =
  c(rep('Malheur WC',nrow(oweb.all[grep('Malheur-Owyhee',oweb.all$Grantee),])),
    rep('Owyhee WC',nrow(oweb.all[grep('Malheur-Owyhee',oweb.all$Grantee),])),
    rep('Bully Creek WS Coalition',nrow(oweb.all[grep('Malheur-Owyhee',oweb.all$Grantee),])))
oweb.all =  rbind(oweb.all[grep('Malheur-Owyhee',oweb.all$Grantee,invert=T),],replace)


replace = rbind(oweb.all[grep("North Coast WS Assn",oweb.all$Grantee),],
                oweb.all[grep("North Coast WS Assn",oweb.all$Grantee),],
                oweb.all[grep("North Coast WS Assn",oweb.all$Grantee),],
                oweb.all[grep("North Coast WS Assn",oweb.all$Grantee),])
replace$Grantee =
  c(rep('Youngs Bay WC',nrow(oweb.all[grep('North Coast WS Assn',oweb.all$Grantee),])),
    rep("Skipanon WC",nrow(oweb.all[grep('North Coast WS Assn',oweb.all$Grantee),])),
    rep("Nicolai-Wickiup WC",nrow(oweb.all[grep('North Coast WS Assn',oweb.all$Grantee),])),
    rep("Ecola Creek WC",nrow(oweb.all[grep('North Coast WS Assn',oweb.all$Grantee),]))
  )
oweb.all =  rbind(oweb.all[grep('North Coast WS Assn',oweb.all$Grantee,invert=T),],replace)



replace = rbind(oweb.all[grep("Clatsop Coordinating Council",oweb.all$Grantee),],
                oweb.all[grep("Clatsop Coordinating Council",oweb.all$Grantee),],
                oweb.all[grep("Clatsop Coordinating Council",oweb.all$Grantee),],
                oweb.all[grep("Clatsop Coordinating Council",oweb.all$Grantee),])
replace$Grantee =
  c(rep('Youngs Bay WC',nrow(oweb.all[grep('Clatsop Coordinating Council',oweb.all$Grantee),])),
    rep("Skipanon WC",nrow(oweb.all[grep('Clatsop Coordinating Council',oweb.all$Grantee),])),
    rep("Nicolai-Wickiup WC",nrow(oweb.all[grep('Clatsop Coordinating Council',oweb.all$Grantee),])),
    rep("Ecola Creek WC",nrow(oweb.all[grep('Clatsop Coordinating Council',oweb.all$Grantee),]))
  )
oweb.all =  rbind(oweb.all[grep('Clatsop Coordinating Council',oweb.all$Grantee,invert=T),],replace)


replace = rbind(oweb.all[grep('Salem-Keizer Urban WS Coordinating Council',oweb.all$Grantee),],
                oweb.all[grep('Salem-Keizer Urban WS Coordinating Council',oweb.all$Grantee),],
                oweb.all[grep('Salem-Keizer Urban WS Coordinating Council',oweb.all$Grantee),])
replace$Grantee =
  c(rep("Claggett Creek WC",nrow(oweb.all[grep('Salem-Keizer Urban WS Coordinating Council',oweb.all$Grantee),])),
    rep("Pringle Creek WC",nrow(oweb.all[grep('Salem-Keizer Urban WS Coordinating Council',oweb.all$Grantee),])),
    rep("Mill Creek WC" ,nrow(oweb.all[grep('Salem-Keizer Urban WS Coordinating Council',oweb.all$Grantee),]))
  )
oweb.all =  rbind(oweb.all[grep('Salem-Keizer Urban WS Coordinating Council',oweb.all$Grantee,invert=T),],replace)


replace = rbind(oweb.all[grep('Rickreall/Luckiamute/Glenn-Gibson WCs',oweb.all$Grantee),],
                oweb.all[grep('Rickreall/Luckiamute/Glenn-Gibson WCs',oweb.all$Grantee),],
                oweb.all[grep('Rickreall/Luckiamute/Glenn-Gibson WCs',oweb.all$Grantee),])
replace$Grantee =
  c(rep('Rickreall WC',nrow(oweb.all[grep('Rickreall/Luckiamute/Glenn-Gibson WCs',oweb.all$Grantee),])),
    rep('Luckiamute WC',nrow(oweb.all[grep('Rickreall/Luckiamute/Glenn-Gibson WCs',oweb.all$Grantee),])),
    rep('Glenn-Gibson WC',nrow(oweb.all[grep('Rickreall/Luckiamute/Glenn-Gibson WCs',oweb.all$Grantee),])))
oweb.all =  rbind(oweb.all[grep('Rickreall/Luckiamute/Glenn-Gibson WCs',oweb.all$Grantee,invert=T),],replace)


replace = rbind(oweb.all[oweb.all$Grantee=='Nehalem WC',],
                oweb.all[oweb.all$Grantee=='Nehalem WC',])
replace$Grantee =
  c(rep('Upper Nehalem WC',nrow(oweb.all[oweb.all$Grantee=='Nehalem WC',])),
    rep("Lower Nehalem WC",nrow(oweb.all[oweb.all$Grantee=='Nehalem WC',])))
oweb.all =  rbind(oweb.all[oweb.all$Grantee!='Nehalem WC',],replace)

replace = rbind(oweb.all[oweb.all$Grantee=='Nehalem WCs',],
                oweb.all[oweb.all$Grantee=='Nehalem WCs',])
replace$Grantee =
  c(rep('Upper Nehalem WC',nrow(oweb.all[oweb.all$Grantee=='Nehalem WCs',])),
    rep("Lower Nehalem WC",nrow(oweb.all[oweb.all$Grantee=='Nehalem WCs',])))
oweb.all =  rbind(oweb.all[oweb.all$Grantee!='Nehalem WCs',],replace)


replace = rbind(oweb.all[oweb.all$Grantee=='Smith River & Elk Cr WC',],
                oweb.all[oweb.all$Grantee=='Smith River & Elk Cr WC',])
replace$Grantee =
  c(rep('Smith River WC',nrow(oweb.all[oweb.all$Grantee=='Smith River & Elk Cr WC',])),
    rep("Elk Creek WC",nrow(oweb.all[oweb.all$Grantee=='Smith River & Elk Cr WC',])))
oweb.all =  rbind(oweb.all[oweb.all$Grantee!='Smith River & Elk Cr WC',],replace)


replace = rbind(oweb.all[oweb.all$Grantee=='Rogue Basin Coordinating Council',],
                oweb.all[oweb.all$Grantee=='Rogue Basin Coordinating Council',],
                oweb.all[oweb.all$Grantee=='Rogue Basin Coordinating Council',],
                oweb.all[oweb.all$Grantee=='Rogue Basin Coordinating Council',],
                oweb.all[oweb.all$Grantee=='Rogue Basin Coordinating Council',],
                oweb.all[oweb.all$Grantee=='Rogue Basin Coordinating Council',])
replace$Grantee =
  c(rep('Applegate WC',nrow(oweb.all[oweb.all$Grantee=='Rogue Basin Coordinating Council',])),
    rep("Upper Rogue WC",nrow(oweb.all[oweb.all$Grantee=='Rogue Basin Coordinating Council',])),
    rep('Illinois Valley WC',nrow(oweb.all[oweb.all$Grantee=='Rogue Basin Coordinating Council',])),
    rep("Lower Rogue WC",nrow(oweb.all[oweb.all$Grantee=='Rogue Basin Coordinating Council',])),
    rep('Seven Basins WC',nrow(oweb.all[oweb.all$Grantee=='Rogue Basin Coordinating Council',])),
    rep("Williams Creek WC",nrow(oweb.all[oweb.all$Grantee=='Rogue Basin Coordinating Council',])))
oweb.all =  rbind(oweb.all[oweb.all$Grantee!='Rogue Basin Coordinating Council',],replace)

replace = rbind(oweb.all[oweb.all$Grantee=='South Coast & Lower Rogue WCs',],
                oweb.all[oweb.all$Grantee=='South Coast & Lower Rogue WCs',])
replace$Grantee =
  c(rep('South Coast WC',nrow(oweb.all[oweb.all$Grantee=='South Coast & Lower Rogue WCs',])),
    rep("Lower Rogue WC",nrow(oweb.all[oweb.all$Grantee=='South Coast & Lower Rogue WCs',])))
oweb.all =  rbind(oweb.all[oweb.all$Grantee!='South Coast & Lower Rogue WCs',],replace)


replace = rbind(oweb.all[oweb.all$Grantee=='Rickreall & Glenn-Gibson WCs',],
                oweb.all[oweb.all$Grantee=='Rickreall & Glenn-Gibson WCs',])
replace$Grantee =
  c(rep('Glenn-Gibson WC',nrow(oweb.all[oweb.all$Grantee=='Rickreall & Glenn-Gibson WCs',])),
    rep("Rickreall WC",nrow(oweb.all[oweb.all$Grantee=='Rickreall & Glenn-Gibson WCs',])))
oweb.all =  rbind(oweb.all[oweb.all$Grantee!='Rickreall & Glenn-Gibson WCs',],replace)

oweb.all$Grantee[grep('East Lane SWCD',oweb.all$Grantee)] = 'Upper Willamette SWCD'
oweb.all$Grantee[grep('Baker Assn of Conservation Dist',oweb.all$Grantee)] = 'Baker Valley SWCD'

for (i in 1:nrow(oweb.all))
{
  if(is.na(oweb.all$HUC8[i]) & oweb.all$which.group[i]=='WC')
  {
    oweb.all$HUC8[i] = oregon.wc@data$HUC_8[match(oweb.all$Grantee[i],oregon.wc@data$altName)]
  }
if(is.na(oweb.all$HUC8[i]) & oweb.all$which.group[i]=='SWCD')
{
  oweb.all$HUC8[i] = oregon.swcd@data$HUC_8[match(oweb.all$Grantee[i],oregon.swcd@data$SWCD_Name)]
}
}



oregon.swcd@data$HUC_8[which(oregon.swcd$SWCD_Name == oweb.all$Grantee[(is.na(oweb.all$HUC8))][i])]
oweb.all$which.group[(is.na(oweb.all$HUC8))][i]=='SWCD'
oweb.all$Grantee[(is.na(oweb.all$HUC8))][i]
oregon.wc@data$HUC_8[which(oregon.wc$altName == oweb.all$Grantee[(is.na(oweb.all$HUC8))][i])]


oweb.all$uq.tid= paste(oweb.all$HUC8,oweb.all$Abs.Month,sep='_')

oweb.all = filter(oweb.all,!is.na(oweb.all$HUC8))



oweb.all$Project.Amount.Monthly = oweb.all$Project.Amount / ifelse(oweb.all$abs.length==0,1,oweb.all$abs.length)


oweb.all.projbymonth = oweb.all[rep(rownames(oweb.all),ifelse(oweb.all$abs.length==0,1,oweb.all$abs.length)),]
oweb.all.projbymonth$Abs.Month = oweb.all$sabs +unlist(sapply(ifelse(oweb.all$abs.length==0,1,oweb.all$abs.length), function(x) 1:x))
oweb.all.projbymonth$uq.tid = paste(oweb.all.projbymonth$HUC8,oweb.all.projbymonth$Abs.Month,sep='_')



################# COMPILE OWEB WC GRANTS BY  HUC8 #############
temp =  oweb.all.projbymonth %>% dplyr::group_by(uq.tid,Project.Type,which.group) %>% dplyr::summarise_each(funs(sum),Project.Amount.Monthly)


huc8_data[,as.vector(outer(unique(paste('OWEB_HUC8_Grant',oweb.all.projbymonth$Project.Type,sep='_')), 
                           unique(oweb.all.projbymonth$which.group), paste, sep="."))] = NA

for (i in 1:nrow(temp))
{
  huc8_data[match(temp$uq.tid[i],huc8_data$uq.tid),
            which(colnames(huc8_data)==paste(paste('OWEB_HUC8_Grant',temp$Project.Type[i],sep='_'),
                                             temp$which.group[i],sep='.'))] = 
    temp$Project.Amount.Monthly[i]
}


huc8_data[,colnames(huc8_data) %in%  unique(as.vector(outer('OWEB_HUC8_Grant',oweb.all.projbymonth$Project.Type,paste,sep='_')))][
  is.na(huc8_data[,colnames(huc8_data) %in%  unique(as.vector(outer('OWEB_HUC8_Grant',oweb.all.projbymonth$Project.Type,paste,sep='_')))])]  = 0


huc8_data[,grep('OWEB',colnames(huc8_data))][is.na(huc8_data[,grep('OWEB',colnames(huc8_data))])] = 0


################# COMPILE OWEB GRANTS BY WC #############

site.in.wc = sp::over(spTransform(all.params.spdf,CRS=CRS(proj4string(oregon.wc))),oregon.wc)
site.in.swcd = sp::over(spTransform(all.params.spdf,CRS=CRS(proj4string(oregon.swcd))),oregon.swcd)

all.params.spdf@data$which.wc = site.in.wc$altName
all.params.spdf@data$which.swcd = site.in.swcd$SWCD_Name
all.params.spdf@data$uq.wc.tid = paste(all.params.spdf@data$which.wc,all.params.spdf@data$Abs.Month,sep='_')
all.params.spdf@data$uq.swcd.tid = paste(all.params.spdf@data$which.swcd,all.params.spdf@data$Abs.Month,sep='_')

wc.year.month = merge(as.data.frame(oregon.wc@data$altName),Year.Month)
names(wc.year.month)[1] = 'altName'
wc.year.month$uq.wc.tid = paste(wc.year.month$altName,wc.year.month$Abs.Month,sep='_')

swcd.year.month = merge(as.data.frame(oregon.swcd@data$SWCD_Name),Year.Month)
names(swcd.year.month)[1] = 'SWCD_Name'
swcd.year.month$uq.swcd.tid = paste(swcd.year.month$SWCD_Name,swcd.year.month$Abs.Month,sep='_')

oweb.all.projbymonth$uq.wc.tid = paste(oweb.all.projbymonth$Grantee,oweb.all.projbymonth$Abs.Month,sep='_')
oweb.all.projbymonth$uq.swcd.tid = paste(oweb.all.projbymonth$Grantee,oweb.all.projbymonth$Abs.Month,sep='_')

temp =  oweb.all.projbymonth %>% filter(which.group=='WC') %>% dplyr::group_by(uq.wc.tid,Project.Type) %>% dplyr::summarise_each(funs(sum),Project.Amount.Monthly)
wc.year.month[,as.vector(unique(paste('OWEB_Grant',oweb.all.projbymonth$Project.Type,sep='_')))] = NA
for (i in 1:nrow(temp))
{wc.year.month[match(temp$uq.wc.tid[i],wc.year.month$uq.wc.tid),
               which(colnames(wc.year.month)==paste('OWEB_Grant',temp$Project.Type[i],sep='_'))] = 
  temp$Project.Amount.Monthly[i]}

wc.year.month[,colnames(wc.year.month) %in%  unique(as.vector(outer('OWEB_Grant',oweb.all.projbymonth$Project.Type,paste,sep='_')))][
  is.na(wc.year.month[,colnames(wc.year.month) %in%  unique(as.vector(outer('OWEB_Grant',oweb.all.projbymonth$Project.Type,paste,sep='_')))])]  = 0
wc.year.month[,grep('OWEB',colnames(wc.year.month))][is.na(wc.year.month[,grep('OWEB',colnames(wc.year.month))])] = 0

################# COMPILE OWEB GRANTS BY SWCD #############

temp =  oweb.all.projbymonth %>% filter(which.group=='SWCD') %>% dplyr::group_by(uq.swcd.tid,Project.Type) %>% dplyr::summarise_each(funs(sum),Project.Amount.Monthly)
swcd.year.month[,as.vector(unique(paste('OWEB_Grant',oweb.all.projbymonth$Project.Type,sep='_')))] = NA
for (i in 1:nrow(temp))
{swcd.year.month[match(temp$uq.swcd.tid[i],swcd.year.month$uq.swcd.tid),
                 which(colnames(swcd.year.month)==paste('OWEB_Grant',temp$Project.Type[i],sep='_'))] = 
  temp$Project.Amount.Monthly[i]}

swcd.year.month[,colnames(swcd.year.month) %in%  unique(as.vector(outer('OWEB_Grant',oweb.all.projbymonth$Project.Type,paste,sep='_')))][
  is.na(swcd.year.month[,colnames(swcd.year.month) %in%  unique(as.vector(outer('OWEB_Grant',oweb.all.projbymonth$Project.Type,paste,sep='_')))])]  = 0
swcd.year.month[,grep('OWEB',colnames(swcd.year.month))][is.na(swcd.year.month[,grep('OWEB',colnames(swcd.year.month))])] = 0


########## ADD WC DATA ###################

# surv = read.csv('https://github.com/tylerascott/quinalt/raw/master/Input/Winter-2012-Council-Survey_Scott.csv')
# surv[surv==''] = NA
# 
# wc.data = data.frame(NAME = surv$Custom.Data)
# wc.data$RECOGNIZED = surv$Is.your.council.officially.designated.or.recognized.by.a.local.government.=='Yes'
# wc.data$NONPROFIT = surv$Is.your.council.a.designated.501.c..3..non.profit.organization.=='Yes'
# wc.data$CITY = surv$X.80
# wc.data$ZIPCODE = surv$X.82
# 
# te.foun = melt(surv[,c(1,87:109)],id.vars='Custom.Data')
# te.foun = te.foun[!is.na(te.foun$value),]
# colnames(te.foun) = c('NAME','Question','YEAR.FOUNDED')
# wc.data = join(wc.data,te.foun[,-2])
# 
# te.bud = melt(surv[,c(1,118:124)],id.vars='Custom.Data')
# te.bud = te.bud[!is.na(te.bud$value),]
# colnames(te.bud) = c('NAME','Question','OPERATING.BUDGET')
# wc.data = join(wc.data,te.bud[,-2])
# 
# te.bud = melt(surv[,c(1,127:133)],id.vars='Custom.Data')
# te.bud = te.bud[!is.na(te.bud$value),]
# colnames(te.bud) = c('NAME','Question','TOTAL.BUDGET')
# wc.data = join(wc.data,te.bud[,-2])
# 
# 
# te.coord = melt(surv[,c(1,134:136)],id.vars='Custom.Data')
# te.coord = te.coord[!is.na(te.coord$value),]
# colnames(te.coord) = c('NAME','Question','COORD.TYPE')
# wc.data = join(wc.data,te.coord[,-2])
# 
# te.coord.fte = melt(surv[,c(1,134:136)],id.vars='Custom.Data')
# te.coord.fte = te.coord.fte[!is.na(te.coord.fte$value),]
# colnames(te.coord.fte) = c('NAME','Question','COORD.FTE')
# wc.data = join(wc.data,te.coord.fte[,-2])
# 
# te.staff.fte = melt(surv[,c(1,156:166)],id.vars='Custom.Data')
# te.staff.fte = te.staff.fte[!is.na(te.staff.fte$value),]
# colnames(te.staff.fte) = c('NAME','Question','STAFF.FTE')
# wc.data = join(wc.data,te.staff.fte[,-2])
# 
# wc.data = cbind(wc.data,!is.na(surv[,c(211:216)]))
# colnames(wc.data)[13:ncol(wc.data)] = c(paste(c('OWEB','Federal','Foundation','Donors','Membership','Other'),'Support',sep='.'))
# 
# wc.data$VOLUNTEERS = surv$Approximately.how.many.non.board.volunteers.have.you.engaged.over.the.past.year.
# 
# wc.data$BOARD.SIZE = surv$How.many.members.serve.on.your.board.
# 
# #write.csv(oregon.wc@data,'Input/watershed_councils.csv')
# write.csv(wc.data,'Input/watershed_councils_tempfile.csv')
# 
# 
# 

# 
# oregon.wc@data$altName[unlist(sapply(wc.data$NAME,agrep,oregon.wc@data$altName,ignore.case=TRUE,max=2))]
# 
# 
# sapply(wc.data$NAME,agrep,oregon.wc@data$altName,ignore.case=TRUE,max=2)
# 


######### COMPUTE RESTORATION GRANT COVARIATES #########

obs.data = all.params.spdf@data

#HUC8 Covariates#
tempAll = huc8_data %>% dplyr::group_by(HUC8) %>% dplyr::arrange(HUC8,Abs.Month) %>%
  dplyr::mutate_each(funs(cumsum),contains('OWEB'))
names(tempAll)[grep('OWEB',names(tempAll))] = paste(names(tempAll)[grep('OWEB',names(tempAll))],'All',sep='_')

temp12 = huc8_data %>% dplyr::group_by(HUC8) %>% dplyr::arrange(HUC8,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=12,fill=0)),contains('OWEB'))
names(temp12)[grep('OWEB',names(temp12))] = paste(names(temp12)[grep('OWEB',names(temp12))],'12',sep='_')

temp24 = huc8_data %>% dplyr::group_by(HUC8) %>% dplyr::arrange(HUC8,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=24,fill=0)),contains('OWEB'))
names(temp24)[grep('OWEB',names(temp24))] = paste(names(temp24)[grep('OWEB',names(temp24))],'24',sep='_')

temp36 = huc8_data %>% dplyr::group_by(HUC8) %>% dplyr::arrange(HUC8,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=36,fill=0)),contains('OWEB'))
names(temp36)[grep('OWEB',names(temp36))] = paste(names(temp36)[grep('OWEB',names(temp36))],'36',sep='_')

temp48 = huc8_data %>% dplyr::group_by(HUC8) %>% dplyr::arrange(HUC8,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=48,fill=0)),contains('OWEB'))
names(temp48)[grep('OWEB',names(temp48))] = paste(names(temp48)[grep('OWEB',names(temp48))],'48',sep='_')

temp60 = huc8_data %>% dplyr::group_by(HUC8) %>% dplyr::arrange(HUC8,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=60,fill=0)),contains('OWEB'))
names(temp60)[grep('OWEB',names(temp60))] = paste(names(temp60)[grep('OWEB',names(temp60))],'60',sep='_')


temp = join_all(list(as.data.frame(tempAll[,c('HUC8','uq.tid',grep('OWEB',names(tempAll),value=T))]),
                     as.data.frame(temp12[,c('HUC8','uq.tid',grep('OWEB',names(temp12),value=T))]),
                     as.data.frame(temp24[,c('HUC8','uq.tid',grep('OWEB',names(temp24),value=T))]),
                     as.data.frame(temp36[,c('HUC8','uq.tid',grep('OWEB',names(temp36),value=T))]),
                     as.data.frame(temp48[,c('HUC8','uq.tid',grep('OWEB',names(temp48),value=T))]),
                     as.data.frame(temp60[,c('HUC8','uq.tid',grep('OWEB',names(temp60),value=T))])))
temp[is.na(temp)] = 0

obs.data = join(obs.data,temp)

all.params.spdf@data = obs.data

#WC covariates##
obs.data = all.params.spdf@data
tempAll = wc.year.month %>% dplyr::group_by(altName) %>% dplyr::arrange(altName,Abs.Month) %>%
  dplyr::mutate_each(funs(cumsum),contains('OWEB'))
names(tempAll)[grep('OWEB',names(tempAll))] = paste(paste(names(tempAll)[grep('OWEB',names(tempAll))],'All',sep='_'),'WC',sep='_')

temp12 = wc.year.month %>% dplyr::group_by(altName) %>% dplyr::arrange(altName,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=12,fill=0)),contains('OWEB'))
names(temp12)[grep('OWEB',names(temp12))] = paste(paste(names(temp12)[grep('OWEB',names(temp12))],'12',sep='_'),'WC',sep='_')

temp24 = wc.year.month %>% dplyr::group_by(altName) %>% dplyr::arrange(altName,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=24,fill=0)),contains('OWEB'))
names(temp24)[grep('OWEB',names(temp24))] = paste(paste(names(temp24)[grep('OWEB',names(temp24))],'24',sep='_'),'WC',sep='_')

temp36 = wc.year.month %>% dplyr::group_by(altName) %>% dplyr::arrange(altName,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=36,fill=0)),contains('OWEB'))
names(temp36)[grep('OWEB',names(temp36))] = paste(paste(names(temp36)[grep('OWEB',names(temp36))],'36',sep='_'),'WC',sep='_')

temp48 = wc.year.month %>% dplyr::group_by(altName) %>% dplyr::arrange(altName,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=48,fill=0)),contains('OWEB'))
names(temp48)[grep('OWEB',names(temp48))] = paste(paste(names(temp48)[grep('OWEB',names(temp48))],'48',sep='_'),'WC',sep='_')

temp60 = wc.year.month %>% dplyr::group_by(altName) %>% dplyr::arrange(altName,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=60,fill=0)),contains('OWEB'))
names(temp60)[grep('OWEB',names(temp60))] = paste(paste(names(temp60)[grep('OWEB',names(temp60))],'60',sep='_'),'WC',sep='_')

temp = join_all(list(as.data.frame(tempAll),as.data.frame(temp12),as.data.frame(temp24),
                     as.data.frame(temp36),as.data.frame(temp48),
                     as.data.frame(temp60)))
temp[is.na(temp)] = 0

obs.data = join(obs.data,temp)

all.params.spdf@data = obs.data

#SWCD Covariates#
obs.data = all.params.spdf@data
tempAll = swcd.year.month %>% dplyr::group_by(SWCD_Name) %>% dplyr::arrange(SWCD_Name,Abs.Month) %>%
  dplyr::mutate_each(funs(cumsum),contains('OWEB'))
names(tempAll)[grep('OWEB',names(tempAll))] = paste(paste(names(tempAll)[grep('OWEB',names(tempAll))],'All',sep='_'),'SWCD',sep='_')

temp12 = swcd.year.month %>% dplyr::group_by(SWCD_Name) %>% dplyr::arrange(SWCD_Name,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=12,fill=0)),contains('OWEB'))
names(temp12)[grep('OWEB',names(temp12))] = paste(paste(names(temp12)[grep('OWEB',names(temp12))],'12',sep='_'),'SWCD',sep='_')

temp24 = swcd.year.month %>% dplyr::group_by(SWCD_Name) %>% dplyr::arrange(SWCD_Name,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=24,fill=0)),contains('OWEB'))
names(temp24)[grep('OWEB',names(temp24))] = paste(paste(names(temp24)[grep('OWEB',names(temp24))],'24',sep='_'),'SWCD',sep='_')

temp36 = swcd.year.month %>% dplyr::group_by(SWCD_Name) %>% dplyr::arrange(SWCD_Name,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=36,fill=0)),contains('OWEB'))
names(temp36)[grep('OWEB',names(temp36))] = paste(paste(names(temp36)[grep('OWEB',names(temp36))],'36',sep='_'),'SWCD',sep='_')

temp48 = swcd.year.month %>% dplyr::group_by(SWCD_Name) %>% dplyr::arrange(SWCD_Name,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=48,fill=0)),contains('OWEB'))
names(temp48)[grep('OWEB',names(temp48))] = paste(paste(names(temp48)[grep('OWEB',names(temp48))],'48',sep='_'),'SWCD',sep='_')

temp60 = swcd.year.month %>% dplyr::group_by(SWCD_Name) %>% dplyr::arrange(SWCD_Name,Abs.Month) %>%
  dplyr::mutate_each(funs(roll_sumr(.,n=60,fill=0)),contains('OWEB'))
names(temp60)[grep('OWEB',names(temp60))] = paste(paste(names(temp60)[grep('OWEB',names(temp60))],'60',sep='_'),'SWCD',sep='_')

temp = join_all(list(as.data.frame(tempAll),as.data.frame(temp12),as.data.frame(temp24),
                     as.data.frame(temp36),as.data.frame(temp48),
                     as.data.frame(temp60)))
temp[is.na(temp)] = 0

obs.data = join(obs.data,temp)

all.params.spdf@data = obs.data

#####rename counties for a few stations#####

all.params.spdf@data$CountyName[all.params.spdf@data$Station == 10407] = 'MALHEUR'
all.params.spdf@data$CountyName[all.params.spdf@data$Station == 12261] = 'MALHEUR'
all.params.spdf@data$CountyName[all.params.spdf@data$Station == 10616] = 'MULTNOMAH'

all.params.spdf@data = join(all.params.spdf@data,
                     dplyr::select(huc8_data,-c(grep('OWEB_OWRI',names(huc8_data)),grep('OWEB_HUC8',names(huc8_data)))))

###### REMOVE AND SAVE ########
if(remote)
{setwd('/homes/tscott1/win/user/quinalt')}
if(!remote)
{setwd('//Users/TScott/Google Drive/quinalt/')}

#rm(list=ls()[ls()%in%c('all.params.spdf','huc8_data')==FALSE])
rm(list=ls()[grep('select',ls())])
rm(list=ls()[grep('grids',ls())])
rm(list=ls()[grep('R1',ls())])
rm(list=ls()[grep('R2',ls())])
rm(list=ls()[grep('R3',ls())])
rm(list=ls()[grep('R4',ls())])
rm(s)
rm(s.crop)

save.image('temp_workspace_data.RData')
write.csv(all.params.spdf@data,'Input/temp_data.csv')

library(mail)
mail::sendmail('tyler.andrew.scott@gmail.com','prepare_data.R finished','nori has finished quinalt project data prep (with precip)')


