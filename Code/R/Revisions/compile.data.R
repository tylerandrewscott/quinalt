
##GENERATE DATA FOR MODELING

######### SETUP BASICS ##########

remote = TRUE
if(remote)
{setwd('/homes/tscott1/win/user/quinalt')}
if(!remote)
{setwd('//Users/TScott/Google Drive/quinalt/')}


First.Year = 1990
Last.Year = 2035
calc.precip=FALSE
site.buffers=FALSE
compile.by.wc = FALSE
compile.by.swcd = FALSE
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
Year.Month$Water.Year = Year.Month$YEAR
Year.Month$Water.Year[Year.Month$Month.Num%in%10:12] <- Year.Month$Water.Year[Year.Month$Month.Num%in%10:12] -1

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

all.params.spdf@data$Water.Year = all.params.spdf@data$YEAR
all.params.spdf@data$Water.Year[all.params.spdf@data$Month.Num %in% 10:12] <- all.params.spdf@data$Water.Year[all.params.spdf@data$Month.Num %in% 10:12] - 1

all.params.spdf = all.params.spdf[all.params.spdf@data$Water.Year>=1995,]


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


#load oregon huc8 shapefile
oregon.huc8 = readOGR(dsn="SpatialData/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)
#join into one polygon
reg <- unionSpatialPolygons(oregon.huc8, rep(1,91),100)
oregon.huc8.points = fortify(reg, region="id")
oregon.huc8.reference = join(oregon.huc8.points, oregon.huc8@data, by="id")
#create oregon coastline dataframe
oregon.coast.df = subset(oregon.huc8.reference,long<(-124)&lat>=min(oregon.huc8.reference[,2]))

plot(oregon.coast.df$long,oregon.coast.df$lat,xlim=c(-125,-123))

mat.dists <- spDists(uq.coords, as.matrix(oregon.coast.df[,1:2]), longlat=TRUE)
uq@data$seaDist <-  apply(mat.dists, 1, min)

rm(mat.dists)
rm(oregon.coast.df)
rm(oregon.huc8.reference)


all.params.spdf@data = join(all.params.spdf@data,uq@data[,c('Station','elevation','seaDist')])

if (site.buffers)
{
  uq = spTransform(uq,CRSobj=CRS(proj4string(R1.ag)))
  
  R1.list = sapply(paste0('SpatialData/tf_rasters/',grep('1992',list.files('SpatialData/tf_rasters/'),value=T)), function(x) raster(x))
  R1.list <- lapply(R1.list, function(x) raster::extract(x,y=uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE))
  R1 = join_all(R1.list)
  
  R2.list = sapply(paste0('SpatialData/tf_rasters/',grep('2001',list.files('SpatialData/tf_rasters/'),value=T)), function(x) raster(x))
  R2.list <- lapply(R2.list, function(x) raster::extract(x,y=uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE))
  R2 = join_all(R2.list)
  R2 = mutate(R1,tf_ag_2001 = tf_past_2001 + tf_crop_2001)
  
  R3.list = sapply(paste0('SpatialData/tf_rasters/',grep('2006',list.files('SpatialData/tf_rasters/'),value=T)), function(x) raster(x))
  R3.list <- lapply(R3.list, function(x) raster::extract(x,y=uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE))
  R3 = join_all(R3.list)
  R3 = mutate(R1,tf_ag_2006 = tf_past_2006 + tf_crop_2006)
  
  R4.list = sapply(paste0('SpatialData/tf_rasters/',grep('2011',list.files('SpatialData/tf_rasters/'),value=T)), function(x) raster(x))
  R4.list <- lapply(R4.list, function(x) raster::extract(x,y=uq,fun=mean,df=T,buffer=100,na.rm=TRUE,small=FALSE))
  R4 = join_all(R4.list)
  R4 = mutate(R1,tf_ag_2011 = tf_past_2011 + tf_crop_2011)
  
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
  all.params.spdf@data = join(all.params.spdf@data,cov)
}

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


proj.info <- proj.info %>% dplyr::mutate(Project.Number = drvdOwebNum, HUC8 = drvdHUC4thField) %>%
#  filter(StartYear!=0,CompleteYear!=0,StartMonth!=0,CompleteMonth!=0) %>%
  mutate(YEAR = CompleteYear, Month.Num = CompleteMonth) 



proj.info = join(proj.info,Year.Month)

proj.info$uq.tid = paste(proj.info$HUC8,proj.info$Abs.Month,sep='_')



#binary true/false: OWEB project?
proj.info <- proj.info %>% mutate(OWRI.OWEB.Grant = ifelse(proj.info$drvdOwebNum=='','NOT_OWEB_OWRI','OWEB_OWRI'),
                                  drvdProjDesc = tolower(proj.info$drvdProjDesc))

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


owri.huc8.summary = proj.info %>% dplyr::filter(about_wq=='wq',OWRI.OWEB.Grant=='NOT_OWEB_OWRI') %>% dplyr::group_by(Water.Year,HUC8) %>% dplyr::summarise(owri.wq.nonoweb.cash.wy = sum(TotalCash),owri.wq.nonoweb.inkind.wy=sum(TotalInKind),owri.wq.nonoweb.both.wy=sum(TotalBoth))


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
if(calc.precip)
{
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
}


if(remote)
{setwd('/homes/tscott1/win/user/quinalt')}
if(!remote)
{setwd('//Users/TScott/Google Drive/quinalt')}


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
          dplyr::filter(which.group %in% c('WC','SWCD','Public')) %>% group_by(Project.Type,which.group) %>%
          summarise_each(funs(nobs,mean),Project.Amount,abs.length))[,-3]
temp[,1] = ifelse(duplicated(data.frame(temp)[,1]),'',data.frame(temp)[,1])
names(temp) = c('Project','Grantee','N','Average cost','Average length (months)')


stargazer(data.frame(temp),summary=F,rownames = F,no.space = T,digits=0,type='latex',
          out='/homes/tscott1/win/user/quinalt/JPART_Submission/Version2/grantsummarytable.tex',
          title = 'OWEB grant summary statistics (1997-2013)',
          label='table:grantsummary')


oweb.all$Grantee = gsub('Watershed Council','WC',oweb.all$Grantee)
oweb.all$Grantee[grep('Applegate',oweb.all$Grantee)] = 'Applegate WC'
oweb.all$Grantee = gsurrrrb('Model WS Program','Model WS',oweb.all$Grantee)
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

oweb.all <- oweb.all %>% mutate(HUC8 = as.character(HUC8),which.group = as.character(which.group))

#fill in missing huc8 values using oregon gis data so that all grants have an huc8 associated with them
oweb.all$HUC8[is.na(oweb.all$HUC8) & oweb.all$which.group=='WC'] <- as.character(oregon.wc@data$HUC_8[match(oweb.all$Grantee[is.na(oweb.all$HUC8) & oweb.all$which.group=='WC'],oregon.wc@data$altName)])
oweb.all$HUC8[is.na(oweb.all$HUC8) & oweb.all$which.group=='SWCD'] <- as.character(oregon.swcd@data$HUC_8[match(oweb.all$Grantee[is.na(oweb.all$HUC8) & oweb.all$which.group=='SWCD'],oregon.swcd@data$SWCD_Name)])


oweb.all$uq.tid= paste(oweb.all$HUC8,oweb.all$Abs.Month,sep='_')

oweb.all = filter(oweb.all,!is.na(oweb.all$HUC8))

# Grants that finished in prior water year
oweb.all$Water.Year = oweb.all$END.YEAR

site.in.wc = sp::over(spTransform(all.params.spdf,CRS=CRS(proj4string(oregon.wc))),oregon.wc)
site.in.swcd = sp::over(spTransform(all.params.spdf,CRS=CRS(proj4string(oregon.swcd))),oregon.swcd)

all.params.spdf@data$which.wc = site.in.wc$altName
all.params.spdf@data$which.swcd = site.in.swcd$SWCD_Name
all.params.spdf@data$uq.wc.tid = paste(all.params.spdf@data$which.wc,all.params.spdf@data$Abs.Month,sep='_')
all.params.spdf@data$uq.swcd.tid = paste(all.params.spdf@data$which.swcd,all.params.spdf@data$Abs.Month,sep='_')


oweb.huc8.summary = oweb.all %>% group_by(Water.Year,HUC8,which.group) %>% summarise_each(funs(sum),Project.Amount)
library(tidyr)
temp = spread(data.frame(oweb.huc8.summary),key = which.group,Project.Amount)
temp = full_join(temp, tidyr::expand(temp,Water.Year,HUC8))

temp$Other[is.na(temp$Other)] = 0
temp$WC[is.na(temp$WC)] = 0
temp$SWCD[is.na(temp$SWCD)] = 0
temp$Public[is.na(temp$Public)] = 0
all.params.spdf@data[,paste0('OWEB.p1.',names(temp)[3:6])] = temp[match(paste(all.params.spdf@data$Water.Year - 1,all.params.spdf@data$HUC8),paste(temp$Water.Year,temp$HUC8)),3:ncol(temp)]
all.params.spdf@data[,paste0('OWEB.p2.',names(temp)[3:6])] = temp[match(paste(all.params.spdf@data$Water.Year - 2,all.params.spdf@data$HUC8),paste(temp$Water.Year,temp$HUC8)),3:ncol(temp)]
all.params.spdf@data[,paste0('OWEB.p3.',names(temp)[3:6])] = temp[match(paste(all.params.spdf@data$Water.Year - 3,all.params.spdf@data$HUC8),paste(temp$Water.Year,temp$HUC8)),3:ncol(temp)]


oweb.huc8.summary.project = oweb.all %>% group_by(Water.Year,HUC8,which.group,Project.Type) %>% summarise_each(funs(sum),Project.Amount)

oweb.huc8.summary.project = oweb.huc8.summary.project %>% mutate(id = paste('OWEB',which.group,Project.Type,sep='.')) %>% dplyr::select(-which.group,-Project.Type)


oweb.huc8.proj = spread(oweb.huc8.summary.project,key = id,value=Project.Amount,fill=0)


temp = oweb.huc8.proj[match(paste(all.params.spdf@data$HUC8,all.params.spdf@data$Water.Year-1),paste(oweb.huc8.proj$HUC8,oweb.huc8.proj$Water.Year)),-c(1:3)]
names(temp) = paste(names(temp),'p1',sep='.')
temp[is.na(temp)] = 0
all.params.spdf@data = cbind(all.params.spdf@data,temp)

temp = oweb.huc8.proj[match(paste(all.params.spdf@data$HUC8,all.params.spdf@data$Water.Year-2),paste(oweb.huc8.proj$HUC8,oweb.huc8.proj$Water.Year)),-c(1:3)]
names(temp) = paste(names(temp),'p2',sep='.')
temp[is.na(temp)] = 0
all.params.spdf@data = cbind(all.params.spdf@data,temp)

temp = oweb.huc8.proj[match(paste(all.params.spdf@data$HUC8,all.params.spdf@data$Water.Year-3),paste(oweb.huc8.proj$HUC8,oweb.huc8.proj$Water.Year)),-c(1:3)]
names(temp) = paste(names(temp),'p3',sep='.')
temp[is.na(temp)] = 0
all.params.spdf@data = cbind(all.params.spdf@data,temp)


#####rename counties for a few stations#####

all.params.spdf@data$CountyName[all.params.spdf@data$Station == 10407] = 'MALHEUR'
all.params.spdf@data$CountyName[all.params.spdf@data$Station == 12261] = 'MALHEUR'
all.params.spdf@data$CountyName[all.params.spdf@data$Station == 10616] = 'MULTNOMAH'

temp = all.params.spdf@data
# 
# 
# temp$WC.grants.in.p1.wy = oweb.huc8.summary$Project.Amount[oweb.huc8.summary$which.group=='WC'][match(paste(temp$Water.Year - 1,temp$HUC8),paste(oweb.huc8.summary$Water.Year,oweb.huc8.summary$HUC8)[oweb.huc8.summary$which.group=='WC'])]
# temp$WC.grants.in.p2.wy = oweb.huc8.summary$Project.Amount[oweb.huc8.summary$which.group=='WC'][match(paste(temp$Water.Year - 2,temp$HUC8),paste(oweb.huc8.summary$Water.Year,oweb.huc8.summary$HUC8)[oweb.huc8.summary$which.group=='WC'])]
# temp$WC.grants.in.p3.wy = oweb.huc8.summary$Project.Amount[oweb.huc8.summary$which.group=='WC'][match(paste(temp$Water.Year - 3,temp$HUC8),paste(oweb.huc8.summary$Water.Year,oweb.huc8.summary$HUC8)[oweb.huc8.summary$which.group=='WC'])]
# 
# temp$SWCD.grants.in.p1.wy = oweb.huc8.summary$Project.Amount[oweb.huc8.summary$which.group=='SWCD'][match(paste(temp$Water.Year - 1,temp$HUC8),paste(oweb.huc8.summary$Water.Year,oweb.huc8.summary$HUC8)[oweb.huc8.summary$which.group=='SWCD'])]
# temp$SWCD.grants.in.p2.wy = oweb.huc8.summary$Project.Amount[oweb.huc8.summary$which.group=='SWCD'][match(paste(temp$Water.Year - 2,temp$HUC8),paste(oweb.huc8.summary$Water.Year,oweb.huc8.summary$HUC8)[oweb.huc8.summary$which.group=='SWCD'])]
# temp$SWCD.grants.in.p3.wy = oweb.huc8.summary$Project.Amount[oweb.huc8.summary$which.group=='SWCD'][match(paste(temp$Water.Year - 3,temp$HUC8),paste(oweb.huc8.summary$Water.Year,oweb.huc8.summary$HUC8)[oweb.huc8.summary$which.group=='SWCD'])]
# 
# temp$Public.grants.in.p1.wy = oweb.huc8.summary$Project.Amount[oweb.huc8.summary$which.group=='Public'][match(paste(temp$Water.Year - 1,temp$HUC8),paste(oweb.huc8.summary$Water.Year,oweb.huc8.summary$HUC8)[oweb.huc8.summary$which.group=='Public'])]
# temp$Public.grants.in.p2.wy = oweb.huc8.summary$Project.Amount[oweb.huc8.summary$which.group=='Public'][match(paste(temp$Water.Year - 2,temp$HUC8),paste(oweb.huc8.summary$Water.Year,oweb.huc8.summary$HUC8)[oweb.huc8.summary$which.group=='Public'])]
# temp$Public.grants.in.p3.wy = oweb.huc8.summary$Project.Amount[oweb.huc8.summary$which.group=='Public'][match(paste(temp$Water.Year - 3,temp$HUC8),paste(oweb.huc8.summary$Water.Year,oweb.huc8.summary$HUC8)[oweb.huc8.summary$which.group=='Public'])]
# 
# temp$Other.grants.in.p1.wy = oweb.huc8.summary$Project.Amount[oweb.huc8.summary$which.group=='Other'][match(paste(temp$Water.Year - 1,temp$HUC8),paste(oweb.huc8.summary$Water.Year,oweb.huc8.summary$HUC8)[oweb.huc8.summary$which.group=='Other'])]
# temp$Other.grants.in.p2.wy = oweb.huc8.summary$Project.Amount[oweb.huc8.summary$which.group=='Other'][match(paste(temp$Water.Year - 2,temp$HUC8),paste(oweb.huc8.summary$Water.Year,oweb.huc8.summary$HUC8)[oweb.huc8.summary$which.group=='Other'])]
# temp$Other.grants.in.p3.wy = oweb.huc8.summary$Project.Amount[oweb.huc8.summary$which.group=='Other'][match(paste(temp$Water.Year - 3,temp$HUC8),paste(oweb.huc8.summary$Water.Year,oweb.huc8.summary$HUC8)[oweb.huc8.summary$which.group=='Other'])]

temp$OWRI.proj.in.p1.wy = owri.huc8.summary$owri.wq.nonoweb.cash.wy[match(paste(temp$Water.Year - 1,temp$HUC8),paste(owri.huc8.summary$Water.Year,owri.huc8.summary$HUC8))]
temp$OWRI.proj.in.p2.wy = owri.huc8.summary$owri.wq.nonoweb.cash.wy[match(paste(temp$Water.Year - 2,temp$HUC8),paste(owri.huc8.summary$Water.Year,owri.huc8.summary$HUC8))]
temp$OWRI.proj.in.p3.wy = owri.huc8.summary$owri.wq.nonoweb.cash.wy[match(paste(temp$Water.Year - 3,temp$HUC8),paste(owri.huc8.summary$Water.Year,owri.huc8.summary$HUC8))]




# 
# all.params.spdf@data = join(all.params.spdf@data,
#                             dplyr::select(huc8_data,-c(grep('OWEB_OWRI',names(huc8_data)),grep('OWEB_HUC8',names(huc8_data)))))



#load("/homes/tscott1/win/user/quinalt/temp_workspace_precip.RData")
mod.data = read.csv('Input/temp_data.csv')

mod.data$Station = as.character(mod.data$Station)

temp = left_join(temp,dplyr::select(mod.data,uq.tid,Station,ag.huc8,forst.huc8,wet.huc8,dev.huc8,monthly.precip.median,Wetl,Forst,Ag,Dev))

for (i in grep('grants.in.|proj.in',colnames(temp)))
{
  temp[,i][is.na(temp[,i])] = 0
}


all.params.spdf@data <- temp

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
write.csv(all.params.spdf@data,'Input/update_data.csv')

library(mail)
mail::sendmail('tyler.andrew.scott@gmail.com','prepare_data.R finished','nori has finished quinalt project data prep (with precip)')


