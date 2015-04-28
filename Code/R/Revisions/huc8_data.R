
##CALCULATE OWRI ON RESTORATION PROJECT SPENDING BY HUC8 and YEAR
###################
First.Year = 1990
Last.Year = 2016
setwd('/homes/tscott1/win/user/quinalt')
require(foreign)
require(plyr)
require(dplyr)
require(rgdal)
require(sp)
require(rgeos)
require(maptools)
require(ggplot2)
require(reshape2)
library(devtools)
library(RCurl)
library(gdata)

require(plyr)
require(dplyr)
require(sp)
require(rgdal)
require(maptools)
require(proj4)
require(ggplot2)
library(lubridate)

############

##################
#READ IN OREGON HUC8 SHAPEFILE, MAKE DATAFRAME
##################
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

#make sequence for years
YEAR = data.frame(YEAR = seq(First.Year,Last.Year,1))
MONTH = data.frame(MONTH = month.name,MONTH.ABB = month.abb)
Year.Month = merge(YEAR,MONTH, type='full')

Year.Month$Month.Num = match(Year.Month$MONTH,month.name)
Year.Month$Year.Num = Year.Month$YEAR - First.Year + 1
Year.Month$Abs.Month = Year.Month$Month.Num  + (Year.Month$Year.Num-1) *12

#full merge to replicated huc8 by year and month
oregon.huc8.df = merge(oregon.huc8.df,Year.Month,type='full')


######################
#ADD LANDCOVER DATA
######################


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
ag.huc8.2001$ag.huc8 = crop.huc8.2001$crop.huc8+
  past.huc8.2001$past.huc8[match(crop.huc8.2001$HUC8,past.huc8.2001$HUC8)]

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
ag.huc8.2006$ag.huc8 = crop.huc8.2006$crop.huc8+past.huc8.2006$past.huc8[match(crop.huc8.2006$HUC8,past.huc8.2006$HUC8)]


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
ag.huc8.2011$ag.huc8 = crop.huc8.2011$crop.huc8+past.huc8.2011$past.huc8[match(crop.huc8.2011$HUC8,past.huc8.2011$HUC8)]
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


######################
#ADD GRANT DATA
######################


setwd('/homes/tscott1/win/user/quinalt')
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


oregon.huc8 = readOGR(dsn="/homes/tscott1/win/user/quinalt_original/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)

oregon.owri.points = readOGR(dsn="/homes/tscott1/win/user/quinalt_original/", layer="OWRI_point_projects")
oregon.owri.points@data$id = rownames(oregon.owri.points@data)
oregon.owri.points = spTransform(oregon.owri.points,CRS(proj4string(oregon.huc8)))
oregon.owri.points.df = as.data.frame(oregon.owri.points)

oregon.poly.proj = readOGR(dsn="/homes/tscott1/win/user/quinalt_original/", layer="OWRI_poly_projects")
oregon.poly.proj@data$id = rownames(oregon.poly.proj@data)
oregon.poly.proj.points = fortify(oregon.poly.proj, region="id")
oregon.poly.proj.df = join(oregon.poly.proj.points, oregon.poly.proj@data, by="id")

oregon.line.proj = readOGR(dsn="/homes/tscott1/win/user/quinalt_original/", layer="OWRI_line_projects")
oregon.line.proj@data$id = rownames(oregon.line.proj@data)
oregon.line.proj.points = fortify(oregon.line.proj, region="id")
oregon.line.proj.df = join(oregon.line.proj.points, oregon.line.proj@data, by="id")

#convert line and poly projects to point projects using centroids
poly.centroids<-coordinates(matrix(cbind(oregon.poly.proj@data$point_x,oregon.poly.proj@data$point_y),ncol=2))
oregon.poly.proj.points = SpatialPointsDataFrame(coords=poly.centroids,data=oregon.poly.proj@data,proj4string=CRS(proj4string(oregon.poly.proj)))

line.centroids<-coordinates(matrix(cbind(oregon.line.proj@data$point_x,oregon.line.proj@data$point_y),ncol=2))
oregon.line.proj.points = SpatialPointsDataFrame(coords=line.centroids,data=oregon.line.proj@data,proj4string=CRS(proj4string(oregon.line.proj)))

#make one larg spatialpointsdf with all point/poly/line projects as points
oregon.poly.proj.points = spTransform(oregon.poly.proj.points,CRS(proj4string(oregon.owri.points)))
oregon.line.proj.points = spTransform(oregon.line.proj.points,CRS(proj4string(oregon.owri.points)))
all.points.list = list(oregon.owri.points,oregon.line.proj.points,oregon.poly.proj.points)
all.points.list[[2]]@data = all.points.list[[2]]@data[,names(oregon.owri.points@data)]
all.points.list[[3]]@data = all.points.list[[3]]@data[,names(oregon.owri.points@data)]
all.points  = do.call("rbind", all.points.list)

all.points@data$id = rownames(all.points@data)
all.points.df = as.data.frame(all.points)


all.points.uq.df = all.points.df %>% select(-gis_source,-site_id,-id,-point_x,-point_y,-coords.x1,-coords.x2)
all.points.uq.df = all.points.uq.df[!duplicated(all.points.uq.df$project_nb),]
all.points.uq.df$PROJNUM = all.points.uq.df$project_nb

proj.info = join(proj.info,all.points.uq.df,type='left')
proj.info$YEAR = proj.info$CompleteYear
proj.info$Month.Num = proj.info$CompleteMonth

proj.info = join(proj.info,Year.Month)
proj.info$uq.tid = paste(proj.info$HUC8,proj.info$Abs.Month,sep='_')

oregon.huc8.df$uq.tid = paste(oregon.huc8.df$HUC8,oregon.huc8.df$Abs.Month,sep='_')


proj.partners= read.csv('Input/owri_project_participants.csv')
public.list = c('county','city','state agency or jobs program (or ownership), state universities',
                'Soil & Water Conservation District',
                'federal agency or jobs program (or ownership)',
                'Extension Service (e.g. OSU Extension)')
wc.list = 'watershed council'
proj.partners$Category = ifelse(proj.partners$ParticipantType %in% public.list,'Public',
                                ifelse(proj.partners$ParticipantType %in% wc.list,'WC','Other'))
temp  = (table(proj.partners$PROJNUM,proj.partners$Category))
t1 = data.frame((ifelse(temp[,3] > 0,'WC',ifelse(temp[,2]>0,'Public','Other'))))
colnames(t1) = 'Lead.Type'
t1$PROJNUM = rownames(t1)

t2 = data.frame(rowSums(temp))
colnames(t2) = 'Num.Partners'
t2$PROJNUM = rownames(t2)
t3 = join(t1,t2)

proj.info = join(proj.info,t3)



#binary true/false: OWEB project?
proj.info$OWEB.Grant = ifelse(proj.info$drvdOwebNum=='',FALSE,TRUE)

proj.info$activity_t = tolower(proj.info$activity_t)
proj.info$drvdProjDesc = tolower(proj.info$drvdProjDesc)


###Create true/false for project about water quality
#######
proj.info$about_wq = FALSE
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
  ] = TRUE
########










temp.huc8 = oregon.huc8.df

temp = proj.info %>% dplyr::group_by(uq.tid) %>% dplyr::summarise_each(funs(sum),TotalCash,TotalInKind,TotalBoth)

temp.huc8 = join(temp.huc8,temp)

temp = proj.info %>% dplyr::group_by(uq.tid,Lead.Type,OWEB.Grant) %>% 
  dplyr::summarise_each(funs(sum),TotalCash,TotalInKind,TotalBoth)

tt = melt(temp,id.vars=c('uq.tid','Lead.Type','OWEB.Grant'))
tt$var.id = paste(tt$Lead.Type,tt$OWEB.Grant,tt$variable,sep='.')

temp.huc8[,unique(tt$var.id)] = NA

#place subset values into proper column
for (i in 1:ncol(temp.huc8))
{
  if (colnames(temp.huc8)[i] %in% unique(tt$var.id))
  {
    t1 = tt[tt$var.id == colnames(temp.huc8)[i],]
    temp.huc8[colnames(temp.huc8)[i]] = t1$value[match(temp.huc8$uq.tid,t1$uq.tid)]
    print(i)
  }
}


temp.huc8[,grep('Total',colnames(temp.huc8))][is.na(temp.huc8[,grep('Total',colnames(temp.huc8))])] = 0


huc8_data = temp.huc8

rm(list=ls()[ls()!='huc8_data'])
######
######
######
######

