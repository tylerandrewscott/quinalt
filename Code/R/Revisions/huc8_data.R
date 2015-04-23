

##CALCULATE OWRI ON RESTORATION PROJECT SPENDING BY HUC8 and YEAR
###################
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
YEAR = data.frame(YEAR = seq(1990,2013,1))
MONTH = data.frame(MONTH = month.name,MONTH.ABB = month.abb)
Year.Month = merge(YEAR,MONTH, type='full')

#full merge to replicated huc8 by year and month
oregon.huc8.df = merge(oregon.huc8.df,Year.Month,type='full')


######################
#ADD LANDCOVER DATA
######################


#1992 nlcd
ag.huc8.1992 = read.dbf('LandUse_RasterData/or_agr_huc8_1992.dbf');colnames(ag.huc8.1992)[5] = 'ag.huc8'
wet.huc8.1992 = read.dbf('LandUse_RasterData/or_wetl_huc8_1992.dbf');colnames(wet.huc8.1992)[5] = 'wet.huc8'
forst.huc8.1992 = read.dbf('LandUse_RasterData/or_forst_huc8_1992.dbf');colnames(forst.huc8.1992)[5] = 'forst.huc8'
dev.huc8.1992 = read.dbf('LandUse_RasterData/or_dev_huc8_1992.dbf');colnames(dev.huc8.1992)[5] = 'dev.huc8'

cover.1992 = join_all(list(ag.huc8.1992,wet.huc8.1992,forst.huc8.1992,dev.huc8.1992))
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
crop.huc8.2001 = read.dbf('LandUse_RasterData/crop_huc8_2001.dbf');colnames(crop.huc8.2001)[5] = 'crop.huc8'
past.huc8.2001 = read.dbf('LandUse_RasterData/past_huc8_2001.dbf');colnames(past.huc8.2001)[5] = 'past.huc8'
wet.huc8.2001 = read.dbf('LandUse_RasterData/wetl_huc8_2001.dbf');colnames(wet.huc8.2001)[5] = 'wet.huc8'
forst.huc8.2001 = read.dbf('LandUse_RasterData/forst_huc8_2001.dbf');colnames(forst.huc8.2001)[5] = 'forst.huc8'
dev.huc8.2001 = read.dbf('LandUse_RasterData/dev_huc8_2001.dbf');colnames(dev.huc8.2001)[5] = 'dev.huc8'
ag.huc8.2001 = crop.huc8.2001
ag.huc8.2001$ag.huc8 = crop.huc8.2001$crop.huc8+
  past.huc8.2001$past.huc8[match(crop.huc8.2001$HUC8,past.huc8.2001$HUC8)]

cover.2001 = join_all(list(ag.huc8.2001,wet.huc8.2001,forst.huc8.2001,dev.huc8.2001))
cover.2001$YEAR = 2001
cover.2001 = dplyr::select(cover.2001,-c(COUNT,AREA,ZONE_CODE,crop.huc8))
cover.2002 = cover.2001; cover.2002$YEAR = 2002
cover.2003 = cover.2001; cover.2003$YEAR = 2003
cover.2004 = cover.2001; cover.2004$YEAR = 2004
cover.2005 = cover.2001; cover.2005$YEAR = 2005

#2006 nlcd
crop.huc8.2006 = read.dbf('LandUse_RasterData/crop_huc8_2006.dbf');colnames(crop.huc8.2006)[5] = 'crop.huc8'
past.huc8.2006 = read.dbf('LandUse_RasterData/past_huc8_2006.dbf');colnames(past.huc8.2006)[5] = 'past.huc8'
wet.huc8.2006 = read.dbf('LandUse_RasterData/wetl_huc8_2006.dbf');colnames(wet.huc8.2006)[5] = 'wet.huc8'
forst.huc8.2006 = read.dbf('LandUse_RasterData/forst_huc8_2006.dbf');colnames(forst.huc8.2006)[5] = 'forst.huc8'
dev.huc8.2006 = read.dbf('LandUse_RasterData/dev_huc8_2006.dbf');colnames(dev.huc8.2006)[5] = 'dev.huc8'
ag.huc8.2006 = crop.huc8.2006
ag.huc8.2006$ag.huc8 = crop.huc8.2006$crop.huc8+past.huc8.2006$past.huc8[match(crop.huc8.2006$HUC8,past.huc8.2006$HUC8)]


cover.2006 = join_all(list(ag.huc8.2006,wet.huc8.2006,forst.huc8.2006,dev.huc8.2006))
cover.2006$YEAR = 2006
cover.2006 = dplyr::select(cover.2006,-c(COUNT,AREA,ZONE_CODE,crop.huc8))
cover.2007 = cover.2006; cover.2007$YEAR = 2007
cover.2008 = cover.2006; cover.2008$YEAR = 2008
cover.2009 = cover.2006; cover.2009$YEAR = 2009
cover.2010 = cover.2006; cover.2010$YEAR = 2010

#2011 nlcd
crop.huc8.2011 = read.dbf('LandUse_RasterData/crop_huc8_2011.dbf');colnames(crop.huc8.2011)[5] = 'crop.huc8'
past.huc8.2011 = read.dbf('LandUse_RasterData/past_huc8_2011.dbf');colnames(past.huc8.2011)[5] = 'past.huc8'
wet.huc8.2011 = read.dbf('LandUse_RasterData/wetl_huc8_2011.dbf');colnames(wet.huc8.2011)[5] = 'wet.huc8'
forst.huc8.2011 = read.dbf('LandUse_RasterData/forst_huc8_2011.dbf');colnames(forst.huc8.2011)[5] = 'forst.huc8'
dev.huc8.2011 = read.dbf('LandUse_RasterData/dev_huc8_2011.dbf');colnames(dev.huc8.2011)[5] = 'dev.huc8'
ag.huc8.2011 = crop.huc8.2011
ag.huc8.2011$ag.huc8 = crop.huc8.2011$crop.huc8+past.huc8.2011$past.huc8[match(crop.huc8.2011$HUC8,past.huc8.2011$HUC8)]
cover.2011 = join_all(list(ag.huc8.2011,wet.huc8.2011,forst.huc8.2011,dev.huc8.2011))
cover.2011$YEAR = 2011
cover.2011 = dplyr::select(cover.2011,-c(COUNT,AREA,ZONE_CODE,crop.huc8))
cover.2012 = cover.2011; cover.2012$YEAR = 2012
cover.2013 = cover.2011; cover.2013$YEAR = 2013


land.cover.huc8 = join_all(list(cover.1990,cover.1991,cover.1992,cover.1993,cover.1994,
                                cover.1995,cover.1996,cover.1997,cover.1998,cover.1999,
                                cover.2000,cover.2001,cover.2002,cover.2003,cover.2004,
                                cover.2005,cover.2006,cover.2007,cover.2008,cover.2009,
                                cover.2010,cover.2011,cover.2012,cover.2013),
                           type='full')

names(oregon.huc8.df)[1] = 'HUC8'

oregon.huc8.df = join(oregon.huc8.df,land.cover.huc8,type='left')


######################
#ADD GRANT DATA
######################


proj.info = read.xls('TempData/OWRI_ExportToExcel_011315/OwriDbExcel_1of3.xls',sheet='XlsProjectInfo')
proj.goal= read.xls('TempData/OWRI_ExportToExcel_011315/OwriDbExcel_1of3.xls',sheet='XlsGoal')
proj.goal = select(proj.goal, -ProjectID)
proj.info = join(proj.info,proj.goal,type='left')






proj.partners= read.xls('TempData/OWRI_ExportToExcel_011315/OwriDbExcel_1of3.xls',sheet='XlsParticipant')




#####load detailed grant data###
dat<-read.csv('Input/oweb_download_grant.csv')
sub = dat[as.character(dat$Project.Number) %in% proj.info$drvdOwebNum,]
head(sub)

dat$
dat$Project.ID[dat$Project.Type=='Restoration']


proj.info$drvdOwebNum

proj.partners$Category = ifelse(proj.partners$ParticipantType %in% public.list,'Public',
                                ifelse(proj.partners$ParticipantType %in% wc.list,'WC','Other'))

table(proj.partners$ProjectID,proj.partners$Category)

head(temp)
table(proj.partners$ProjectID)

table(proj.partners$ParticipantType)
names(proj.info)
head(proj.info)
head(proj.info$drvdOwebNum)
proj.info$drvdOwebNum
head(p)

temp = filter(proj.partners,ParticipantType=='watershed council')




public.list = c('county','city','state agency or jobs program (or ownership), state universities',
                'Soil & Water Conservation District',
                'federal agency or jobs program (or ownership)',
                'Extension Service (e.g. OSU Extension)')
wc.list = 'watershed council'




table(sub$Project.Type)
head(dat)
list.files()
read.csv('Input/')


dat$Proj.Paste<-paste(paste(paste(paste(paste(paste(paste(paste(paste(paste(dat$Project.Summary,dat$X,sep=' '),
                                                                      dat$X.1,sep=''),dat$X.2,sep=''),dat$X.3, sep=''),dat$X.4, sep =''),
                                              dat$X.5, sep = ''), dat$X.6, sep = ''), dat$X.7, sep = ''),
                            dat$X.8, sep = ''), dat$X.9, sep = '')

levels(dat$Region) <- c('NW','SW','WIL','CENT','EAST','MID','SW')

dat$Award.Date<-as.Date(dat$Award.Date,format = '%m/%d/%y')
dat$Project.Start.Date<-as.Date(dat$Project.Start.Date,format = '%m/%d/%y')
dat$Project.End.Date<-as.Date(dat$Project.End.Date,format = '%m/%d/%y')

dat$Grantee1<-NA
dat$Grantee2<-NA

#########load spatial grant data
oweb.grants = readOGR(dsn="/homes/tscott1/win/user/quinalt_original/OWEB_grants", layer="OWEB_Grants_8-4-2014")





oweb.grants@data$id = rownames(oweb.grants@data)

oregon.huc8 = readOGR(dsn="H:/quinalt/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)
oregon.huc8.points = fortify(oregon.huc8, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")

temp.which.huc8 = over(spTransform(oweb.grants,CRSobj=CRS(proj4string(oregon.huc8))),oregon.huc8)
oweb.grants$HUC8 = temp.which.huc8$HUC8

oweb.grants@data = data.frame(oweb.grants@data,dat[match(oweb.grants$gr_project,dat$Project.ID),])

grant.temp = oweb.grants@data
param.temp = all.params.spdf@data


levels(grant.temp$project_ty)[8] = 'Education.Outreach'
levels(grant.temp$project_ty)[5] = 'Education.Outreach'

grant.temp = grant.temp[grant.temp$Grantee.Type=='Watershed Council',]
grant.temp = grant.temp[grant.temp$project_ty!='Research',]

require(lubridate);require(reshape2)
grant.temp$Project.Start.Date = ymd(grant.temp$Project.Start.Date)
grant.temp$Project.End.Date = ymd(grant.temp$Project.End.Date)
grant.temp$Start.Year = year(grant.temp$Project.Start.Date)
grant.temp$End.Year = year(grant.temp$Project.End.Date)
grant.temp$End.Month = month(grant.temp$Project.End.Date)
grant.temp$Start.Month = month(grant.temp$Project.Start.Date)
grant.temp$Award.Year = year(grant.temp$Award.Date)
grant.temp = filter(grant.temp,!is.na(End.Year))

#####MAKE TABLE SUMMARIZING GRANTS
dat2 = grant.temp[grant.temp$Project.Type%in%levels(grant.temp$Project.Type)[c(2,3,5,8,9,12,14)],]
dat2$Project.Type = as.factor(as.character(dat2$Project.Type))
dat2 = dat2[dat2$Start.Year<=2013,]
monthlength=(dat2$Project.Start.Date %--% dat2$Project.End.Date) 
dat2$projlengthmonths = as.period(monthlength)%/%months(1)
levels(dat2$Project.Type) = c('Monitoring/Assessment/Tech. Assistance','Council Support',
                              'Education/Outreach','Monitoring/Assessment/Tech. Assistance',
                              'Education/Outreach','Restoration','Monitoring/Assessment/Tech. Assistance')
grant.table = join(tally(group_by(dat2,Project.Type)),
                   dat2 %>% group_by(Project.Type) %>% summarise(round(mean(Project.Amount),2)))
grant.table$Project.Type = as.character(grant.table$Project.Type)
tem = dat2 %>% group_by(Project.Type) %>% summarise(round(mean(projlengthmonths),2))
grant.table = join(grant.table,tem)

colnames(grant.table) = c('Project Type','N','Avg. Amount ($)','Avg. Length (months)')
grant.table[5,] = c('All Grants',sum(grant.table$N),round(mean(dat2$Project.Amount),2),round(mean(dat2$projlengthmonths),2))

rownames(grant.table) = grant.table[,1]
grant.table = grant.table[,-1]
