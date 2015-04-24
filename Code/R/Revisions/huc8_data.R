
##CALCULATE OWRI ON RESTORATION PROJECT SPENDING BY HUC8 and YEAR
###################
First.Year = 1995
Last.Year = 2013
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


proj.info = read.csv('Input/owri_project_info.csv')
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


oregon.huc8.df$Restoration.Project.Total = 0
oregon.huc8.df$Restoration.Project.Cash = 0
oregon.huc8.df$Restoration.Project.InKind = 0

for (i in 1:nrow(proj.info))
{
oregon.huc8.df$Restoration.Project.Total[oregon.huc8.df$HUC8 == proj.info$drvdHUC4thField[i] &
  oregon.huc8.df$Abs.Month >= proj.info$Start.Abs[i]&
  oregon.huc8.df$Abs.Month <= proj.info$Complete.Abs[i]] = 
  proj.info$TotalBoth[i] / length(proj.info$Start.Abs[i]:proj.info$Complete.Abs[i])

oregon.huc8.df$Restoration.Project.Cash[oregon.huc8.df$HUC8 == proj.info$drvdHUC4thField[i] &
                                           oregon.huc8.df$Abs.Month >= proj.info$Start.Abs[i]&
                                           oregon.huc8.df$Abs.Month <= proj.info$Complete.Abs[i]] = 
proj.info$TotalCash[i] / length(proj.info$Start.Abs[i]:proj.info$Complete.Abs[i])

oregon.huc8.df$Restoration.Project.InKind[oregon.huc8.df$HUC8 == proj.info$drvdHUC4thField[i] &
                                          oregon.huc8.df$Abs.Month >= proj.info$Start.Abs[i]&
                                          oregon.huc8.df$Abs.Month <= proj.info$Complete.Abs[i]] = 
  proj.info$TotalInKind[i] / length(proj.info$Start.Abs[i]:proj.info$Complete.Abs[i])
}



7200/3
proj.info$TotalBoth[i] / nrow(.)



paste(Year.Month$Month.Num,Year.Month$YEAR)
paste(proj.info$StartMonth,proj.info$StartYear)
paste(proj.info$CompleteMonth,proj.info$CompleteYear)


proj.info$Complete.Abs = match(paste(proj.info$CompleteMonth,proj.info$CompleteYear), paste(Year.Month$Month.Num,Year.Month$YEAR))

head(Year.Month)
head(proj.info)

filter(oregon.huc8.df,oregon.huc8.df$YEAR == proj.info$S)

proj.info$StartMonth
proj.info$StartYear

proj.goal= read.csv('Input/owri_project_goal.csv')
proj.goal = select(proj.goal, -ProjectID)



proj.info = join(proj.info,proj.goal,type='left')
proj.info$Project.Number = proj.info$drvdOwebNum







head(data.frame(table(proj.goal$PROJNUM,proj.goal$Goal)))

proj.info$
oweb_records = read.csv('Input/oweb_download_grant.csv')
oweb_records$OWEB.Project=TRUE

proj.info =  join(proj.info,oweb_records,type='full')
proj.info$OWEB.Project[is.na(proj.info$OWEB.Project)] = FALSE

convertCurrency <- function(currency) {
  currency1 <- sub('$','',as.character(currency),fixed=TRUE)
  currency2 <- as.numeric(gsub('\\,','',as.character(currency1))) 
  currency2
}
proj.info$TotalCash <- convertCurrency(proj.info$TotalCash)
proj.info$TotalInKind <- convertCurrency(proj.info$TotalInKind)
proj.info$TotalBoth <- proj.info$TotalCash + proj.info$TotalInKind
proj.info$WQ = TRUE
proj.info$WQ[unique(c(grep('fish',proj.info$Goal,invert=F),grep('habitat',proj.info$Goal,invert=F),
      grep('groundwater',proj.info$Goal,invert=F)))] = FALSE


head(proj.info)
table(proj.info$OWEB.Project)
head(proj.info)


table(proj.info$OWEB.Project,proj.info$WQ[3] improve/increase stream pools                                            
[4] improve/increase stream structure & complexity                           
                            
improve/increase stream gravel recruitment                               
improve/increase stream refuge cover                                     
[9] increase future LWD recruitment to stream                                
[10] increase streambank stabilization/protection                             
[11] improve/increase stream interaction with floodplain                      
[12] other                                                                    
[13] improve/increase fish passage                                            
[14] improve/increase stream slow water habitat                               
[15] improve/increase stream cool water habitat                               
[16] improve/increase stream summer habitat                                   
[17] improve/increase stream flow                                             
[18] increase nutrient input to stream (e.g. plant material, salmon carcasses)
[19] decrease erosion/stream sedimentation                                    
[20] education                                                                
[21] decrease stream temperature                                              
[22] increase wildlife habitat (vegetation for food, cover, or nesting)       
[23] increase future stream shading/increase vegetation to provide shade      
[24] decrease livestock access to stream                                      
[25] increase road/upslope drainage                                           
[26] increase upslope stability (soil, road)                                  
[27] decrease road access                                                     
[28] decrease run-off contaminant input                                       
[29] improve flood/slide repair                                               
[30] unknown                                                                  
[31] decrease washout/diversion potential at stream crossings                 
[32] increase native plant species composition                                
[33] increase storage capacity of wetland                                     
[34] provide fish protection (e.g. by screening diversions)                   
[35] decrease road density                                                    
[36] increase net area of wetland                                             
[37] increase wetland vegetation for flood control                            
[38] increase the number of wetland types at site                             
[39] increase upland water storage capacity                                   
[40] increase wetland connection to adjacent natural area                     
[41] increase/improve water quality                                           
[42] increase/improve water conservation                                      
[43] increase vegetation to filter runoff                                     
[44] increase water to stream during low flows                                
[45] increase/improve aquatic habitat                                         
[46] decrease groundwater contaminant                                         
[47] increase/improve groundwater recharge                                    
 



table(proj.info$PASSAGE)

unique(proj.info$Goal)

interintersect(which(proj.info$drvdHUC4thField[1] == oregon.huc8.df$HUC8),
which(proj.info$StartYear[1] == oregon.huc8.df$YEAR))


proj.info$drvdHUC4thField[1]
oregon.huc8.df$MONTH.YEAR = paste(oregon.huc8.df$MONTH,oregon.huc8.df$YEAR)
master.month = seq(1,length(unique(oregon.huc8.df$MONTH.YEAR)),1)
data.frame(Master = master.mont,MONTH = )






oregon.huc8.df$MONTH.YEAR

month.abb[proj.info$CompleteMonth]

month.abb[proj.info$StartMonth]



gsub('$','',proj.info$TotalCash)
proj.info$TotalCash


proj.partners= read.csv('Input/owri_project_participants.csv')

public.list = c('county','city','state agency or jobs program (or ownership), state universities',
                'Soil & Water Conservation District',
                'federal agency or jobs program (or ownership)',
                'Extension Service (e.g. OSU Extension)')
wc.list = 'watershed council'
proj.partners$Category = ifelse(proj.partners$ParticipantType %in% public.list,'Public',
                                ifelse(proj.partners$ParticipantType %in% wc.list,'WC','Other'))

temp = table(proj.partners$PROJNUM,proj.partners$Category)

WC.projects = rownames(temp)[temp[,3] > 0]




rownames(temp)

head(temp)
head(temp)
colnames(temp) = c('PROJNUM','Category','Count')
WC.projects = temp %>% filter(Category == 'WC') %>% filter (Count > 0)
Public.projects = temp %>% filter(C)



WC.projects = temp$PROJNUM[(temp$Category=='WC'&temp$Count==0)][()]

(temp$Category=='Public')]

length(WC.projects)


#####load detailed grant data###

sub = dat[as.character(dat$Project.Number) %in% proj.info$drvdOwebNum,]
head(sub)

dat$
dat$Project.ID[dat$Project.Type=='Restoration']


proj.info$drvdOwebNum



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
