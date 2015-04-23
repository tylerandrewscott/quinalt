

####READ IN WATER QUALITY DATA, PRODUCE SPDF
###Also adds in owri spending by year, cumulative for each HUC8
setwd('H:/quinalt')
load('midpoint.0.RData')
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

oregon = readOGR(dsn="H:/quinalt/government_units", layer="state_nrcs_a_or")
oregon@data$id = rownames(oregon@data)
oregon.points = fortify(oregon, region="id")
oregon.df = join(oregon.points, oregon@data, by="id")

oregon.huc8 = readOGR(dsn="H:/quinalt/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)

#set up channel (need special workound since I'm on 64bit R)
cl<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/TylerScottQueryResults.accdb")
#read in entire table (it's not THAT big)
all<-sqlFetch(cl,"tblTylerScottQuery")
#post 1990
all$PARAMETER <- tolower(all$PARAMETER)
all$YEAR<-year(all$SAMPLE_DATETIME)
allpost90<-all[all$YEAR>=1990,]
allpost90$ddate<-decimal_date(allpost90$SAMPLE_DATETIME)

uqdf<- function(df)
{
  temp<-ddply(df, "STATION_KEY", summarise,
        duration = max(ddate) - min(ddate),
        y0=min(YEAR),
        Y1=max(YEAR),
        nobs = length(unique(SAMPLE_DATETIME)),
        long = median(DECIMAL_LONG),
        lat = median(DECIMAL_LAT),
        parameter = names(which.max(table(PARAMETER))))
  return(temp)
}


phos<-allpost90[grep('phos',allpost90$PARAMETER,ignore.case=T),]
phos$stdresult<-scale(phos$RESULT,center=T,scale=T)
phos$PARAMETER = 'phos'
phos = phos[!is.na(phos$DECIMAL_LAT)&!is.na(phos$DECIMAL_LONG),]
phos = phos[phos$UNIT == 'mg/L',]

turb<-allpost90[grep('turbidity',allpost90$PARAMETER,ignore.case=T),]
turb$stdresult<-scale(turb$RESULT,center=T,scale=T)
turb$PARAMETER = 'turb'
turb = turb[turb$UNIT=='NTU',]
turb = turb[!is.na(turb$DECIMAL_LAT)&!is.na(turb$DECIMAL_LONG),]

nitr<-allpost90[grep('nitr',allpost90$PARAMETER,ignore.case=T),]
nitr$stdresult<-scale(nitr$RESULT,center=T,scale=T)
nitr$PARAMETER = 'nitr'
nitr = nitr[nitr$UNIT=='mg/L',]
nitr = nitr[!is.na(nitr$DECIMAL_LAT)&!is.na(nitr$DECIMAL_LONG),]

tss<-allpost90[grep('suspended',allpost90$PARAMETER,ignore.case=T),]
tss$stdresult<-scale(tss$RESULT,center=T,scale=T)
tss$PARAMETER = 'tss'
tss = tss[tss$UNIT=='mg/L',]
tss = tss[!is.na(tss$DECIMAL_LAT)&!is.na(tss$DECIMAL_LONG),]

cond<-allpost90[grep('conductivity',allpost90$PARAMETER,ignore.case=T),]
cond$stdresult<-scale(cond$RESULT,center=T,scale=T)
cond$PARAMETER = 'cond'
cond$UNIT[cond$UNIT %in% levels(cond$UNIT)[grep('/cm',levels(cond$UNIT))]] <- levels(cond$UNIT)[grep('/cm',levels(cond$UNIT))][1]
cond = cond[!is.na(cond$DECIMAL_LAT)&!is.na(cond$DECIMAL_LONG),]

sodi<-allpost90[grep('dissolved  sodium',allpost90$PARAMETER,ignore.case=T),]
sodi$stdresult<-scale(sodi$RESULT,center=T,scale=T)
sodi$PARAMETER = 'sodi'
sodi = sodi[sodi$UNIT=='mg/L',]
sodi = sodi[!is.na(sodi$DECIMAL_LAT)&!is.na(sodi$DECIMAL_LONG),]

#ph<-allpost90[grep('field  ph',allpost90$PARAMETER,ignore.case=T),]
#ph$stdresult<-scale(ph$RESULT,center=T,scale=T)
#ph$PARAMETER = 'ph'
#ph = ph[ph$UNIT=='SU',]
#ph = ph[!is.na(ph$DECIMAL_LAT)&!is.na(ph$DECIMAL_LONG),]


all.params.df = join_all(list(phos,nitr,cond,turb,tss,sodi),type='full')

all.params.spdf = SpatialPointsDataFrame(coords = matrix(cbind(all.params.df$DECIMAL_LONG,
                       all.params.df$DECIMAL_LAT),ncol=2),
       data=all.params.df,proj4string=CRS("+datum=NAD83 +proj=longlat"))

temp = all.params.spdf@data


dat = read.csv('Scott_OWQI_1980_2013.csv')
dat = dat %>% filter(water_yr>1990)

dat$YEAR = dat$water_yr
dat$MONTH =   month(mdy(dat$Date),label=TRUE)

n_stat = length(unique(dat$Station))
n_year = length(unique(dat$water_yr))

dat$monthyr = paste(dat$MONTH,dat$water_yr)



###########

my = NULL

for (i in 1990:2013)
{
  my = c(my,paste(month(1:12,label=TRUE),i))
}
obs_matrix = matrix(0,ncol=length(my),nrow=n_stat)
colnames(obs_matrix) = my
rownames(obs_matrix) = sort(unique(dat$Station))


for (i in 1:nrow(dat))
{
  obs_matrix[match(dat$Station[i],rownames(obs_matrix)),match(dat$monthyr[i],colnames(obs_matrix))] = dat$owqi[i]
}  

####code for forward fill 

obs_matrix[obs_matrix==0] = NA

##forward fill NA observations (all but first column)
#    for (i in 1:nrow(obs_matrix))
#    {
#   for (j in 2:ncol(obs_matrix))
#   {
#     if(is.na(obs_matrix[i,j])){obs_matrix[i,j] = obs_matrix[i,(j-1)]}
#   }
#  }

#for (i in 1:nrow(obs_matrix))
#{
#  for (j in (ncol(obs_matrix)-1):1)
#  {
#    if(is.na(obs_matrix[i,j])){obs_matrix[i,j] = obs_matrix[i,(j+1)]}
#  }
#}

wide.response = as.data.frame(obs_matrix)
wide.response$Station = rownames(wide.response)

long = melt(obs_matrix)
colnames(long) = c('STATION','MonthYear','owqi')

long$YEAR = gsub(".* ","",long$MonthYear)
long$MONTH = gsub(" .*","",long$MonthYear)
temp.sub = temp[,c('STATION_KEY','LOCATION_DESCRIPTION','DECIMAL_LAT','DECIMAL_LONG')]

colnames(temp.sub)[1] = 'STATION'
long = filter(long,!is.na(owqi))
long = plyr::join(long,temp.sub,match='first')
long = filter(long,!is.na(long$DECIMAL_LAT))


all.params.spdf = SpatialPointsDataFrame(coords=matrix(cbind(long$DECIMAL_LONG,long$DECIMAL_LAT),ncol=2),
                                         data = long,proj4string=CRS("+datum=NAD83 +proj=longlat"))

which.huc8.point = over(spTransform(all.params.spdf,CRS(proj4string(oregon.huc8))),oregon.huc8)

all.params.spdf@data$HUC8 <- which.huc8.point$HUC8
all.params.spdf@data$HUC8.AreaSqKm <- which.huc8.point$AreaSqKm

all.params.spdf@data$HUC8 = as.character(all.params.spdf@data$HUC8)
all.params.spdf@data$YEAR = as.numeric(all.params.spdf@data$YEAR)

huc8.projects.df$MONTH=month(huc8.projects.df$MONTH,label=TRUE)
colnames(huc8.projects.df)[2] = 'HUC8'

all.params.spdf@data = dplyr::left_join(all.params.spdf@data,huc8.projects.df)

#start huc8 data table for predictions with model
huc8.database = join(huc8.projects.df,oregon.huc8@data)

rm(list=ls()[intersect(grep('huc8.database',ls(),invert=TRUE),grep('all.params.spdf',ls(),invert=TRUE))])
save.image('midpoint.1b.RData')

#write.csv(all.params.spdf@data,'wq.data.plotting.csv')
##