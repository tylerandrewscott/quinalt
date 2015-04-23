install.packages('RODBC')
setwd('H:/')
library(RODBC)
library(plyr)
library(rgdal)
library(reshape2)
library(maptools)
library(ggplot2)
library(doParallel)
library(lubridate)
library(sp)

#set up channel (need special workound since I'm on 64bit R)
cl<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/TylerScottQueryResults.accdb")
#read in entire table (it's not THAT big)
all<-sqlFetch(cl,"tblTylerScottQuery")
#post 1990
allpost90<-all[all$SAMPLE_DATETIME<'1990-01-01',]
allpost90$PARAMETER<-tolower(allpost90$PARAMETER)
allpost90$YEAR<-year(allpost90$SAMPLE_DATE)

uqdf<- function(df)
{
  temp<-ddply(df, "STATION_KEY", summarise,
        duration = max(YEAR) - min(YEAR),
        y0=min(YEAR),
        Y1=max(YEAR),
        nobs = length(unique(SAMPLE_DATETIME)),
        long = median(DECIMAL_LONG),
        lat = median(DECIMAL_LAT),
        parameter = names(which.max(table(PARAMETER))))
  return(temp)
}


turb<-allpost90[grep('field  turbidity',allpost90$PARAMETER,ignore.case=T),]
turb.uq<-uqdf(turb)
phos<-allpost90[grep('phos',allpost90$PARAMETER,ignore.case=T),]
phos.uq<-uqdf(phos)
phos.uq$parameter<-'phos'
nitr<-allpost90[grep('nitr',allpost90$PARAMETER,ignore.case=T),]
nitr.uq<-uqdf(nitr)
nitr.uq$parameter<-'nitr'
cond<-allpost90[grep('field  conductivity',allpost90$PARAMETER,ignore.case=T),]
cond.uq<-uqdf(cond)
sodi<-allpost90[grep('dissolved  sodium',allpost90$PARAMETER,ignore.case=T),]
sodi.uq<-uqdf(sodi)
ph<-allpost90[grep('field  ph',allpost90$PARAMETER,ignore.case=T),]
ph.uq<-uqdf(ph)
tss<-allpost90[grep('suspended',allpost90$PARAMETER,ignore.case=T),]
tss.uq<-uqdf(tss)
all.uq<-rbind(tss.uq,turb.uq,nitr.uq,phos.uq,ph.uq,sodi.uq,cond.uq)

write.csv(all.uq,'H:/quinalt/unique_sites.csv')
write.csv(allpost90, 'H:/quinalt/oregon_obs.csv')


oregon = readOGR(dsn="/government_units", layer="state_nrcs_a_or")
oregon@data$id = rownames(oregon@data)
oregon.points = fortify(oregon, region="id")
oregon.df = join(oregon.points, oregon@data, by="id")

oregon.huc8 = readOGR(dsn="/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)
oregon.huc8.points = fortify(oregon.huc8, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")


ggplot()+geom_polygon(aes(long,lat,group=group),alpha=0,colour='black',data=oregon.df)+coord_equal()+
  geom_polygon(aes(long,lat,group=group),alpha=0,colour='grey',data=oregon.huc8.df)+
  geom_point(data=all.uq,aes(x=long,y=lat,size=nobs,colour=parameter),pch=21,alpha=.9,position='jitter')+
  scale_colour_brewer(type='qual',palette=2)
      
all.uq<-all.uq[is.na(all.uq$long)==FALSE,]
coordinates(all.uq)<-c('long','lat')


spTransform(all.uq,CRS(oregon.huc8))

all.uq[is.na(all.uq$long),]

all[all$STATION_KEY==24344,]


test<-sapply(over(oregon.huc8, all.uq, returnList = TRUE), length)





SpatialPoints(cbind(all.uq$lat,all.uq$long))

head(turb.uq)
unique(all.uq$parameter)

sum(turb$STATION_KEY==turb.uq$STATION_KEY[751])



head(turb.uq)
head(turb.uq)
help(package='plyr')


state_nrcs_a_or

hist(table(turb$STATION_KEY))

tabulate(as.factor(turb$STATION_KEY))

library(ggplot2)

list.files()




stack(table(allpost90$PARAMETER))
?tabulate


table(allpost90$PARAMETER)
unique(all$PARAMETER)
table(all$PARAMETER)
?grep

head(test)
turb<-sqlQuery(cl,
"select * from tblTylerScottQuery where SAMPLE_DATETIME< '1990-01-01' & PARAMETER = 'Turbidity'")





dim(turb)
head(turb)
(turb$SAMPLE_DATETIME)<"1990-01-01"
turb$SAMPLE_DATETIME[1]

,
         "where PARAMETER = 'Turbidity' ")
sqlQuery(channel, "select * from pg_tables where tableowner='ripley'")

data <- sqlQuery( cl , paste ("select *
 from Name_of_table_in_my_database"))

lon<-sqlQuery(cl,"select DECIMAL_LA, DECIMAL_LC where PARAMETER == Turbidity")
lon


sqlQuery(channel, 
         paste("select State, Murder from USArrests",
                       
               "where Rape > 30 order by Murder"))
?sqlQuery
library(dplyr)
