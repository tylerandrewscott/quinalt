install.packages('RODBC')
setwd('H:/quinalt')
rm(list=ls())
library(RODBC)
library(plyr)
library(rgdal)
library(reshape2)
library(maptools)
library(ggplot2)
library(doParallel)
library(lubridate)
library(sp)
library(ggmap)
library(lme4)
require(gridExtra)
require(lattice)
require(ggplot2)
require(splancs)
require(fields)


#set up channel (need special workound since I'm on 64bit R)
cl<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=H:/TylerScottQueryResults.accdb")
#read in entire table (it's not THAT big)
all<-sqlFetch(cl,"tblTylerScottQuery")
#post 1990
all$PARAMETER <- tolower(all$PARAMETER)
all$YEAR<-year(all$SAMPLE_DATETIME)
allpost90<-all[all$YEAR>=1990,]


allpost90$ddate<-decimal_date(allpost90$SAMPLE_DATE)


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


turb<-allpost90[grep('turbidity',allpost90$PARAMETER,ignore.case=T),]
turb$stdresult<-scale(turb$RESULT,center=T,scale=T)
turb$PARAMETER = 'turb'
turb.uq<-uqdf(turb)
phos<-allpost90[grep('phos',allpost90$PARAMETER,ignore.case=T),]
phos$stdresult<-scale(phos$RESULT,center=T,scale=T)
phos$PARAMETER = 'phos'
phos = phos[!is.na(phos$DECIMAL_LAT)&!is.na(phos$DECIMAL_LONG),]
phos = SpatialPointsDataFrame(coords = matrix(cbind(phos$DECIMAL_LONG,phos$DECIMAL_LAT),ncol=2),data=phos,proj4string=CRS("+datum=NAD83 +proj=longlat"))
phos.uq<-uqdf(phos)
phos.uq$parameter<-'phos'

nitr<-allpost90[grep('nitr',allpost90$PARAMETER,ignore.case=T),]
nitr$stdresult<-scale(nitr$RESULT,center=T,scale=T)
nitr$PARAMETER <- 'nitr'
nitr.uq<-uqdf(nitr)
nitr.uq$parameter<-'nitr'
cond<-allpost90[grep('conductivity',allpost90$PARAMETER,ignore.case=T),]
cond$stdresult<-scale(cond$RESULT,center=T,scale=T)
cond$PARAMETER = 'cond'
cond.uq<-uqdf(cond)
sodi<-allpost90[grep('dissolved  sodium',allpost90$PARAMETER,ignore.case=T),]
sodi$stdresult<-scale(sodi$RESULT,center=T,scale=T)
sodi$PARAMETER = 'sodi'
sodi.uq<-uqdf(sodi)
ph<-allpost90[grep('field  ph',allpost90$PARAMETER,ignore.case=T),]
ph$stdresult<-scale(ph$RESULT,center=T,scale=T)
ph$PARAMETER = 'ph'
ph.uq<-uqdf(ph)
tss<-allpost90[grep('suspended',allpost90$PARAMETER,ignore.case=T),]
tss$stdresult<-scale(tss$RESULT,center=T,scale=T)
tss$PARAMETER = 'tss'
tss.uq<-uqdf(tss)

all.params = join_all(list(phos,nitr,cond,turb,tss,ph,sodi),type='full')


oregon = readOGR(dsn="H:/quinalt/government_units", layer="state_nrcs_a_or")
oregon@data$id = rownames(oregon@data)
oregon.points = fortify(oregon, region="id")
oregon.df = join(oregon.points, oregon@data, by="id")

oregon.huc8 = readOGR(dsn="H:/quinalt/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)


which.huc8.phos = over(spTransform(phos,CRS(proj4string(oregon.huc8))),oregon.huc8)
phos@data$HUC8 <- which.huc8.phos$HUC8


source("http://www.math.ntnu.no/inla/givemeINLA.R")

coordinates(phos)<- matrix(cbind(round(phos$DECIMAL_LONG,3),round(phos$DECIMAL_LAT,3)),ncol=2)

m1 <- inla.mesh.2d(coordinates(phos), max.edge=c(.45,1), cutoff=0.2)

A <- inla.spde.make.A(m1, loc=coordinates(phos))
spde <- inla.spde2.matern(m1, alpha=2)


coords = coordinates(phos)
mesh.index <- inla.spde.make.index(name="field",
                                     n.spde=spde$n.spde)

stk.dat <- inla.stack(data=list(y=phos@data$RESULT), A=list(A,1), tag ="est",
                        effects=list(c(mesh.index,list(Intercept=1)),
                                     list(long=inla.group(coordinates(phos)[,1]),
                                          lat=inla.group(coordinates(phos)[,2]))))
                                          
f.s <- y ~ -1 + Intercept  +
  f(field, model=spde)

r.s <- inla(f.s, family="Gamma",
            data=inla.stack.data(stk.dat), verbose=TRUE,
            control.predictor=list(A=inla.stack.A(stk.dat),
                                   compute=TRUE))

                                          ,
                                          HUC8=inla.group(phos@data$HUC8))))


coordinates(phos)[,1]


?inla.mesh.2d
library(gridExtra); library(ggplot2); library(lattic
                                              e)
> library(INLA); library(splancs); library(fields)

install.packages('INLA')
hist(ph$RESULT)
hist(sodi$RESULT)
summary(sodi$RESULT)

ggplot(sodi)+geom_density(aes(x=RESULT))
sodi
?geom_density

all.uq<-rbind(tss.uq,turb.uq,nitr.uq,phos.uq,ph.uq,sodi.uq,cond.uq)
all.uq<-all.uq[is.na(all.uq$long)==FALSE,]
turb$long<-turb$DECIMAL_LONG
turb$lat<-turb$DECIMAL_LAT
coordinates(all.uq)<-c('long','lat')
coordinates(turb.uq)<-c('long','lat')
coordinates(turb)<-c('long','lat')


##################
##Read in Oregon Spatial Data
###################
oregon = readOGR(dsn="H:/quinalt/government_units", layer="state_nrcs_a_or")
oregon@data$id = rownames(oregon@data)
oregon.points = fortify(oregon, region="id")
oregon.df = join(oregon.points, oregon@data, by="id")

oregon.huc8 = readOGR(dsn="H:/quinalt/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)
oregon.huc8.points = fortify(oregon.huc8, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")


oregon.wc = readOGR(dsn="H:/quinalt/watershed_councils", layer="OregonWatershedCouncils")
oregon.wc@data$id = rownames(oregon.wc@data)
oregon.wc<-spTransform(oregon.wc,CRS(proj4string(oregon.huc8)))
oregon.wc.points = fortify(oregon.wc, region="id")
oregon.wc.df = join(oregon.wc.points, oregon.wc@data, by="id")

oregon.cty = readOGR(dsn="H:/quinalt/government_units", layer="county_nrcs_a_or")
oregon.cty@data$id = rownames(oregon.cty@data)
oregon.cty.points = fortify(oregon.cty, region="id")
oregon.cty.df = join(oregon.cty.points, oregon.cty@data, by="id")

########################################

proj4string(turb.uq)<-CRS("+proj=longlat")
turb.uq.t<-sp::spTransform(turb.uq,CRS(proj4string(oregon.huc8)))
turb.huc8<-over(turb.uq.t,oregon.huc8)

turb.wc<-over(turb.uq.t,oregon.wc)

turb.sites<-cbind(turb.huc8,turb.uq)
turb2<-join(turb,turb.sites,by='STATION_KEY',type="left")

m<-lmer(RESULT ~ ddate + (1|YEAR) +(1|HUC8),data=turb2)

##############
###GRANT DATA####
##############
dat<-read.csv('oweb_download_grant.csv')
dat$Proj.Paste<-paste(paste(paste(paste(paste(paste(paste(paste(paste(paste(dat$Project.Summary,dat$X,sep=' '),
                                                                      dat$X.1,sep=''),dat$X.2,sep=''),dat$X.3, sep=''),dat$X.4, sep =''),
                                              dat$X.5, sep = ''), dat$X.6, sep = ''), dat$X.7, sep = ''),
                            dat$X.8, sep = ''), dat$X.9, sep = '')

levels(dat$Region) <- c('NW','SW','WIL','CENT','EAST','MID','SW')

dat$Start<-(mdy(dat$Project.Start.Date))
dat$End<-(mdy(dat$Project.End.Date))
dat$SYear<-year(dat$Start)
dat<-dat[!is.na(dat$Start)|!is.na(dat$End),]
wcg.df<-dat[grep(' WC',dat$Grantee),]
wcg.df$Start<-decimal_date(wcg.df$Start)
wcg.df$End<-decimal_date(wcg.df$End)

grant.matrix<-matrix(0,nrow=length(unique(wcg.df$Grantee)),ncol=length(seq(1990,2017,(1/12))))
colnames(grant.matrix)<-seq(1990,2017,(1/12))
rownames(grant.matrix)<-sort(unique(wcg.df$Grantee))

sort(unique(wcg.df$Grantee))
for (i in 1:nrow(grant.matrix))
{
  for (j in 1:ncol(grant.matrix))
  {
    grant.matrix[i,j]<-sum(wcg.df$Project.Amount[
      wcg.df$Grantee==rownames(grant.matrix)[i]&
      wcg.df$Start<=colnames(grant.matrix)[j]&
        wcg.df$End>=colnames(grant.matrix)[j]])
  }
}
head(thepage)
thepage
wcg.df[wcg.df$Grantee=='Applegate Partnership & WC',]
save.image('oregon.5-29-14.RData')
load('oregon.5-29-14.RData')
thepage = readLines('http://oregonwatersheds.org/councils')
mypattern1 = 'Watershed Council<'
datalines = grep(mypattern1,thepage,value=TRUE)


\
unlist(strsplit(
  
  unlist(strsplit(datalines, "\">"))
  unlist(strsplit(datalines, "Watershed Council"))
  ,"</a"))[grep('Watershed Council',unlist(strsplit(unlist(strsplit(datalines, "\">")),"</a>")))]


getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(mypattern,datalines)
matches = mapply(getexpr,datalines,gg)

result = gsub(mypattern,'\1',matches)
names(result) = NULL


result
head(grant.matrix)
head(names(wcg.df)
wcg.df$Start

test<-apply(grant.matrix,MARGIN=c(1,2), 
      sum(wcg.df$Project.Amount[wcg.df$Grantee==rownames(grant.matrix)&
                                  wcg.df$SYear==colnames(grant.matrix)]))

t<-wcg.df[wcg.df$Grantee==unique(test$Grantee)[1],]


foreach(i in 1:nrow(t))
?foreach






t$Project.Amount
t$Start
t$End

tt<-seq(1990,2013,1)


t<-melt(test, id.vars = "Grantee", measure.vars = c("Start", "End",'Project.Amount','Project.Type'))
melt(test)
head(t)




wcg.df[is.na(wcg.df$Project.End.Date),]
?decimal_date
class(wcg.df$Project.Start.Date)
wcgrantee<-as.list(unique(as.character(wcg.df$Grantee)))
names(wcg.df)
month(dat$Project.Start.Dat)
head(wcg.df$Project.Start.Date)
decimal_date(wcg.df$Project.Start.Date)
class(wcg.df$Project.Start.Date)

start<-"01-01-1990"
mdy(start) + wcg.df$Project.Start.Date

install.packages('xts')
library(xts)
ldply(.data,)
?ldply




table(wcg.df$Project.Type)


dat$Grantee1<-NA
dat$Grantee2<-NA


head(wcg.df)

lubridate(wcg.df$Project.Start.Date)
help(package='lubridate')
wcg.df$Grantee[grep('-',wcg.df$Grantee)]




date_decimal(wcg.df$Project.Start.Date)

?standardize
head(allpost90)




stdresult<-tapply(allpost90$RESULT,allpost90$PARAMETER,scale,center=T,scale=T)




?tapply
?scale






head(turb.sites)
?point.in.polygon
head(turb.huc8)
names(turb.huc8)
head(turb.uq.t)
help(package='sp')
turb.huc8[1:10,]
lmer(turb$RESULT~)




dim(turb.uq)
head(turb.huc8)
?over


identicalCRS(turb.uq.t,oregon.huc8)
proj4string(turb.uq)
proj4string(oregon.huc8)





?proj4string
#write.csv(all.uq,'H:/quinalt/unique_sites.csv')
#write.csv(allpost90, 'H:/quinalt/oregon_obs.csv')
names(allpost90)




?over
sample.locations.turb<- foreach(i = 1:nrow(turb.uq)) %do% revgeocode(c(turb.uq$long[i],turb.uq$lat[i]),output='more')
  head(sample.locations.turb)


turb.df<-ldply(sample.locations.turb)



turb.df$administrative_area_level_2



$administrative_area_level_2



names(sample.locations.turb[[1]])



revgeocode(all.uq[[1]])

geocodeQueryCheck
head(sample.locations)
all.uq$long[i]

oregon = readOGR(dsn="H:/quinalt/government_units", layer="state_nrcs_a_or")
oregon@data$id = rownames(oregon@data)
oregon.points = fortify(oregon, region="id")
oregon.df = join(oregon.points, oregon@data, by="id")

oregon.huc8 = readOGR(dsn="H:/quinalt/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)
oregon.huc8.points = fortify(oregon.huc8, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")


oregon.wc = readOGR(dsn="H:/quinalt/watershed_councils", layer="OregonWatershedCouncils")
oregon.wc@data$id = rownames(oregon.wc@data)
oregon.wc.points = fortify(oregon.wc, region="id")
oregon.wc.df = join(oregon.wc.points, oregon.wc@data, by="id")

oregon.cty = readOGR(dsn="H:/quinalt/government_units", layer="county_nrcs_a_or")
oregon.cty@data$id = rownames(oregon.cty@data)
oregon.cty.points = fortify(oregon.cty, region="id")
oregon.cty.df = join(oregon.cty.points, oregon.cty@data, by="id")


ggplot()+geom_polygon(aes(long,lat,group=group),alpha=0,colour='black',data=oregon.df)+coord_equal()+
  geom_polygon(aes(long,lat,group=group),alpha=0,colour='black',data=oregon.cty.df)
  #geom_polygon(aes(long,lat,group=group,colour=NOBS),alpha=0,data=oregon.huc8.df)+
  scale_colour_continuous(breaks=c(50,100,200,500))


wcgrant.df<-grant.df[grant.df$Grantee.Type=='Watershed Council',]
wcg.df<-grant.df[grep(' WC',grant.df$Grantee),]
head(wcg.df)



wcgrant.df$Grantee



head(all.uq)


table(as.character(wcgrant.df$Grantee))
wcgrant.df$Grantee
head(oregon.wc.df)
oregon.wc.df$instName<-gsub('Watershed Council','WC',oregon.wc.df$instName)

wcgrant.df$Grantee[wcgrant.df$Grantee %in% oregon.wc.df$instName==FALSE]


sort(unique(oregon.wc.df$instName))
'Walla Walla Basin WC', 'Walla Walla WC'

table(wcgrant.df$Grantee)
table(oregon.wc.df$instName)
table(grant.df$Grantee.Type)
grant.df<-read.csv('oweb_download_grant.csv')
grant.df[grant.df$County=='',]
stack(table(grant.df$County))
proj.per.cty<-foreach(i = 1:length(unique(as.character(oregon.cty.df$COUNTYNAME))) %do% sum(unique(as.character(oregon.cty.df$COUNTYNAME))[i]==as.character(grant.df$County))

grant.df$County
                          
head(oregon.cty.df)



                          high='green',low='grey')



?

+
  geom_point(data=all.uq,aes(x=long,y=lat,size=nobs,colour=parameter),pch=21,alpha=.9,position='jitter')+
  scale_colour_brewer(type='qual',palette=2)



?spTransform
class(oregon.huc8)
rgdal::CRS(oregon.huc8)
?CRS
spTransform(all.uq,CRS(oregon.huc8))

all.uq[is.na(all.uq$long),]

all[all$STATION_KEY==24344,]


obs.in.huc8<-sapply(over(oregon.huc8, all.uq, returnList = TRUE), length)
obs.in.huc8
?over
oregon.huc8$id

# EPSG strings
latlong = "+init=epsg:4326"
proj4string(oregon.huc8)
proj4string(all.uq)<-proj4string(oregon.huc8)

identicalCRS(oregon.huc8,all.uq)
CRS(latlong)

ukgrid = "+init=epsg:27700"
google = "+init=epsg:3857"

test<-all.uq %over% oregon.huc8
test1<-stack(table(test$Name));colnames(test1)<-c('NOBS','Name')
oregon.huc8.df<-join(oregon.huc8.df,test1)



table(test$Name)


head(test1)
head(oregon.huc8.df)

# Spatial*
proj4string(SPDF)

## [1] NA

proj4string(SPDF) = CRS(latlong)
SL1 = SpatialLines(list(Ls1), proj4string = CRS(latlong))

# Raster CRS
projection(r1)

## [1] "NA"

# - assign or set on creation
projection(r1) = CRS(latlong)
r1 = raster(list(x = x, y = y, z = z), crs = latlong)

# Transform Spatial*
SPtrans = spTransform(SPDF, CRS(google))

# Transform/Warp Raster
rTrans = projectRaster(r1, crs = google)




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
