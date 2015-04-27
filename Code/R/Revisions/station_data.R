
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



dat = read.csv('Input/Scott_OWQI_1980_2013.csv')

oregon = readOGR(dsn="H:/quinalt/government_units", layer="state_nrcs_a_or")
oregon@data$id = rownames(oregon@data)
oregon.points = fortify(oregon, region="id")
oregon.df = join(oregon.points, oregon@data, by="id")

oregon.huc8 = readOGR(dsn="H:/quinalt/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)

#set up channel (need special workound since I'm on 64bit R)
cl<-RODBC::odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=Input/TylerScottQueryResults.accdb")

list.files('Input')

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


all.params.spdf = SpatialPointsDataFrame(coords = matrix(cbind(all.params.df$DECIMAL_LONG,
                                                               all.params.df$DECIMAL_LAT),ncol=2),
                                         data=all.params.df,proj4string=CRS("+datum=NAD83 +proj=longlat"))

temp = all.params.spdf@data






dat = dat %>% filter(water_yr>1990)


dat$YEAR = dat$water_yr
dat$MONTH =   month(mdy(dat$Date),label=TRUE)

all.params.spdf = SpatialPointsDataFrame(coords = matrix(cbind(all.params.df$DECIMAL_LONG,
                                                               all.params.df$DECIMAL_LAT),ncol=2),
                                         data=all.params.df,proj4string=CRS("+datum=NAD83 +proj=longlat"))

temp = all.params.spdf@data






n_stat = length(unique(dat$Station))
n_year = length(unique(dat$water_yr))

dat$monthyr = paste(dat$MONTH,dat$water_yr)


