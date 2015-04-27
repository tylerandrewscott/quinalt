
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



all.params.spdf = SpatialPointsDataFrame(coords = matrix(cbind(all.params.df$DECIMAL_LONG,
                                                               all.params.df$DECIMAL_LAT),ncol=2),
                                         data=all.params.df,proj4string=CRS("+datum=NAD83 +proj=longlat"))

temp = all.params.spdf@data


dat = read.csv('Input/Scott_OWQI_1980_2013.csv')
dat = dat %>% filter(water_yr>1990)

dat$YEAR = dat$water_yr
dat$MONTH =   month(mdy(dat$Date),label=TRUE)

n_stat = length(unique(dat$Station))
n_year = length(unique(dat$water_yr))

dat$monthyr = paste(dat$MONTH,dat$water_yr)


