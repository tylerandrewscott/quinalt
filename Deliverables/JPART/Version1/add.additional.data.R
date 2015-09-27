
require(ggplot2)
#source("http://www.math.ntnu.no/inla/givemeINLA.R")
require(sp)
require(rgdal)
require(rgeos)
require(ggplot2)
require(maptools)
require(plyr)
require(dplyr)
rm(list=ls())
setwd('H:/quinalt')
load('midpoint.2.RData')

temp.dat = all.params.spdf@data

oregon.county = readOGR(dsn="H:/quinalt/government_units", layer="county_nrcs_a_or")
oregon.county@data$id = rownames(oregon.county@data)
oregon.county.points = fortify(oregon.county, region="id")
oregon.county.df = join(oregon.county.points, oregon.county@data, by="id")

which.county = over(spTransform(all.params.spdf,CRS(proj4string(oregon.county))),oregon.county)
all.params.spdf@data$COUNTY = which.county$COUNTYNAME
rm(which.county)
rm(oregon.county)


oregon.eco = readOGR(dsn="H:/quinalt", layer="ecoregion")
oregon.eco@data$id = rownames(oregon.eco@data)
oregon.eco.points = fortify(oregon.eco, region="id")
oregon.eco.df = join(oregon.eco.points, oregon.eco@data, by="id")

oregon.huc8 = readOGR(dsn="H:/quinalt/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)
oregon.huc8.points = fortify(oregon.huc8, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")

which.eco = over(spTransform(all.params.spdf,CRS(proj4string(oregon.eco))),oregon.eco)

all.params.spdf@data$ECOREG3 = which.eco$LEV3_NAME
rm(oregon.eco)
rm(which.eco)


#oregon.prec.aug = readOGR(dsn="H:/quinalt/climate/precipitation", layer="precip1981_2010aug_a_or")
#oregon.prec.aug@data$id = rownames(oregon.prec.aug@data)

library(raster)
m = data.frame(lon = temp.dat$DECIMAL_LONG,lat = temp.dat$DECIMAL_LAT)
us.alt = getData('alt',country='US')

elevation.df = cbind(m, alt = raster::extract(us.alt[[1]], m, method = "bilinear"))


all.params.spdf@data$elevation = elevation.df$alt
mean.huc8.elev = stack(tapply(all.params.spdf@data$elevation,all.params.spdf@data$HUC8,mean,na.rm=T))


all.params.spdf@data$elevation[is.na(all.params.spdf@data$elevation)] = mean.huc8.elev$values[match(all.params.spdf@data$HUC8[is.na(all.params.spdf@data$elevation)],mean.huc8.elev$ind)]
all.params.spdf@data$mean.huc8.elevation = mean.huc8.elev$values[match(all.params.spdf@data$HUC8,mean.huc8.elev$ind)]


#load oregon boundary shapefile
oregon = readOGR(dsn="H:/quinalt/government_units", layer="state_nrcs_a_or")
oregon@data$id = rownames(oregon@data)
oregon.points = fortify(oregon, region="id")
oregon.df = join(oregon.points, oregon@data, by="id")

#load oregon huc8 shapefile
oregon.huc8 = readOGR(dsn="H:/quinalt/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)
#join into one polygon
reg <- unionSpatialPolygons(oregon.huc8, rep(1,91),100)
oregon.huc8.points = fortify(reg, region="id")
oregon.huc8.df = join(oregon.huc8.points, oregon.huc8@data, by="id")
#create oregon coastline dataframe
oregon.coast.df = subset(oregon.huc8.df,long<(-124)&lat>=min(oregon.df[,2]))

temp = all.params.spdf@data
coords <- as.matrix(cbind(temp$DECIMAL_LONG,temp$DECIMAL_LAT))

mat.dists <- spDists(coords, oregon.coast.df[,1:2], longlat=TRUE)
temp$"seaDist" <- apply(mat.dists, 1, min)
all.params.spdf@data = temp

rm(list=ls()[intersect(grep('huc8.database',ls(),invert=TRUE),grep('all.params.spdf',ls(),invert=TRUE))])
save.image('midpoint.3.RData')

