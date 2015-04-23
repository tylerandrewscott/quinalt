
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
Year = data.frame(Year = seq(1990,2014,1))
Month = data.frame(Month = month.name,Month.Abb = month.abb)
Year.Month = merge(Year,Month, type='full')

#full merge to replicated huc8 by year and month
oregon.huc8.df = merge(oregon.huc8.df,Year.Month,type='full')


######################
#Add landcover data



######################


