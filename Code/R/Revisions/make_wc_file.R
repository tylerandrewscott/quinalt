
localDir <- 'TempData'
if (!file.exists(localDir)) {
  dir.create(localDir)
}

url <- 'http://oe.oregonexplorer.info/ExternalContent/SpatialDataforDownload/iw_or_watershedcouncils_2011.zip'
file <- paste(localDir,basename(url),sep='/')
if (!file.exists(file)) {
  download.file(url, file)
  unzip(file,exdir=localDir)
}


# layerName is the name of the unzipped shapefile without file type extensions 
layerName <- "iw_or_watershedcouncils_2011"  
# Read in the data
oregon.wc <- readOGR(dsn=localDir, layer=layerName) 
oregon.wc@data$id = rownames(oregon.wc@data)
oregon.wc.df = as.data.frame(oregon.wc)

write.csv(oregon.wc.df,'Input/watershed_councils.csv')
