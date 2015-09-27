#add land cover data

rm(list=ls())
setwd('H:/quinalt/')
load('midpoint.1b.RData')


require(foreign)
require(dplyr)

oregon.huc8 = readOGR(dsn="H:/quinalt/hydrologic_units", layer="wbdhu8_a_or")
oregon.huc8@data$id = rownames(oregon.huc8@data)

#1992 nlcd
ag.huc8.1992 = read.dbf('or_agr_huc8_1992.dbf');colnames(ag.huc8.1992)[5] = 'ag.huc8'
wet.huc8.1992 = read.dbf('or_wetl_huc8_1992.dbf');colnames(wet.huc8.1992)[5] = 'wet.huc8'
forst.huc8.1992 = read.dbf('or_forst_huc8_1992.dbf');colnames(forst.huc8.1992)[5] = 'forst.huc8'
dev.huc8.1992 = read.dbf('or_dev_huc8_1992.dbf');colnames(dev.huc8.1992)[5] = 'dev.huc8'

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
crop.huc8.2001 = read.dbf('crop_huc8_2001.dbf');colnames(crop.huc8.2001)[5] = 'crop.huc8'
past.huc8.2001 = read.dbf('past_huc8_2001.dbf');colnames(past.huc8.2001)[5] = 'past.huc8'
wet.huc8.2001 = read.dbf('wetl_huc8_2001.dbf');colnames(wet.huc8.2001)[5] = 'wet.huc8'
forst.huc8.2001 = read.dbf('forst_huc8_2001.dbf');colnames(forst.huc8.2001)[5] = 'forst.huc8'
dev.huc8.2001 = read.dbf('dev_huc8_2001.dbf');colnames(dev.huc8.2001)[5] = 'dev.huc8'
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
crop.huc8.2006 = read.dbf('crop_huc8_2006.dbf');colnames(crop.huc8.2006)[5] = 'crop.huc8'
past.huc8.2006 = read.dbf('past_huc8_2006.dbf');colnames(past.huc8.2006)[5] = 'past.huc8'
wet.huc8.2006 = read.dbf('wetl_huc8_2006.dbf');colnames(wet.huc8.2006)[5] = 'wet.huc8'
forst.huc8.2006 = read.dbf('forst_huc8_2006.dbf');colnames(forst.huc8.2006)[5] = 'forst.huc8'
dev.huc8.2006 = read.dbf('dev_huc8_2006.dbf');colnames(dev.huc8.2006)[5] = 'dev.huc8'
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
crop.huc8.2011 = read.dbf('crop_huc8_2011.dbf');colnames(crop.huc8.2011)[5] = 'crop.huc8'
past.huc8.2011 = read.dbf('past_huc8_2011.dbf');colnames(past.huc8.2011)[5] = 'past.huc8'
wet.huc8.2011 = read.dbf('wetl_huc8_2011.dbf');colnames(wet.huc8.2011)[5] = 'wet.huc8'
forst.huc8.2011 = read.dbf('forst_huc8_2011.dbf');colnames(forst.huc8.2011)[5] = 'forst.huc8'
dev.huc8.2011 = read.dbf('dev_huc8_2011.dbf');colnames(dev.huc8.2011)[5] = 'dev.huc8'
ag.huc8.2011 = crop.huc8.2011
ag.huc8.2011$ag.huc8 = crop.huc8.2011$crop.huc8+past.huc8.2011$past.huc8[match(crop.huc8.2011$HUC8,past.huc8.2011$HUC8)]
cover.2011 = join_all(list(ag.huc8.2011,wet.huc8.2011,forst.huc8.2011,dev.huc8.2011))
cover.2011$YEAR = 2011
cover.2011 = dplyr::select(cover.2011,-c(COUNT,AREA,ZONE_CODE,crop.huc8))
cover.2012 = cover.2011; cover.2007$YEAR = 2012
cover.2013 = cover.2011; cover.2008$YEAR = 2013


land.cover.huc8 = join_all(list(cover.1990,cover.1991,cover.1992,cover.1993,cover.1994,
                     cover.1995,cover.1996,cover.1997,cover.1998,cover.1999,
                     cover.2000,cover.2001,cover.2002,cover.2003,cover.2004,
                     cover.2005,cover.2006,cover.2007,cover.2008,cover.2009,
                     cover.2010,cover.2011,cover.2012,cover.2013),
                     type='full')


#######ADD IN LANDCOVER DATA#############

huc8.database = join(huc8.database,land.cover.huc8)

rm(list=ls()[intersect(grep('huc8.database',ls(),invert=TRUE),grep('all.params.spdf',ls(),invert=TRUE))])
save.image('midpoint.2.RData')

#############################
