

#exec(open('H:/quinalt/scratch7.py').read())

# Name: MakeZonalStatistics_forstland.py
# Description: Summarizes values of forstland raster (forst = 1, other = 0) within the zones of huc8, HUC8, Counties
#               reports the results to a table.
# Requirements: Spatial Analyst Extension
# Author: Tyler Scott

# Import system modules
import arcpy
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")

# Set environment settings
env.workspace = "H:/duckabush"
            
inZoneData = "WBDHU8.shp"
zoneField = "HUC8"
inValueRaster = "tf_dev_2006"
outTable = "dev_huc8_2006.dbf"
# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
          
                                                       
inZoneData = "WBDHU8.shp"
zoneField = "HUC8"
inValueRaster = "tf_dev_2011"
outTable = "dev_huc8_2011.dbf"
# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 
                                 
inZoneData = "WBDHU8.shp"
zoneField = "HUC8"
inValueRaster = "tf_dev_2001"
outTable = "dev_huc8_2001.dbf"
# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                              
arcpy.CheckInExtension("Spatial")