# Name: MakeZonalStatistics_wetlland.py
# Description: Summarizes values of wetlland raster (wetl = 1, other = 0) within the zones of huc8, HUC8, Counties
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

# Set local variables
inZoneData = "WBDHU8.shp"
zoneField = "HUC8"
inValueRaster = "tf_wetl_2001"
outTable = "wetl_huc8_2001.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 
      # Set local variables
inZoneData = "WBDHU8.shp"
zoneField = "HUC8"
inValueRaster = "tf_wetl_2006"
outTable = "wetl_huc8_2006.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 
inZoneData = "WBDHU8.shp"
zoneField = "HUC8"
inValueRaster = "tf_wetl_2011"
outTable = "wetl_huc8_2011.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 
                             
arcpy.CheckInExtension("Spatial")