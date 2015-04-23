# Name: MakeZonalStatistics_Cropland.py
# Description: Summarizes values of cropland raster (crop = 1, other = 0) within the zones of huc8, HUC8, Counties
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
inValueRaster = "tf_crop_2001"
outTable = "crop_huc8_2001.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 
      # Set local variables
inZoneData = "WBDHU8.shp"
zoneField = "HUC8"
inValueRaster = "tf_crop_2006"
outTable = "crop_huc8_2006.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 
inZoneData = "WBDHU8.shp"
zoneField = "HUC8"
inValueRaster = "tf_crop_2011"
outTable = "crop_huc8_2011.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 

                                 # Set local variables
inZoneData = "WBDHU8.shp"
zoneField = "HUC8"
inValueRaster = "tf_past_2001"
outTable = "past_huc8_2001.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 
      # Set local variables
inZoneData = "WBDHU8.shp"
zoneField = "HUC8"
inValueRaster = "tf_past_2006"
outTable = "past_huc8_2006.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 
inZoneData = "WBDHU8.shp"
zoneField = "HUC8"
inValueRaster = "tf_past_2011"
outTable = "past_huc8_2011.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 
                                 
arcpy.CheckInExtension("Spatial")