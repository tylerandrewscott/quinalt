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
env.workspace = "H:/quinalt/hydrologic_units"
            
inZoneData = "wbdhu8_a_or.shp"
zoneField = "HUC8"
inValueRaster = "tf_forst_1992"
outTable = "or_forst_huc8_1992.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 
inZoneData = "wbdhu8_a_or.shp"
zoneField = "HUC8"
inValueRaster = "tf_wetl_1992"
outTable = "or_wetl_huc8_1992.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 
inZoneData = "wbdhu8_a_or.shp"
zoneField = "HUC8"
inValueRaster = "tf_dev_1992"
outTable = "or_dev_huc8_1992.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")
                                 
inZoneData = "wbdhu8_a_or.shp"
zoneField = "HUC8"
inValueRaster = "tf_agr_1992"
outTable = "or_agr_huc8_1992.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "MEAN")

                                                                                                                
arcpy.CheckInExtension("Spatial")