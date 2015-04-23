# Name: MakeZonalStatistics_impervious land.py
# Description: Summarizes impervious land within the huc4, huc8, and county
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
inZoneData = "tl_2012_us_county.shp"
zoneField = "COUNTYFP"
inValueRaster = "nlcd_2001_imperv/nlcd_2001_impervious_2011_edition_2014_03_31.img"
outTable = "imp_county_2001.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "ALL")
                                 
      # Set local variables
inZoneData = "tl_2012_us_county.shp"
zoneField = "COUNTYFP"
inValueRaster = "nlcd_2006_imperv/nlcd_2006_impervious_2011_edition_2014_03_31.img"
outTable = "imp_county_2006.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "ALL")
                                 
inZoneData = "tl_2012_us_county.shp"
zoneField = "COUNTYFP"
inValueRaster = "nlcd_2011_imperv/nlcd_2011_impervious_2011_edition_2014_03_31.img"
outTable = "imp_county_2011.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "ALL")
                                 

# Set local variables
inZoneData = "WBDHU8.shp"
zoneField = "HUC8"
inValueRaster = "nlcd_2001_imperv/nlcd_2001_impervious_2011_edition_2014_03_31.img"
outTable = "imp_huc8_2001.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "ALL")
                                 
      # Set local variables
inZoneData = "WBDHU8.shp"
zoneField = "HUC8"
inValueRaster = "nlcd_2006_imperv/nlcd_2006_impervious_2011_edition_2014_03_31.img"
outTable = "imp_huc8_2006.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "ALL")
                                 
inZoneData = "WBDHU8.shp"
zoneField = "HUC8"
inValueRaster = "nlcd_2011_imperv/nlcd_2011_impervious_2011_edition_2014_03_31.img"
outTable = "imp_huc8_2011.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "ALL")             
                                 
   
# Set local variables
inZoneData = "WBDHU4.shp"
zoneField = "HUC4"
inValueRaster = "nlcd_2001_imperv/nlcd_2001_impervious_2011_edition_2014_03_31.img"
outTable = "imp_huc4_2001.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "ALL")
                                 
      # Set local variables
inZoneData = "WBDHU4.shp"
zoneField = "HUC4"
inValueRaster = "nlcd_2006_imperv/nlcd_2006_impervious_2011_edition_2014_03_31.img"
outTable = "imp_huc4_2006.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "ALL")
                                 
inZoneData = "WBDHU4.shp"
zoneField = "HUC4"
inValueRaster = "nlcd_2011_imperv/nlcd_2011_impervious_2011_edition_2014_03_31.img"
outTable = "imp_huc4_2011.dbf"

# Execute ZonalStatisticsAsTable
outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, inValueRaster, 
                                 outTable, "DATA", "ALL")             
                                 
                                                                                       
arcpy.CheckInExtension("Spatial")