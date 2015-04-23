#make conditional (0/1) wetlands rasters

# Import system modules
import arcpy
from arcpy import env
from arcpy.sa import *

# Set environment settings
env.workspace = "H:/duckabush"
# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")

###2001 Wetlands######
# Set local variables
inRaster = Raster("nlcd_2001_landcover_2011_edition_2014_03_31.img")
inTrueRaster = 1
inFalseConstant = 0
whereClause = "Value >= 89"
# Execute Con
outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
# Save the outputs 
outCon.save("H:/duckabush/tf_wetl_2001")

####2006 Wetlands######
# Set local variables
inRaster = Raster("nlcd_2006_landcover_2011_edition_2014_03_31.img")
inTrueRaster = 1
inFalseConstant = 0
whereClause = "Value >= 89"
# Execute Con
outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
# Save the outputs 
outCon.save("H:/duckabush/tf_wetl_2006")


#####2011 Wetlands######
# Set local variables
inRaster = Raster("nlcd_2011_landcover_2011_edition_2014_03_31.img")
inTrueRaster = 1
inFalseConstant = 0
inFalseConstant = 0
whereClause = "Value >= 89"
# Execute Con
outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
# Save the outputs 
outCon.save("H:/duckabush/tf_wetl_2011")



#####1992 forstands######
# Set local variables
inRaster = Raster("nlcd92mosaic.img")
inTrueRaster = 1
inFalseConstant = 0
inFalseConstant = 0
whereClause = "(Value > 40) & (Value < 45)"
# Execute Con
#outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
outCon = Con(inRaster < 45, Con(inRaster > 40, inTrueRaster, inFalseConstant), inFalseConstant)
# Save the outputs 
outCon.save("H:/duckabush/tf_forst_1992")


#####1992 ag######
# Set local variables
inRaster = Raster("nlcd92mosaic.img")
inTrueRaster = 1
inFalseConstant = 0
inFalseConstant = 0
whereClause = "(Value > 80) & (Value < 85)"
# Execute Con
#outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
outCon = Con(inRaster < 85 , Con(inRaster > 80, inTrueRaster, inFalseConstant), inFalseConstant)
# Save the outputs 
outCon.save("H:/duckabush/tf_ag_1992")

#####1992 ag######
# Set local variables
inRaster = Raster("nlcd92mosaic.img")
inTrueRaster = 1
inFalseConstant = 0
inFalseConstant = 0
whereClause = "(Value > 90) & (Value < 93)"
# Execute Con
#outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
outCon = Con(inRaster < 93 , Con(inRaster > 90, inTrueRaster, inFalseConstant), inFalseConstant)
# Save the outputs 
outCon.save("H:/duckabush/tf_wetl_1992")

#####1992 ag######
# Set local variables
inRaster = Raster("nlcd92mosaic.img")
inTrueRaster = 1
inFalseConstant = 0
inFalseConstant = 0
whereClause = "(Value > 20) & (Value < 24)"
# Execute Con
#outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
outCon = Con(inRaster < 24 , Con(inRaster > 20, inTrueRaster, inFalseConstant), inFalseConstant)
# Save the outputs 
outCon.save("H:/duckabush/tf_dev_1992")



# Check in the ArcGIS Spatial Analyst extension license
arcpy.CheckInExtension("Spatial")

