#make conditional (0/1) forstands rasters

# Import system modules
import arcpy
from arcpy import env
from arcpy.sa import *

# Set environment settings
env.workspace = "H:/duckabush"
# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")

###2001 forstands######
# Set local variables
inRaster = Raster("nlcd_2001_landcover_2011_edition_2014_03_31.img")
inTrueRaster = 1
inFalseConstant = 0
whereClause = "(Value > 40) & (Value < 45)"
# Execute Con
#outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
outCon = Con(inRaster < 45, Con(inRaster > 40, inTrueRaster, inFalseConstant), inFalseConstant)
# Save the outputs 
outCon.save("H:/duckabush/tf_forst_2001")

####2006 forstands######
# Set local variables
inRaster = Raster("nlcd_2006_landcover_2011_edition_2014_03_31.img")
inTrueRaster = 1
inFalseConstant = 0
whereClause = "(Value > 40) & (Value < 45)"
# Execute Con
#outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
outCon = Con(inRaster < 45, Con(inRaster > 40, inTrueRaster, inFalseConstant), inFalseConstant)
# Save the outputs 
outCon.save("H:/duckabush/tf_forst_2006")


#####2011 forstands######
# Set local variables
inRaster = Raster("nlcd_2011_landcover_2011_edition_2014_03_31.img")
inTrueRaster = 1
inFalseConstant = 0
inFalseConstant = 0
whereClause = "(Value > 40) & (Value < 45)"
# Execute Con
#outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
outCon = Con(inRaster < 45, Con(inRaster > 40, inTrueRaster, inFalseConstant), inFalseConstant)
# Save the outputs 
outCon.save("H:/duckabush/tf_forst_2011")

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

# Check in the ArcGIS Spatial Analyst extension license
arcpy.CheckInExtension("Spatial")




#exec(open('H:/quinalt/scratch7.py').read())
#make conditional (0/1) rasters for developed land 2001/2006/2011

# Import system modules
import arcpy
from arcpy import env
from arcpy.sa import *

# Set environment settings
env.workspace = "H:/duckabush"
# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")

###2001 forstands######
# Set local variables
inRaster = Raster("nlcd_2001_landcover_2011_edition_2014_03_31.img")
inTrueRaster = 1
inFalseConstant = 0
whereClause = "(Value > 21) & (Value < 25)"
# Execute Con
#outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
outCon = Con(inRaster < 25, Con(inRaster > 21, inTrueRaster, inFalseConstant), inFalseConstant)
# Save the outputs 
outCon.save("H:/duckabush/tf_dev_2001")

####2006 forstands######
# Set local variables
inRaster = Raster("nlcd_2006_landcover_2011_edition_2014_03_31.img")
inTrueRaster = 1
inFalseConstant = 0
whereClause = "(Value > 21) & (Value < 25)"
# Execute Con
#outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
outCon = Con(inRaster < 25, Con(inRaster > 21, inTrueRaster, inFalseConstant), inFalseConstant)
# Save the outputs 
outCon.save("H:/duckabush/tf_dev_2006")


#####2011 forstands######
# Set local variables
inRaster = Raster("nlcd_2011_landcover_2011_edition_2014_03_31.img")
inTrueRaster = 1
inFalseConstant = 0
inFalseConstant = 0
whereClause = "(Value > 21) & (Value < 25)"
# Execute Con
#outCon = Con(inRaster, inTrueRaster, inFalseConstant, whereClause)
outCon = Con(inRaster < 25, Con(inRaster > 21, inTrueRaster, inFalseConstant), inFalseConstant)
# Save the outputs 
outCon.save("H:/duckabush/tf_dev_2011")


# Check in the ArcGIS Spatial Analyst extension license
arcpy.CheckInExtension("Spatial")




