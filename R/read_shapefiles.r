## Stat Areas
shapefilePath <- here::here("InputShapefiles")
#shapeFiles <- list.files(here::here("InputShapefiles"),pattern="\\.shp$")

message("Reading in shapefiles ...")

OffshoreA <-  rgdal::readOGR(dsn = shapefilePath, layer = 'AtlOffshoreLAFigure2_',verbose = F)
MassExpansion <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Mass_Bay_Expansion_Only',verbose = F)
CCBay <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Proposed_Cape_Cod_Bay_Area',verbose = F)
SouthShoreA <-  rgdal::readOGR(dsn = shapefilePath, layer = 'SouthShoreA',verbose = F)
SouthShoreB <-  rgdal::readOGR(dsn = shapefilePath, layer = 'SouthShoreB',verbose = F)
SouthShoreC <-  rgdal::readOGR(dsn = shapefilePath, layer = 'SouthShoreC',verbose = F)

#EastCoastLines <-  rgdal::readOGR(dsn = shapefilePath, layer = 'EastCoastLines',verbose = F)
GB <-  rgdal::readOGR(dsn = shapefilePath, layer = 'GB_100M_to_600M',verbose = F)
GOM <-  rgdal::readOGR(dsn = shapefilePath, layer = 'GOM_100M_to_EEZ',verbose = F)
#GSC_Gillnet <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Great_South_Channel_Restricted_Gillnet_Area',verbose = F)
#GSC_Trap <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Great_South_Channel_Restricted_Trap-Pot_Area',verbose = F)
#GSC_Sliver <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Great_South_Channel_Sliver_Restricted_Area',verbose = F)
LCMAs <-  rgdal::readOGR(dsn = shapefilePath, layer = 'LCMAs',verbose = F)
MASS_RA <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Massachusetts_Restricted_Area',verbose = F)
MASS_RANE <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Massachusetts_Restricted_Area_North_Expansion',verbose = F)
NEA_NR <-  rgdal::readOGR(dsn = shapefilePath, layer = 'NEA_Nantucket_Rectangle',verbose = F)
NEA_WGOM <-  rgdal::readOGR(dsn = shapefilePath, layer = 'NEA_WGOM_Area',verbose = F)
#SA_DT <-  rgdal::readOGR(dsn = shapefilePath, layer = 'StatAreas_DecisionTool',verbose = F)
#SA_537 <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Statistical_Area_537',verbose = F)
#TinyWedge <-  rgdal::readOGR(dsn = shapefilePath, layer = 'TinyWedge_LMA1',verbose = F)
StatAreas <-  rgdal::readOGR(dsn = shapefilePath, layer = 'Statistical_Areas',verbose = F)
areas <- c("464" ,"465" ,"511" ,"511", "512", "512", "513" ,"514" ,"515", "521", "522", "525", "526",
           "537" ,"538" ,"538", "539", "561" ,"562", "616")
StatAreas <- raster::subset(StatAreas,Id %in% areas)

s512 <-  rgdal::readOGR(dsn = shapefilePath, layer = 's512',verbose = F)

message("Done")


