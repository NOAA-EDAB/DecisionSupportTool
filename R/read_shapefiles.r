## Stat Areas
shapefilePath <- here::here("InputShapefiles")
#shapeFiles <- list.files(here::here("InputShapefiles"),pattern="\\.shp$")


iso100ft <-  rgdal::readOGR(dsn = shapefilePath, layer = '100f_Isobath')




