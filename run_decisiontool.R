# This is run from shiny 
#
# The following lines need to be placed in shiny
#
#l ibrary(rgdal)
# library(sp)
# library(maps)
# library(maptools)
# library(grid)
# library(gtable)
# library(gridExtra)
# library(maptools)
# HD <- here::here()
# source(here::here("function_DecisionSupportTool_V1.2.R"))
#

run_decisiontool <- function(HD=here::here(),InputSpreadsheetName="ExampleRun4.csv"){
  DecisionTool(
    HomeDir=HD, ## home directory for subdirectories
    InputSpreadsheetName=InputSpreadsheetName, ## csv input file with specified criteria
    HighResolution=FALSE, ## Option to run in HighResolution mode. Slows model drammatically, not fully tested
    PrintTables=TRUE, ## print pdf tables of results
    PrintDefaultMaps=TRUE, ## print maps of default states; turned off to speed model run
    PrintScenarioMaps=TRUE, ## print maps fo scenario states; turned off to speed model run
    WriteMapSources=TRUE, ## write output used for producing maps to .Rdata file. 
    ##    Only works if both PrintDefaultMaps and PrintScenarioMaps =TRUE
    WriteOutputCsv=TRUE ## write results in PrintTables as csv file for further analysis
  )
  
  
}