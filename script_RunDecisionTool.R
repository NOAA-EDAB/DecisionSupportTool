library(rgdal)
library(sp)
library(maps)
library(maptools)

## set working directory
HD=here::here()
# HD="C:/Lobsters/RightWhales/DecisionSupportTool"

## source decision support function
source(paste(HD, "function_DecisionSupportTool_V1.2.R", sep="/"))

## call function
DecisionTool(
  HomeDir=HD, ## home directory for subdirectories
  InputSpreadsheetName="ExampleRun.csv", ## csv input file with specified criteria
  HighResolution=FALSE, ## Option to run in HighResolution mode. Slows model drammatically, not fully tested
  PrintTables=TRUE, ## print pdf tables of results
  PrintDefaultMaps=TRUE, ## print maps of default states; turned off to speed model run
  PrintScenarioMaps=TRUE, ## print maps fo scenario states; turned off to speed model run
  WriteMapSources=TRUE, ## write output used for producing maps to .Rdata file. 
      ##    Only works if both PrintDefaultMaps and PrintScenarioMaps =TRUE
  WriteOutputCsv=TRUE ## write results in PrintTables as csv file for further analysis
)


## To Do list:
## Trap Redistribution
  ##  Even
  ## Inverse Distance
  ## associated maps
## Extend Shapefiles to IEc domain
## Expanded Duke domain to IEc
## Switch resolution to IecIndex rather than GridID
## Trawl Length Regulations
## Rope diameter regulations
## Threat model
  ## threat model incorporation to lookup and input spreadsheet
## Detailed Tables Output
  ## By LMA
  ## By Stat Area
