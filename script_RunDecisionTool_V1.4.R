#source 1.4
library(shinydashboard)
library(htmlwidgets)
library(rhandsontable)
library(rgdal)
library(sp)
library(maps)
library(maptools)
library(grid)
library(gtable)
library(gridExtra)
library(shinyjs)
library(leaflet)
library(imager)
library(shinyEffects)
library(stringr)
library(dplyr)

#Source helper functions
r.dir <- here::here("R")

source(file.path(r.dir,"read_shapefiles.R"))
source(file.path(r.dir,"model-specs.R"))
# source("function_DecisionSupportTool_V1.2.R")
source(file.path(r.dir,"run_decisiontool.R"))


## set working directory
HD=here::here()
# HD="C:/Lobsters/RightWhales/DecisionSupportTool"
## source decision support function
source(paste(HD, "function_DecisionSupportTool_V1.4.R", sep="/"))

## call function
DecisionTool(
  HomeDir=HD, ## home directory for subdirectories
  InputSpreadsheetName="ScenarioTemplate1_4.csv", ## csv input file with specified criteria
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
