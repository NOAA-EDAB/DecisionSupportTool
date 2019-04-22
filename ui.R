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
library(maptools)
library(shinyjs)
library(leaflet)
library(imager)
library(shinyEffects)
library(stringr)


#Source helper functions
r.dir <- here::here("R")

# source(file.path(r.dir,"read_shapefiles.R"))
source(file.path(r.dir,"model-specs.R"))
source("function_DecisionSupportTool_V1.2.R")
source(file.path(r.dir,"run_decisiontool.R"))

#A zoom effect for boxes
setZoom <- shinyEffects::setZoom

#User interface

ui <- dashboardPage(
  dashboardHeader(title = "ALW TRT Scenario Planning", titleWidth = 300),
  dashboardSidebar(    
    sidebarMenu(
    menuItem("Specify Model", tabName = "specify_model", icon = icon("dashboard")),
    menuItem("View Output", tabName = "view_output", icon = icon("th")),
    menuItem("Help", tabName = "help", icon = icon("question"))
    )
  ),
  dashboardBody(    
    
    tabItems(
      
      # First tab content
      tabItem(tabName = "specify_model",
              h4("Specify scenarios and scenario parameters"),
              fluidRow(
                box(
                  textInput("filename", label = "Enter new scenario name:", value = NULL)
                  ),
                box(
                  selectInput("existing_scenarios",
                              "Choose existing scenario:",
                              selected = "",
                              c("",existing_input_scenarios),
                              multiple = F)
                  )
                ),
              fluidRow(
                box(
                  
                  rHandsontableOutput("hot", width = "100%"),
                  actionButton(inputId="run",label="Run model"),
                  helpText('Parameterize actions by entering information into the spreadsheet above.
                           Right click and select "Insert Row Above" to incorporate multiple actions into the
                           scenario.'),
                  width = 12
                  )
                ),
              fluidRow(
                shinyjs::useShinyjs(),
                  textOutput("run-text")
                )
              ),
      tabItem(tabName = "view_output",
        fluidRow(
          box(
            selectInput("select_plots", label = "Select plots to view:", selected = "Total Threat",
                        choices = list(
                          "Trap Density",
                          "Trawl Length",
                          "Line Density",
                          "Line Diameter",
                          "Mean Threat",
                          "Total Threat",
                          "Risk Score",
                          "Whale Habitat"
                          )
                        ),
            checkboxInput("log_plots", label = "Log transform?", FALSE)
          )
        ),
        fluidRow(
            box(
              h4("Default model output:"),
              plotOutput("plot1",click="plot1_click",
                         dblclick = "plot1_dblclick",
                         height = "100%",
                         brush = brushOpts(
                           id = "plot1_brush",
                           resetOnNew = TRUE
                )
              )
            ),
            box(
              h4("Scenario model output:"),
              plotOutput("plot2", click="plot2_click",
                         dblclick = "plot2_dblclick",
                         height = "100%",
                         brush = brushOpts(
                           id = "plot2_brush",
                           resetOnNew = TRUE
                )
              )
            )
          )
        ),
      tabItem(tabName = "help",
              fluidPage(
              shinydashboard::box(width = NULL, solidHeader = TRUE, status = 'primary', leafletOutput('help_map',width="100%",height="80vh")),
              absolutePanel(top = 100, left = 280,
                            sliderInput("range", "Magnitudes", 1,10,
                                        value = range(1:10), step = 0.1),
                            checkboxGroupInput(inputId='shapefiles',label="Display Options",c("100f"="iso100ft","EastCoast"="EastCoastLines","GB"="GB","GOM"="GOM"),
                                               selected = c("iso100f","EastCoast"),inline = T)                  
          )
        ) 
      )
    )
  )
)