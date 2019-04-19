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

#Source helper functions
r.dir <- here::here("R")
source(file.path(r.dir,"read_shapefiles.R"))
source(file.path(r.dir,"model-specs.R"))
source("function_DecisionSupportTool_V1.2.R")
source(file.path(r.dir,"run_decisiontool.R"))


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
                )
              ),
      tabItem(tabName = "view_output",
        fluidRow(
            
          )
        ),
      tabItem(tabName = "help",
              fluidPage(
              shinydashboard::box(width = NULL, solidHeader = TRUE, status = 'primary', leafletOutput('help_map',width="100%",height="80vh")),
              absolutePanel(top = 100, left = 280,
                            sliderInput("range", "Magnitudes", 1,10,
                                        value = range(1:10), step = 0.1),
                            checkboxInput(inputId='iso_100',label="100 ft contour",value=F)
                            
              )
                
      ) #,
    )
  )
  )
)


#Server code
server <- function(input, output) {
  

  output$help_map = renderLeaflet({
    # initiates rendering. This all remains same for whole instance of app
    leaflet() %>%
      setView(lng = -68.73742, lat = 42.31386, zoom = 6) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addScaleBar(position = 'bottomright', options = scaleBarOptions(maxWidth = 250))


  })
  
  ########### 100 fit isobar check box
  observeEvent(input$iso_100, {

    if (input$iso_100 == T) {
      leafletProxy("help_map") %>% clearGroup(group = 'iso_100') %>%
        addPolygons(group = 'iso_100',data = iso100ft,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = 'iso_100')
    }
    
  })
  
  

  #Specifies table layout for custom input parameters

  output$hot = renderRHandsontable({
    
    #Show blank template if no input file is chosen
    if (input$existing_scenarios == ""){
      
      rhandsontable(DF, stretchH = "all", readOnly  = F) %>% 
        hot_col(col = "Action", type = "autocomplete", source = Action) %>% 
        hot_col(col = "LMA", type = "autocomplete", source = LMA) %>% 
        hot_col(col = "State", type = "autocomplete", source = State) %>% 
        hot_col(col = "StatArea", type = "autocomplete", source = StatArea) %>% 
        hot_col(col = "Fishery", type = "autocomplete", source = Fishery) %>% 
        hot_col(col = "Shapefile", strict = F, type = "autocomplete") %>% 
        hot_col(col = "Months", type = "autocomplete", source = Months) %>%
        hot_col(col = "Percentage", type = "numeric", strict = F) %>% 
        hot_col(col = "TrapRedistributionArea", type = "autocomplete", source = TrapRedistributionArea) %>% 
        hot_col(col = "TrapRedistributionMethod", type = "autocomplete", source = TrapRedistributionMethod)
    
    #Show filled template if input file is chosen
    } else {
      
      DF <- read.csv(paste0(file.path("InputSpreadsheets",input$existing_scenarios),".csv"))
      rhandsontable(DF, stretchH = "all", readOnly  = F) %>% 
        hot_col(col = "Action", type = "autocomplete", source = Action) %>% 
        hot_col(col = "LMA", type = "autocomplete", source = LMA) %>% 
        hot_col(col = "State", type = "autocomplete", source = State) %>% 
        hot_col(col = "StatArea", type = "autocomplete", source = StatArea) %>% 
        hot_col(col = "Fishery", type = "autocomplete", source = Fishery) %>% 
        hot_col(col = "Shapefile", strict = F, type = "autocomplete") %>% 
        hot_col(col = "Months", type = "autocomplete", source = Months) %>%
        hot_col(col = "Percentage", type = "numeric", strict = F) %>% 
        hot_col(col = "TrapRedistributionArea", type = "autocomplete", source = TrapRedistributionArea) %>% 
        hot_col(col = "TrapRedistributionMethod", type = "autocomplete", source = TrapRedistributionMethod)
      
    }
     
      
      
    
  })
  
  #Observes the "Run Model" button   
  observeEvent(input$run, {
    
    if (input$existing_scenarios == "" & all(is.na(hot_to_r(input$hot)))) {
      shinyjs::disable("run")
    } else {
      shinyjs::enable("run")
    
        #Converts table input into something shiny can use ----------------------------
        param <- hot_to_r(input$hot) 
        param[is.na(param)] <- ""
        # dumb workaround. we should be able to declare data types of each column in ui. 
        param$Action <- as.character(param$Action)
        param$LMA <- as.character(param$LMA)
        param$State <- as.character(param$State)
        param$StatArea <- as.character(param$StatArea)
        param$Fishery <- as.character(param$Fishery)
        param$Shapefile <- as.character(param$Shapefile)
        param$Months <- as.character(param$Months)
        #####################################
        param <- param %>% dplyr::filter(Action != "")
    
        #Saves output and runs model---------------------------------------------------
        
          write.csv(param, 
                    file = paste0(file.path("InputSpreadsheets",input$filename),".csv"), na="",row.names = F)
          print("Saved.")
          run_decisiontool(HD=here::here(),InputSpreadsheetName=paste0(input$filename,".csv"))
    }
      
  })
  
  #Observes the "Choose existing scenario button"
  observeEvent(input$existing_scenarios, {
    selected_scenario <- read.csv(paste0(file.path("InputSpreadsheets", input$existing_scenarios),".csv"))
  })
  
  
}

shinyApp(ui, server)