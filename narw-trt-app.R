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
previousShapefiles <- NULL


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
            
          )
        ),
      tabItem(tabName = "help",
              fluidPage(
              shinydashboard::box(width = NULL, solidHeader = TRUE, status = 'primary', leafletOutput('help_map',width="100%",height="80vh")),
              
              absolutePanel(top = 100, left = 320,
                    # sliderInput("range", "Magnitudes", 1,10,
                    #                     value = range(1:10), step = 0.1),
                    # # checkboxGroupInput(inputId='shapefiles',label="Display Options",c("100ft"="iso100ft","EastCoast"="EastCoastLines","GB"="GB","GOM"="GOM"),
                    #                            inline = T)
                    h3("Display options"),
                    checkboxInput(inputId='shapefile1',label="100ft",value = F),
                    checkboxInput(inputId='shapefile2',label="EastCoast",value = F),
                    checkboxInput(inputId='shapefile3',label="GB",value = F),
                    checkboxInput(inputId='shapefile4',label="GOM",value = F),
                    checkboxInput(inputId='shapefile5',label="GSC_Gillnet",value = F),
                    checkboxInput(inputId='shapefile6',label="GSC_Trap",value = F),
                    checkboxInput(inputId='shapefile7',label="GSC_Sliver",value = F),
                    checkboxInput(inputId='shapefile8',label="LCMAs",value = F),
                    checkboxInput(inputId='shapefile9',label="MASS_RA",value = F),
                    checkboxInput(inputId='shapefile10',label="MASS_RANE",value = F),
                    checkboxInput(inputId='shapefile11',label="NEA_NR",value = F),
                    checkboxInput(inputId='shapefile12',label="NEA_WGOM",value = F),
                    checkboxInput(inputId='shapefile13',label="SA_DT",value = F),
                    checkboxInput(inputId='shapefile14',label="SA_537",value = F)
                    
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
  
  ########### 100 fit isobar check box ##############
  # observeEvent(input$shapefiles, {
  #     for (ichoice in 1:length(input$shapefiles)) {
  #       group <- input$shapefiles[ichoice]
  #       data  <-  eval(parse(text=group))
  #       leafletProxy("help_map") %>% clearGroup(group = group)  %>%
  #          addPolygons(group = group ,data = data ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
  #     }
  # })
  ###############################################################################################
  ################### HORRIBLE CODE . NEED TO FIND A BETTER WAY #################################
  ###############################################################################################
  observeEvent(input$shapefile1, {
    if(input$shapefile1 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile1")  %>%
        addPolygons(group = "shapefile1" ,data = iso100ft ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile1")
    }
  })
  observeEvent(input$shapefile2, {
    if(input$shapefile2 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile2")  %>%
        addPolygons(group = "shapefile2" ,data = EastCoastLines ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile2")
    }
  })
  observeEvent(input$shapefile3, {
    if(input$shapefile3 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile3")  %>%
        addPolygons(group = "shapefile3" ,data = GB,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile3")
    }
  })
  observeEvent(input$shapefile4, {
    if(input$shapefile4 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile4")  %>%
        addPolygons(group = "shapefile4" ,data = GOM ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile4")
    }
  })
  observeEvent(input$shapefile5, {
    if(input$shapefile5 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile5")  %>%
        addPolygons(group = "shapefile5" ,data = GSC_Gillnet ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile5")
    }
  })
  observeEvent(input$shapefile6, {
    if(input$shapefile6 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile6")  %>%
        addPolygons(group = "shapefile6" ,data = GSC_Trap ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile6")
    }
  })
  observeEvent(input$shapefile7, {
    if(input$shapefile7 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile7")  %>%
        addPolygons(group = "shapefile7" ,data = GSC_Sliver ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile7")
    }
  })
  observeEvent(input$shapefile8, {
    if(input$shapefile8 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile8")  %>%
        addPolygons(group = "shapefile8" ,data = LCMAs ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
      } else {
        leafletProxy("help_map") %>% clearGroup(group = "shapefile8")
        }
  })  
  observeEvent(input$shapefile9, {
    if(input$shapefile9 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile9")  %>%
        addPolygons(group = "shapefile9" ,data = MASS_RA ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile9")
    }
  })
  observeEvent(input$shapefile10, {
    if(input$shapefile10 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile10")  %>%
        addPolygons(group = "shapefile10" ,data = MASS_RANE ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile10")
    }
  }) 
  observeEvent(input$shapefile11, {
    if(input$shapefile11 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile11")  %>%
        addPolygons(group = "shapefile11" ,data = NEA_NR ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile11")
    }
  })
  observeEvent(input$shapefile12, {
    if(input$shapefile12 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile12")  %>%
        addPolygons(group = "shapefile12" ,data = NEA_WGOM ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile12")
    }
  })
  observeEvent(input$shapefile13, {
    if(input$shapefile13 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile13")  %>%
        addPolygons(group = "shapefile13" ,data = SA_DT ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile13")
    }
  })
  observeEvent(input$shapefile14, {
    if(input$shapefile14 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile14")  %>%
        addPolygons(group = "shapefile14" ,data = SA_537 ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile14")
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
  
  observeEvent(input$filename, {
    
    #Prevent model run if no file is chosen and no custom input
    if (is.null(input$hot)){
      shinyjs::disable("run")
      
      #Prevent model run if custom parameters exist without a scenario name
    } else if (input$filename == ""){
      shinyjs::disable("run")
      
      #Otherwise run the model and save the ouput to csv
    } else {
      shinyjs::enable("run")
    }
    
  })
  
  #Observes the "Run Model" button   
  observeEvent(input$run, {
    
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
          
          withCallingHandlers({
            shinyjs::html("run-text", "")
            run_decisiontool(HD=here::here(),InputSpreadsheetName=paste0(input$filename,".csv"))
          },
          message = function(m) {
            shinyjs::html(id = "run-text", html = paste0(m$message,"<br>"), add = TRUE)
          })
      })
  

  # #Observes the "Choose existing scenario button"
  # observeEvent(input$existing_scenarios, {
  #   selected_scenario <- read.csv(paste0(file.path("InputSpreadsheets", input$existing_scenarios),".csv"))
  # })

  
}

shinyApp(ui, server)