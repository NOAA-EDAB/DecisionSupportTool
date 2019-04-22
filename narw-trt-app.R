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

#Source helper functions
r.dir <- here::here("R")

# source(file.path(r.dir,"read_shapefiles.R"))
source(file.path(r.dir,"model-specs.R"))
source("function_DecisionSupportTool_V1.2.R")
source(file.path(r.dir,"run_decisiontool.R"))
previousShapefiles <- NULL

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
                         width = "650px",
                         height = "750px",
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
                         width = "650px",
                         height = "750px",
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

  observeEvent(input$shapefiles, {

    for (ichoice in 1:length(input$shapefiles)) {
        leafletProxy("help_map") %>% clearGroup(group = input$shapefile[ichoice]) %>%
        addPolygons(group = input$shapefile[ichoice] ,data = eval(parse(text=input$shapefile[ichoice])),stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
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
  
  #View output tab---------------------------------------------------------------------

  ### Function to read in images
  read.image <- function(image.file){
    im <- load.image(image.file)
    if(dim(im)[4] > 3){
      im <- imappend(channels(im, 1:3), 'c')
    }
    im
  }
  
  ### Generic function for plotting the image
  app.plot <- function(im, clicks.x = NULL, clicks.y = NULL, lineslist = NULL){
    if(is.null(im)){
      return(NULL)
    }
    if(is.null(ranges$x) | is.null(ranges$y)){
      plot(im, axes = F, ann=FALSE)
    }else{
      plot(im, axes = F, ann=FALSE, xlim=ranges$x,  ylim=c(ranges$y[2], ranges$y[1]))
    }
  }
  ### Set ranges for zooming
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  ### Code to zoom in on brushed area when double clicking for plot 1
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  v <- reactiveValues(
    originalImage = NULL,
    imgclick.x = NULL,
    imgclick.y = NULL
  )
  
  v2 <- reactiveValues(
    originalImage = NULL,
    imgclick.x = NULL,
    imgclick.y = NULL
  )
  
  output$plot1 <- renderPlot({

    
    matched_plots <- list.files("Scenarios/ExampleRun3/")[str_which(list.files("Scenarios/ExampleRun3/"),
                                                                    str_remove(input$select_plots, " "))]
    if (input$log_plots){

      matched_plots <- matched_plots[str_which(matched_plots, "Log")] 
    } else {
      matched_plots <- matched_plots[str_which(matched_plots, "Log", negate = T)] 
    }
    
    v$originalImage <- read.image(paste0("Scenarios/ExampleRun3/",matched_plots[1]))
    v$imgclick.x <- NULL
    v$imgclick.y <- NULL
    app.plot(v$originalImage,v$imgclick.x, v$imgclick.y)
    
  }, width = 675, height = 750)
  
  output$plot2 <- renderPlot({
    
    matched_plots <- list.files("Scenarios/ExampleRun3/")[str_which(list.files("Scenarios/ExampleRun3/"),
                                                                    str_remove(input$select_plots, " "))]
    if (input$log_plots){
      
      matched_plots <- matched_plots[str_which(matched_plots, "Log")] 
    } else {
      matched_plots <- matched_plots[str_which(matched_plots, "Log", negate = T)] 
    }
    
    print(paste0("Scenarios/ExampleRun3/",matched_plots[2]))
    v2$originalImage <- read.image(paste0("Scenarios/ExampleRun3/",matched_plots[2]))
    v2$imgclick.x <- NULL
    v2$imgclick.y <- NULL
    app.plot(v2$originalImage,v2$imgclick.x, v2$imgclick.y)

  }, width = 675, height = 750)

}

shinyApp(ui, server)