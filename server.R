#Server code
function(input, output, session) {
  
  output$help_map = renderLeaflet({
    
    # initiates rendering. This all remains same for whole instance of app
    leaflet() %>%
      setView(lng = -68.73742, lat = 42.31386, zoom = 6) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addScaleBar(position = 'bottomright', options = scaleBarOptions(maxWidth = 250))
    
  })
  
  
  ###############################################################################################
  ################### HORRIBLE CODE . NEED TO FIND A BETTER WAY #################################
  ###############################################################################################
  observeEvent(input$shapefile1, {
    if(input$shapefile1 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile1")  %>%
        addPolygons(group = "shapefile1" ,data = SouthShoreA ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile1")
    }
  })
  observeEvent(input$shapefile2, {
    if(input$shapefile2 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile2")  %>%
        addPolygons(group = "shapefile2" ,data = SouthShoreB ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
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
        addPolygons(group = "shapefile5" ,data = SouthShoreC ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile5")
    }
  })
  observeEvent(input$shapefile6, {
    if(input$shapefile6 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile6")  %>%
        addPolygons(group = "shapefile6" ,data = CCBay ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile6")
    }
  })
  observeEvent(input$shapefile7, {
    if(input$shapefile7 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile7")  %>%
        addPolygons(group = "shapefile7" ,data = MassExpansion ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile7")
    }
  })
  observeEvent(input$shapefile8a, {
    if(input$shapefile8a == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile8a")  %>%
        addPolygons(group = "shapefile8a" ,data =raster::subset(LCMAs,Name=="A1")  ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile8a")
    }
  })
  observeEvent(input$shapefile8b, {
    if(input$shapefile8b == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile8b")  %>%
        addPolygons(group = "shapefile8b" ,data =raster::subset(LCMAs,Name=="A2") ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile8b")
    }
  }) 
  observeEvent(input$shapefile8c, {
    if(input$shapefile8c == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile8c")  %>%
        addPolygons(group = "shapefile8c" ,data =raster::subset(LCMAs,Name=="A3")  ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile8c")
    }
  })  
  observeEvent(input$shapefile8d, {
    if(input$shapefile8d == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile8d")  %>%
        addPolygons(group = "shapefile8d" ,data =raster::subset(LCMAs,Name=="A2_3overlap")  ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile8d")
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
  observeEvent(input$shapefile13, { #STatAreas
    coords <- as.data.frame(coordinates(StatAreas))
    colnames(coords) <- c("Long","Lat")
    if(input$shapefile13 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile13")  %>%
        addPolygons(group = "shapefile13" ,data = StatAreas , stroke = TRUE, color = '#5a5a5a', 
                    opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3) %>% 
        addLabelOnlyMarkers(
          group = "shapefile13",data = coords,lng = ~Long,
          lat = ~Lat, label = ~as.character(StatAreas$Id),
          labelOptions = labelOptions(noHide = T, textOnly = T))
        # addAwesomeMarkers(group = "shapefile13",data = coords,lng = ~Long,lat = ~Lat, label = as.character(StatAreas$Id) )
      
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile13")
    }
  })

  observeEvent(input$shapefile14, {
    if(input$shapefile14 == T) {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile14")  %>%
        addPolygons(group = "shapefile14" ,data = OffshoreA ,stroke = TRUE, color = '#5a5a5a', opacity = 1.0, weight = 0.5, fillColor = "#dcdcdc", fillOpacity = 0.3)
    } else {
      leafletProxy("help_map") %>% clearGroup(group = "shapefile14")
    }
  })
  
  
  observeEvent(input$update_list,{
  # get existing scenarios for listing as scenaerio inputs
  existing_input_csvs <- list.files(here::here("InputSpreadsheets"))
  existing_input_scenarios <- stringr::str_remove(existing_input_csvs, ".csv|.xlsx")

  # Can also set the label and select items
  updateSelectInput(session,
                    "existing_scenarios",
                    choices = c("",existing_input_scenarios),
                    selected = "")
  })

  
  
  #Specifies table layout for custom input parameters
  output$hot = renderRHandsontable({
    #Show blank template if no input file is chosen
    if (input$existing_scenarios == ""){
      
      rhandsontable(DF, stretchH = "all", readOnly  = F) %>% 
        hot_cols(colWidths = c(100,50)) %>% 
        hot_col(col = "Action", type = "autocomplete", source = Action) %>% 
        hot_col(col = "LMA", type = "autocomplete", source = LMA) %>% 
        hot_col(col = "State", type = "autocomplete", source = State) %>% 
        hot_col(col = "StatArea", type = "autocomplete", source = StatArea) %>% 
        hot_col(col = "Fishery", type = "autocomplete", source = Fishery) %>% 
        hot_col(col = "Shapefile", strict = F, type = "autocomplete", source = shapefile_names) %>% 
        hot_col(col = "Months", type = "autocomplete", source = Months) %>%
        hot_col(col = "Percentage", type = "numeric", strict = F) %>% 
        hot_col(col = "TrapRedistributionArea", type = "autocomplete", source = TrapRedistributionArea) %>% 
        hot_col(col = "TrapRedistributionMethod", type = "autocomplete", source = TrapRedistributionMethod)
      
      #Show filled template if input file is chosen
    } else {
      print(paste("Selected model:",input$existing_scenarios))
   
      #For running models back to back in same session. 
      DF <- read.csv(paste0(here::here("InputSpreadsheets",input$existing_scenarios),".csv"))
      
      rhandsontable(DF, stretchH = "all", readOnly  = F) %>% 
        hot_cols(colWidths = c(100,50)) %>% 
        hot_col(col = "Action", type = "autocomplete", source = Action) %>% 
        hot_col(col = "LMA", type = "autocomplete", source = LMA) %>% 
        hot_col(col = "State", type = "autocomplete", source = State) %>% 
        hot_col(col = "StatArea", type = "autocomplete", source = StatArea) %>% 
        hot_col(col = "Fishery", type = "autocomplete", source = Fishery) %>% 
        hot_col(col = "Shapefile", strict = F, type = "autocomplete", source = shapefile_names) %>% 
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
  
  #Observes the "Run Model" button-------------------------------------------------------------------
  observeEvent(input$run, {
    
    #Converts table input into something shiny can use 
    
    param <- hot_to_r(input$hot) 
    param[is.na(param)] <- ""
    param$Action <- as.character(param$Action)
    param$LMA <- as.character(param$LMA)
    param$State <- as.character(param$State)
    param$StatArea <- as.character(param$StatArea)
    param$Fishery <- as.character(param$Fishery)
    param$Shapefile <- as.character(param$Shapefile)
    param$Months <- as.character(param$Months)
    param <- param %>% dplyr::filter(Action != "")
    
    
    #Saves output and runs model
    print("Saving parameters to file.")
    write.csv(param, 
              file = paste0(here::here("InputSpreadsheets",input$filename),".csv"), na="",row.names = F)
    
    #Run decision tool function here. Will print messages associated w/ function in UI
    withCallingHandlers({
      shinyjs::html("run-text", "")
      tryCatch({
        print('About to run decision tool function.')
        run_decisiontool(HD=here::here(),InputSpreadsheetName=paste0(input$filename,".csv"))

      },
      error = function(e){
        message("Error in decision tool function.")
      })
      
    },
    message = function(m) {
      shinyjs::html(id = "run-text", html = paste0(m$message,"<br>"), add = TRUE)
    })
    
  })
  
  #View output tab-----------------------------------------------------------------------------------
  
  ### Function to read in png files
  read.image <- function(image.file){
    im <- load.image(image.file)
    if(dim(im)[4] > 3){
      im <- imappend(channels(im, 1:3), 'c')
    }
    im
  }
  
  ### Functions for image zooming-----------------------------------------------------------------------
  ## Code for image zoom was written by Jacob Fiksel
  ## See https://jfiksel.github.io/2017-02-26-cropping_images_with_a_shiny_app/
  
  
  #A function to plot the images after they've been loaded
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
  
  #Initialize images as reactive-------------------------------------------------------------------------
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
  
  #A function to identify file paths for results--------------------------------------------------------
  find_result <- function(){
    
    if (input$filename == "") {
      scenario_path <- paste0(here::here("Scenarios",input$existing_scenarios))
    } else {
      scenario_path <- paste0(here::here("Scenarios",input$filename))
    }
    
    print(paste0("Model output saved in ",scenario_path))
    
    matched_plots <- list.files(scenario_path) [str_which(list.files(scenario_path),
                                                          str_remove(input$select_plots, " "))]
    print(matched_plots)
    if (input$log_plots){
      matched_plots <- matched_plots[str_which(matched_plots, "Log")] 
    } else {
      matched_plots <- matched_plots[str_which(matched_plots, "Log", negate = T)] 
    }
    
    matched_plots <- file.path(scenario_path,matched_plots)
    return(matched_plots)
  }
  
  #Validation function to prevent Shiny from loading images without a path to start from-----------------
  validate_function <- function() {
    if (input$filename == "" & input$existing_scenarios == ""){
      "Please select an existing scenario or create your own."
    } else {
      NULL
    }
  }
  
  validate_header <- function() {
    if (input$filename == "" & input$existing_scenarios == ""){
      "No results found."
    } else {
      NULL
    }
  }
  
  #Implement the validation function and make the filenames reactive  
  matched_plots <- reactive({
     # Make sure requirements are met before looking for results
    validate(
      validate_function()
    ) 
    find_result()
  })
  
  #Plots files found using the functions above-----------------------------------------------------------
  
  output$plot_scenario_name <- renderText({
    if (input$filename == "") {
      validate(
        validate_header()
      ) 
      scenario <- paste0("Results from ",input$existing_scenarios)
    } else {
      scenario <- paste0("Results from ",input$filename)
    }
      scenario
  })
  
  output$table_scenario_name <- renderText({
    if (input$filename == "") {
      validate(
        validate_header()
      ) 
      scenario <- paste0("Results from ",input$existing_scenarios)
    } else {
      scenario <- paste0("Results from ",input$filename)
    }
    scenario
  })
  
  #Left plot
  output$plot1 <- renderPlot({
    print(paste0("Figure filenames:",matched_plots()))
    v$originalImage <- read.image(matched_plots()[1])
    v$imgclick.x <- NULL
    v$imgclick.y <- NULL
    app.plot(v$originalImage,v$imgclick.x, v$imgclick.y)
    
  }, width = 675, height = 750)
  
  #Right plot
  output$plot2 <- renderPlot({
    print(paste0("Figure filenames:",matched_plots()))
    v2$originalImage <- read.image(matched_plots()[2])
    v2$imgclick.x <- NULL
    v2$imgclick.y <- NULL
    app.plot(v2$originalImage,v2$imgclick.x, v2$imgclick.y)
    
  }, width = 675, height = 750)
  
  find_tables <- function(){
    
    if (input$filename == "") {
      scenario_path <- here::here(sprintf("Scenarios/%s/%s_OutputData.csv",
                                          input$existing_scenarios,
                                          input$existing_scenarios))
      # scenario_path <- paste0("Scenarios/",input$existing_scenarios,"/", input$existing_scenarios, "OutputData.csv")
    } else {
      scenario_path <- here::here(sprintf("Scenarios/%s/%s_OutputData.csv",
                                          input$filename,
                                          input$filename))
      # scenario_path <- paste0("Scenarios/",input$filename,"/")
    }
    
    print(paste0("Results in ",scenario_path))
    return(scenario_path)
  }
  
  #Implement the validation function and make the filenames reactive  
  matched_tables <- reactive({
    # Make sure requirements are met before looking for results
    validate(
      validate_function()
    ) 
    find_tables()
  })
  
  output$RelativeRisk = DT::renderDT({  #Tables loaded from the proper Scenario folder
    # dat <- read.csv("Scenarios/ExampleRun/ExampleRun_OutputData.csv", stringsAsFactors = FALSE) %>% 
    dat <- read.csv(matched_tables(), stringsAsFactors = FALSE) %>%
      dplyr::mutate(Default = round(Default, 0),
                    Scenario = round(Scenario, 0),
                    Reduction = paste0(round(Reduction, 2) * 100, "%")) %>% 
      dplyr::filter(Variable == "RelativeRisk") %>% 
      dplyr::select(-Variable) %>% 
      DT::datatable(., rownames = FALSE,
                    options = list(pageLength = 13, 
                                   dom = 't',
                                   autoWidth = TRUE,
                                   columnDefs = list(list(
                                     className = 'dt-right', targets = c(0,3)
                                   ))
                    ))
  })
  
  output$VerticalLines = DT::renderDT({  #Tables loaded from the proper Scenario folder
    # dat <- read.csv("Scenarios/ExampleRun/ExampleRun_OutputData.csv", stringsAsFactors = FALSE) %>% 
    dat <- read.csv(matched_tables(), stringsAsFactors = FALSE) %>%
      dplyr::mutate(Default = round(Default, 0),
                    Scenario = round(Scenario, 0),
                    Reduction = paste0(round(Reduction, 2) * 100, "%")) %>% 
      dplyr::filter(Variable == "VerticalLines") %>% 
      dplyr::select(-Variable) %>% 
      DT::datatable(., rownames = FALSE,
                    options = list(pageLength = 13, 
                                   dom = 't',
                                   autoWidth = TRUE,
                                   columnDefs = list(list(
                                     className = 'dt-right', targets = c(0,3)
                                   ))
                    ))
  })
  
  output$TrapsFished = DT::renderDT({  #Tables loaded from the proper Scenario folder
    # dat <- read.csv("Scenarios/ExampleRun/ExampleRun_OutputData.csv", stringsAsFactors = FALSE) %>% 
    dat <- read.csv(matched_tables(), stringsAsFactors = FALSE) %>%
      dplyr::mutate(Default = round(Default, 0),
                    Scenario = round(Scenario, 0),
                    Reduction = paste0(round(Reduction, 2) * 100, "%")) %>% 
      dplyr::filter(Variable == "TrapsFished") %>% 
      dplyr::select(-Variable) %>% 
      DT::datatable(., rownames = FALSE,
                    options = list(pageLength = 13, 
                                   dom = 't',
                                   autoWidth = TRUE,
                                   columnDefs = list(list(
                                     className = 'dt-right', targets = c(0,3)
                                   ))
                    ))
  })
  
  output$Trawls = DT::renderDT({  #Tables loaded from the proper Scenario folder
    # dat <- read.csv("Scenarios/ExampleRun/ExampleRun_OutputData.csv", stringsAsFactors = FALSE) %>% 
    dat <- read.csv(matched_tables(), stringsAsFactors = FALSE) %>%
      dplyr::mutate(Default = round(Default, 0),
                    Scenario = round(Scenario, 0),
                    Reduction = paste0(round(Reduction, 2) * 100, "%")) %>% 
      dplyr::filter(Variable == "Trawls") %>% 
      dplyr::select(-Variable) %>% 
      DT::datatable(., rownames = FALSE,
                    options = list(pageLength = 13, 
                                   dom = 't',
                                   autoWidth = TRUE,
                                   columnDefs = list(list(
                                     className = 'dt-right', targets = c(0,3)
                                   ))
                    ))
  })
  
  output$renderedReadme <- renderUI({           
    includeHTML(rmarkdown::render(input = "readme.md", "html_document"))
  })
}
