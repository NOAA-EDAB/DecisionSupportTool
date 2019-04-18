library(shinydashboard)
library(htmlwidgets)
library(rhandsontable)
# from Burtons Code
library(rgdal)
library(sp)
library(maps)
library(maptools)
library(grid)
library(gtable)
library(gridExtra)
library(maptools)

r.dir <- here::here("R")
source(file.path(r.dir,"model-specs.R"))
source(here::here("function_DecisionSupportTool_V1.2.R"))
source(file.path(r.dir,"run_decisiontool.R"))


ui <- dashboardPage(
  dashboardHeader(title = "ALW TRT Scenario Planning", titleWidth = 300),
  dashboardSidebar(    
    sidebarMenu(
    menuItem("Specify Model", tabName = "specify_model", icon = icon("dashboard")),
    menuItem("View Output", tabName = "view_output", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # First tab content
      tabItem(tabName = "specify_model",
              h4("Specify scenarios and scenario parameters"),
              fluidRow(
                box(
                  textInput("filename", label = "Enter scenario name", value = NULL)
                ),
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
        )
      )
    )
  )


server <- function(input, output) {
  output$hot = renderRHandsontable({
    rhandsontable(DF, stretchH = "all", readOnly  = F) %>% 
      hot_col(col = "Action", type = "autocomplete", source = Action) %>% 
      hot_col(col = "LMA", type = "autocomplete", source = LMA) %>% 
      hot_col(col = "States", type = "autocomplete", source = States) %>% 
      hot_col(col = "Fishery", type = "autocomplete", source = Fishery) %>% 
      hot_col(col = "StatArea", type = "autocomplete", source = StatArea) %>% 
      hot_col(col = "TrapRedistributionArea", type = "autocomplete", source = TrapRedistributionArea) %>% 
      hot_col(col = "TrapRedistributionMethod", type = "autocomplete", source = TrapRedistributionMethod) %>% 
      hot_col(col = "Months", type = "autocomplete", source = Months) %>% 
      hot_col(col = "Percentage", type = "numeric", strict = F) %>% 
      hot_col(col = "Shapefile", strict = F, type = "autocomplete")
    
  })
  
  observeEvent(input$run, {
    
    param <- hot_to_r(input$hot)
    param[is.na(param)] <- ""
    
    if (is.null(input$filename)){
      warning("Enter filename for scenario run.")
    } else {
      write.csv(param, 
                file = paste0(file.path("InputSpreadsheets",input$filename),".csv"), row.names = F)
      print("Saved.")
      run_decisiontool(HD=here::here(),InputSpreadsheetName=paste0(input$filename,".csv"))
    }
    
    
  })
  
}

shinyApp(ui, server)