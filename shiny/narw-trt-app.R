library(shinydashboard)
library(htmlwidgets)
library(rhandsontable)

source("R/model-specs.R")


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
                  rHandsontableOutput("hot", width = "100%"),
                  actionButton(inputId="run",label="Run model"),
                  helpText('Parameterize actions by entering information into the spreadsheet above.
                           Right click and select "Add Row" to incorporate multiple actions into the
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
      hot_col(col = "LMAs", type = "autocomplete", source = LMAs) %>% 
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
    print(param)
    
  })
}

shinyApp(ui, server)
