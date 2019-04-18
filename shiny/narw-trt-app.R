library(shinydashboard)
library(htmlwidgets)
library(rhandsontable)

source("R/model-specs.R")


ui <- dashboardPage(
  dashboardHeader(title = "NARW TRT Scenario Planning", titleWidth = 300),
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
    rhandsontable(DF, stretchH = "all") %>% 
      hot_col(col = "Action", type = "dropdown", source = Action) %>% 
      hot_col(col = "LMAs", type = "dropdown", source = LMAs) %>% 
      hot_col(col = "States", type = "dropdown", source = States) %>% 
      hot_col(col = "Fishery", type = "dropdown", source = Fishery) %>% 
      hot_col(col = "StatArea", type = "dropdown", source = StatArea) %>% 
      hot_col(col = "TrapRedistributionArea", type = "dropdown", source = TrapRedistributionArea) %>% 
      hot_col(col = "TrapRedistributionMethod", type = "dropdown", source = TrapRedistributionMethod)

  })
}

shinyApp(ui, server)
