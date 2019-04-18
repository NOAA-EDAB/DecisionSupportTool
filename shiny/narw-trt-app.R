library(shinydashboard)
library(rhandsontable)

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
                  rHandsontableOutput("hot", width = 700),
                  width = 10
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
    rhandsontable(do.call(cbind, lapply(1:10, function(i) data.table(rnorm(15)))))
  })
}

shinyApp(ui, server)