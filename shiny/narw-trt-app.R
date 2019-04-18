library(shinydashboard)
library(rhandsontable)

# 
# Action <- c("Closure",
#             "Constraint_Fishery",
#             "Constraint_Spatial",
#             "EndlineModification",
#             "MinTrawlLength",
#             "TrapCap",
#             "TrapReduction")
# 
# LMAs <- c("All",
#           "A1",
#           "A2",
#           "A2_3overlap",
#           "A3")
# 
# States <- c("All",
#             "ME",
#             "NH",
#             "MA")
# 
# Fishery <- c("All",
#              "NonExempt",
#              "Exempt",
#              "Midshelf_Lobster",
#              "Midshelf_Crab",
#              "Offshore_Lobster",
#              "Offshore_Crab")
# 
# # select multiple
# StatArea <- c(464, 465, 511,
#               512, 513, 514,
#               515, 521, 522,
#               561, 562, 537,
#               538, 539)
# 
# TrapRedistributionArea <- c("WithinStatArea",
#                             "AdjacentStatAreas",
#                             "AcrossLMA",
#                             "None")
# 
# TrapRedistributionMethod <- c("Even",
#                               "IDW")


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