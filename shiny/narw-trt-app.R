library(shinydashboard)

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
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)