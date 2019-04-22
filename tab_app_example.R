
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

ui = shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  # sidebarLayout(
  #   sidebarPanel(
  #     sliderInput("bins",
  #                 "Number of bins:",
  #                 min = 1,
  #                 max = 50,
  #                 value = 30)
  #   ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(
          "2 columns example",
          fluidRow(
            column(width = 6,
                   h2("Relative Risk"),
                   DT::dataTableOutput('RelativeRisk')),
            column(width = 6,
                   h2("Vertical Lines"),
                   DT::dataTableOutput('VerticalLines'))
            ),
          fluidRow(
            column(width = 6,
                   h2("Traps Fished"),
                   DT::dataTableOutput('TrapsFished')),
            column(width = 6,
                   h2("Trawls"),
                   DT::dataTableOutput('Trawls')
            ))
        )
        
      )
    )
))

library(shiny)
library(DT)
datatable(iris)

server = shinyServer(function(input, output) {
  
  # output$distPlot <- renderPlot({
  #   
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2]
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  #   
  # })
  dat <- read.csv("Scenarios/ExampleRun/ExampleRun_OutputData.csv", stringsAsFactors = FALSE)

  dat <- dat %>% 
    dplyr::mutate(Default = round(Default, 0),
           Scenario = round(Scenario, 0),
           Reduction = paste0(round(Reduction, 2) * 100, "%"))
  
  X <- split(dat, dat$Variable)
  X <- lapply(X, function(x) x[!(names(x) %in% c("Variable"))])  
  
  
  output$RelativeRisk = DT::renderDataTable(X$RelativeRisk,
                                           rownames = FALSE,
                                           options = list(pageLength = 13, dom = 't'))
  
  output$Trawls = DT::renderDataTable(X$Trawls,
                                      rownames = FALSE,
                                      options = list(pageLength = 13, dom = 't'))
  
  
  output$TrapsFished = DT::renderDataTable(X$TrapsFished,
                                           rownames = FALSE,
                                           options = list(pageLength = 13, dom = 't'))
  
  output$VerticalLines = DT::renderDataTable(X$VerticalLines,
                                      rownames = FALSE,
                                      options = list(pageLength = 13, dom = 't'))
  
})


shinyApp(ui = ui, server = server)




  
  