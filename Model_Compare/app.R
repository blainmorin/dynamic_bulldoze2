

library(shiny)
library(tidyverse)
library(ctsem)
options(shiny.maxRequestSize=200*1024^2)


# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Compare Dynamic Models"),

    fluidRow(
        column(6, fileInput("model1", h3("Upload Model 1"))),
        column(6, fileInput("model2", h3("Upload Model 2")))
    ),
    
    fluidRow(
        
        column(6, plotOutput("plot1")),
        column(6, plotOutput("plot2"))
        
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    
    output$plot1 = renderPlot({
        file1 = input$model1
        load(file1$datapath)
        test = ctKalman(model)
        one = plot(test, plot = FALSE)
        one
        
        })
    
    output$plot2 = renderPlot({
        file2 = input$model2
        load(file2$datapath)
        test = ctKalman(model)
        two = plot(test, plot = FALSE)
        two
        })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
