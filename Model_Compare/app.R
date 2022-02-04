
library(shiny)
library(tidyverse)
library(ctsem)
library(plotly)
library(rstan)
library(kableExtra)
options(shiny.maxRequestSize=500*1024^2)
load("~/dynamic_bulldoze2/agencynames.rda")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Compare Latent Models"),
  
  fluidRow(
    column(6, fileInput("model1", "Upload Model 1")),
    column(6, fileInput("model2", "Upload Model 2"))
  ),
  
  fluidRow(
    column(6, plotlyOutput("latentplot1", height = "800px")),
    column(6, plotlyOutput("latentplot2", height = "800px"))
  ),
  
  fluidRow(
    column(6, plotlyOutput("maniplot1", height = "1100px")),
    column(6, plotlyOutput("maniplot2", height = "1100px"))
  ),
  
  fluidRow(
    checkboxGroupInput("agencies",
                       "Select Agencies to Plot",
                       choices = agency_names,
                       selected = agency_names,
                       width = "100%")
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$latentplot1 <- renderPlotly({
    
    file1 = input$model1
    load(file1$datapath)
    kalman.data1 = ctKalman(model, 
                           timestep = .25,
                           subjects = 1:length(model$setup$idmap$new))
    g = kalman.data1 %>%
      filter(Element %in% c("etasmooth")) %>%
      ggplot(aes(x = Time, y = value)) +
      geom_line(aes(color = Subject)) +
      facet_wrap(~Row, ncol = 1) +
      theme_bw() +
      theme(strip.background =element_rect(fill="black")) +
      theme(strip.text = element_text(colour = 'white')) +
      theme(legend.position = "none") +
      ggtitle("Latent Values") +
      ylab("") +
      xlab("")
    
    ggplotly(g)
    
    
  })
    
    output$latentplot2 <- renderPlotly({
      
      file2 = input$model2
      load(file2$datapath)
      kalman.data2 = ctKalman(model, 
                              timestep = .25,
                              subjects = 1:length(model$setup$idmap$new))
      g = kalman.data2 %>%
        filter(Element %in% c("etasmooth")) %>%
        ggplot(aes(x = Time, y = value)) +
        geom_line(aes(color = Subject)) +
        facet_wrap(~Row, ncol = 1) +
        theme_bw() +
        theme(strip.background =element_rect(fill="black")) +
        theme(strip.text = element_text(colour = 'white')) +
        theme(legend.position = "none") +
        ggtitle("Latent Values") +
        ylab("") +
        xlab("")
      
      ggplotly(g)
      
      
    })

    output$maniplot1 <- renderPlotly({
      
      file1 = input$model1
      load(file1$datapath)
      kalman.data1 = ctKalman(model, 
                              timestep = .25,
                              subjects = 1:length(model$setup$idmap$new))
      d1 = kalman.data1 %>%
        filter(Element %in% c("y"))
      
      d2 = kalman.data1 %>%
        filter(Element %in% c("ysmooth"))
      
      g = kalman.data1 %>%
        filter(Element %in% c("y", "ysmooth")) %>%
        ggplot(aes(x = Time, y = value)) +
        geom_point(aes(color = Subject, group = Element), data = d1) +
        geom_line(aes(color = Subject, group = Element), data = d2) +
        facet_wrap(~Row, ncol = 1) +
        theme_bw() +
        theme(strip.background =element_rect(fill="black")) +
        theme(strip.text = element_text(colour = 'white')) +
        theme(legend.position = "none") +
        ggtitle("Manifest Values") +
        ylab("") +
        xlab("Year")
      
      ggplotly(g)
      
      
    })
    
    output$maniplot2 <- renderPlotly({
      
      file2 = input$model2
      load(file2$datapath)
      kalman.data2 = ctKalman(model, 
                              timestep = .25,
                              subjects = 1:length(model$setup$idmap$new))
      d1 = kalman.data2 %>%
        filter(Element %in% c("y"))
      
      d2 = kalman.data2 %>%
        filter(Element %in% c("ysmooth"))
      
      g = kalman.data2 %>%
        filter(Element %in% c("y", "ysmooth")) %>%
        ggplot(aes(x = Time, y = value)) +
        geom_point(aes(color = Subject, group = Element), data = d1) +
        geom_line(aes(color = Subject, group = Element), data = d2) +
        facet_wrap(~Row, ncol = 1) +
        theme_bw() +
        theme(strip.background =element_rect(fill="black")) +
        theme(strip.text = element_text(colour = 'white')) +
        theme(legend.position = "none") +
        ggtitle("Manifest Values") +
        ylab("") +
        xlab("Year")
      
      ggplotly(g)
      
      
    })
    
  
  
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
