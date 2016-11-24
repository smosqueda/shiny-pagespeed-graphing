library(shiny)
#source("helper.R")

#library(survival)
# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("PageSpeed Stats"),
    
  fluidRow(
  
      column(2,
        wellPanel(
          sliderInput("days", "Number of Days:",  
                    min = 1, max = 30, value = 7, step = 1)
          )
      )
    ,
    #  column(6,
    mainPanel(
      tabsetPanel(type = "tabs", size="100%",
                  tabPanel("Plots", 
                          plotOutput("ggPlotVersion"),
                          plotOutput("plotData", width = "900px", height="900px")
                          )
        
                  )
      
    )

)

))

