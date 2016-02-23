library(shiny)

shinyUI(fluidPage(
  titlePanel("Conway's Game of life"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("step", label="Step",
                  min = 1, max = 100, value = 1, step = 1,
                  animate = animationOptions(interval=200, loop=FALSE, playButton=NULL, pauseButton=NULL)),
      sliderInput("prob", "Probability", 0.25, min=0, max=1, step=0.05),
      
      actionButton("Refresh", "Refresh")
    ),
    
    mainPanel(
      plotOutput("golPlot")
    )
  )
))