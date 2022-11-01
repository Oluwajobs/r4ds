# Load shiny package
library(shiny)

#-----------Start of UI---------------------------#

# Define UI for app that draws a histogram
# Fluidpage creates a page with fluid layout
ui <- fluidPage(
  
  # App title ---
  titlePanel("Old Faithful Geyser"),
  
  # Sidebar layout with input and output definitions ---
  sidebarLayout(  
  # within the sidebarLayout() we have the sidebarPanel and mainPanel
    
  # Sidebar panel for input ---
    sidebarPanel(
      
      # Input: Slider for the number of bins ---
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 30)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output Histogram -----
      plotOutput(outputId = "distPlot")
    )
  )
)
#-----------End of UI---------------------------#


#-----------Start of Server----------------------#

# Define server logic required to plot a histogram ---
server <- function(input, output) {
  #' Histogram of the Old Faithful Geyser Data with requested
  #' number of bins
  #' This expression that generates a histogram is wrapped in a call
  #' to renderPlot() to indicate that:
  #' 
  #' 1. It is 'reactive' and therefore should be automatically
  #'    re-executed when inputs (input$bins) change (remember the input with inputId in ui)
  #'  
  #' 2. Its output is a plot
  
  # our distPlot interpolation maps with the outputId we defined in ui earlier
  output$distPlot <-  renderPlot({  
    
    # taking a data set
    x <- faithful$waiting
    # creating equal bins using the sequence.... seq() function
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # plotting the hist
    hist(x, breaks=bins, col="#75AADB", border="orange",
         xlab = "Waiting time for next eruption (in mins)",
         name = "Histogram of waiting times"),
        type= "bar"
  })
  
}
  
  
  

# ---------------Make a call to shinyApp-----------#
shinyApp(ui = ui, server = server)