install.packages("shiny", "rio")
library(shiny)
library(rio)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Airbnb Average Prices in The Hague (NL)"),
  
   # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(

      textOutput("filename")
            
      # # Input: Slider for the number of bins ----
      # sliderInput(inputId = "bins",
      #             label = "Number of bins:",
      #             min = 1,
      #             max = 50,
      #             value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tableOutput("view")
      
      # # Output: Histogram ----
      # plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Define the local file path (update this with the path to your file)
  local_file_path <- "data/listings.csv"  # Change this to your local file path
  
  # Import the dataset using RIO
  dataset <- rio::import(local_file_path)
  
  # Display the file name (for reference)
  output$filename <- renderText({
    paste("Data imported from:", local_file_path)
  })
  
  # Render the dataset as a table
  output$view <- renderTable({
    dataset
    
  # # Histogram of the Old Faithful Geyser Data ----
  # # with requested number of bins
  # # This expression that generates a histogram is wrapped in a call
  # # to renderPlot to indicate that:
  # #
  # # 1. It is "reactive" and therefore should be automatically
  # #    re-executed when inputs (input$bins) change
  # # 2. Its output type is a plot
  # output$distPlot <- renderPlot({
  #   
  #   x    <- faithful$waiting
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   hist(x, breaks = bins, col = "#75AADB", border = "white",
  #        xlab = "Waiting time to next eruption (in mins)",
  #        main = "Histogram of waiting times")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server, options = list(display.mode = "showcase"))