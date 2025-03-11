
library(shiny)
library(rio)
library(dplyr)
library(plotly)
library(ggplot2)

# Define the local file path (update this with the path to your file)
local_file_path <- "data/listings.csv"  # Change this to your local file path

# Import the dataset using RIO
dataset <- rio::import(local_file_path)

dataset <- dataset[!is.na(dataset$name) & !is.na(dataset$neighbourhood) & !is.na(dataset$room_type) & !is.na(dataset$minimum_nights) & !is.na(dataset$price), ]


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Airbnb Average Prices in The Hague (NL)"),
  
   # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput("group_by", "Select Group By Field:", choices = names(dataset), selected = "neighbourhood")

      # checkboxGroupInput("cols", "Select columns to display", 
      #                    choices = NULL, selected = NULL)
            
      # # Input: Slider for the number of bins ----
      # sliderInput(inputId = "bins",
      #             label = "Number of bins:",
      #             min = 1,
      #             max = 50,
      #             value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tableOutput("view"),
      
      plotOutput("barChart")
      
      # plotlyOutput("pie_chart")
      
      # # Output: Histogram ----
      # plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {


  # # Reactive dataset with selective NA removal
  # cleaned_data <- reactive({
  #   data <- dataset
  #   if (input$remove_na) {
  #     data <- drop_na(data, c(name, neighbourhood, room_type, minimum_nights, price))  # Remove NA only from these columns
  #   }
  #   data
  # })
    
  # Reactive summary table based on selected group
  summary_data <- reactive({
    

    req(input$group_by)  # Ensure input is available
    dataset %>%
      group_by(.data[[input$group_by]]) %>%
      summarise(Count = n(),
               Avg_Price_EUR = mean(price), .groups = "drop") %>%
      arrange(desc(Avg_Price_EUR))
  })
  
  output$view <- renderTable({
    summary_data()
  })
  
  output$barChart <- renderPlot({
    ggplot(summary_data(), aes(x = .data[[input$group_by]], y = Avg_Price_EUR)) +
      geom_bar(stat = "identity", fill = "skyblue") +  # Create bar chart
      theme_minimal() +
      labs(x = "Comparison criteria selected", y = "Average Price (€)", title = "Airbnb Average Price in the Hague (NL)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
    # output$pie_chart <- renderPlotly({
    #   plot_ly(summary_data(), labels = ~neighbourhood, values = ~Avg_Price_EUR, type = "pie") %>%
    #     layout(title = "Dimension Distribution")
  
  # observe({
  #   updateCheckboxGroupInput(session, "cols", choices = colnames(dataset))
  # })
  
  # # Reactive expression to filter dataset based on selected columns
  # filteredData <- reactive({
  #   req(input$cols)  # Ensure that columns are selected
  #   dataset[, c("name", "neighbourhood", "room_type", "minimum_nights", "price"), drop = FALSE]  # Select only the chosen columns
  # })
  

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
shinyApp(ui, server)
#shinyApp(ui, server, options = list(display.mode = "showcase"))