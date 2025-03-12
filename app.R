
library(shiny)
library(rio)
library(dplyr)
library(ggplot2)
library(bs4Dash)
library(waiter)

# Define the local file path (update this with the path to your file)
local_file_path <- "data/listings.csv"  # Change this to your local file path

# Import the dataset using RIO
dataset <- rio::import(local_file_path)

dataset <- dataset[!is.na(dataset$name) & !is.na(dataset$neighbourhood) & !is.na(dataset$room_type) & !is.na(dataset$minimum_nights) & !is.na(dataset$price), ]

preloader <- list(html = tagList(spin_1(), "Loading ..."), color = "#343a40")

# ui = dashboardPage(
#   title = "Airbnb Average Price in The Hague (NL)",
#   preloader = preloader,
#   header = dashboardHeader(
#     title = "Dashboard"
#   ),
#   sidebar = dashboardSidebar(
#     selectInput("group_by", "Select Group By Field:", choices = names(dataset), selected = "neighbourhood"),
#   ),
#   controlbar = dashboardControlbar(),
#   footer = dashboardFooter(),
#   body = dashboardBody()
# )


ui <- dashboardPage(preloader = preloader,
  dashboardHeader(title = "Airbnb Average Price in The Hague (NL)"),
  dashboardSidebar(
    selectInput("group_by", "Select Group By Field:", choices = names(dataset), selected = "neighbourhood"),
    sidebarMenu(
      menuItem("Chart", tabName = "chart", icon = icon("dashboard")),
      menuItem("Table", tabName = "table", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "chart",
        # Boxes need to be put in a row (or column)
        fluidRow(
          box(width = 12, plotOutput("barChart"))
          )
      ),
      tabItem(tabName = "table",
        fluidRow(
          box(tableOutput("view"))
        )
      )
    )
  )
)


# # Define UI for app that draws a histogram ----
# ui <- fluidPage(
#   
#   # App title ----
#   titlePanel("Airbnb Average Price in The Hague (NL)"),
#   
#    # Sidebar layout with input and output definitions ----
#   sidebarLayout(
#     
#     # Sidebar panel for inputs ----
#     sidebarPanel(width=2,
#       
#       selectInput("group_by", "Select Group By Field:", choices = names(dataset), selected = "neighbourhood")
# 
#     ),
#     
#     # Main panel for displaying outputs ----
#     mainPanel(width = 10,
#       
#       plotOutput("barChart"), br(), br(), br(),
#       
#       tableOutput("view")
#       
#     )
#   )
# )

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {


  # Reactive summary table based on selected group
  summary_data <- reactive({
    req(input$group_by)  # Ensure input is available
    dataset %>%
      group_by(.data[[input$group_by]]) %>%
      summarise(Amount_Listings = n(),
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
      labs(x = as.character(input$group_by), y = "Average Price (€)", title = "Airbnb Average Price in The Hague (NL)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.title.x = element_text(face = "bold", size = 14))
  })
  
}

# Create Shiny app ----
#shinyApp(ui, server, options = list(display.mode = "showcase"))
shinyApp(ui, server)

