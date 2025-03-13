
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


ui <- bs4DashPage(preloader = preloader,
  dashboardHeader(title = "Airbnb Average Price in The Hague (NL)"),
  bs4DashSidebar(width = '300px', minified = TRUE, expandOnHover = TRUE,
    sidebarMenu(
      menuItem("Chart", tabName = "chart", icon = icon("dashboard")),
      menuItem("Table", tabName = "table", icon = icon("th")),
      selectInput("filter1", "Filter by Room Type:", choices = c("All", as.list(unique(dataset$room_type))) , selected = "All"),
      sliderInput("filter2", "Filter by range of Minimum Nights:",
                  min = min(dataset$minimum_nights),
                  max = max(dataset$minimum_nights),
                  value = c(min(dataset$minimum_nights), min(dataset$minimum_nights) + 1),
                  step = 1),br(),
      selectInput("group_by", "Select Group By Field:", choices = names(dataset), selected = "neighbourhood")
    )
  ),
#  dashboardFooter(left = "Created on 2025-03-12"
#  ),
  bs4DashBody(
    fluidRow(
      bs4ValueBox(subtitle = "Average price (EUR) of listings", bs4ValueBoxOutput("filtered_mean"), width = 6, icon = icon("thumbs-up"), color = "primary"),
      bs4ValueBox(subtitle = "Highest price (EUR) of listings", bs4ValueBoxOutput("filtered_max"), width = 3, icon = icon("thumbs-down"), color = "secondary"),
      bs4ValueBox(subtitle = "Lowest price (EUR) of listings", bs4ValueBoxOutput("filtered_min"), width = 3, icon = icon("thumbs-up"), color = "secondary")
    ),
    tabItems(
      tabItem(tabName = "chart",
        # Boxes need to be put in a row (or column)
        fluidRow(
          box(width = 12, plotOutput("barChart"), title = "Chart")
        )
      ),
      tabItem(tabName = "table",
        fluidRow(
          box(tableOutput("view"), title = "Table")
        )
      )
    )
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Reactive summary table based on selected group
  summary_data <- reactive({
    req(input$group_by)  # Ensure input is available
    req(input$filter1)
    req(input$filter2)
    dataset %>%
      filter(ifelse(input$filter1 == "All", TRUE, room_type == input$filter1)) %>%
<<<<<<< HEAD
      filter(minimum_nights >= input$filter2[1], minimum_nights <= input$filter2[2]) %>%
=======
      filter(maximum_nights >= input$filter2[1], maximum_nights <= input$filter2[2]) %>%
>>>>>>> b7d57e1eb06bb8d927f529c406c2a1a9b0ed991e
      group_by(.data[[input$group_by]]) %>%
      summarise(Amount_Listings = n(),
               Avg_Price_EUR = mean(price), .groups = "drop") %>%
      arrange(desc(Avg_Price_EUR))
  })

  summary_data_pre <- reactive({
    req(input$group_by)  # Ensure input is available
    req(input$filter1)
    req(input$filter2)
    dataset %>%
      filter(ifelse(input$filter1 == "All", TRUE, room_type == input$filter1)) %>%
      filter(minimum_nights >= input$filter2[1], minimum_nights <= input$filter2[2])
  })

  output$filtered_mean <- renderbs4ValueBox({
    mean(summary_data_pre()$price)
  })
  output$filtered_max <- renderbs4ValueBox({
    max(summary_data_pre()$price)
  })  
  output$filtered_min <- renderbs4ValueBox({
    min(summary_data_pre()$price)
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

