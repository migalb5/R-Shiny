
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
      selectInput("group_by", "Select Group By Field:", choices = names(dataset), selected = "neighbourhood"),
      selectInput("filter1", "Filter by Room Type:", choices = c("All", as.list(unique(dataset$room_type))) , selected = "All"),
      sliderInput("filter2", "Filter by range of Minimum Nights:",
                  min = min(dataset$minimum_nights),
                  max = max(dataset$minimum_nights),
                  value = c(min(dataset$minimum_nights), min(dataset$minimum_nights) + 1),
                  step = 1)
    )
  ),
#  dashboardFooter(left = "Created on 2025-03-12"
#  ),
  bs4DashBody(
    fluidRow(
      valueBox(subtitle = "Average price (EUR) of listings", value = "", width = 2),
      valueBox(subtitle = "Highest price (EUR) of listings", value = "30", width = 8),
      valueBox(subtitle = "Lowest price (EUR) of listings", value = "30", width = 2),
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
    dataset %>%
      group_by(.data[[input$group_by]]) %>%
      summarise(Amount_Listings = n(),
               Avg_Price_EUR = mean(price), .groups = "drop") %>%
      arrange(desc(Avg_Price_EUR))
  })

  summary_data2 <- reactive({
    req(input$group_by)  # Ensure input is available
    req(input$filter1)
    req(input$filter2)
    dataset %>%
      filter(input$group_by == .data[[input$group_by]]) %>%
      filter(ifelse(input$filter1 == "All", TRUE, room_type == input$filter1)) %>%
      filter(maximum_nights >= input$filter2[1], maximum_nights <= input$filter2[2])
  })

  # average_price = mean(d$price)
  # highest_price = max(summary_data$price)
  # lowest_price = min(summary_data$price)
  # 
  # output$view <- renderbs4ValueBox(average_price)
    
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

