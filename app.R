library(shiny)

# Load the data
tesla_data <- read.csv("TSLA.csv")

# Convert the Date column to Date type
tesla_data$Date <- as.Date(tesla_data$Date)

# UI
ui <- fluidPage(
  titlePanel("Tesla Stock Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("startDate",
                  "Select Start Date:",
                  choices = unique(tesla_data$Date),
                  selected = min(tesla_data$Date)),
      selectInput("endDate",
                  "Select End Date:",
                  choices = unique(tesla_data$Date),
                  selected = max(tesla_data$Date))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Closing Price Scatter Plot", plotOutput("scatterPlot")),
        tabPanel("Volume Histogram", plotOutput("histogram")),
        tabPanel("Open vs Close Prices Line Plot", plotOutput("linePlot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    tesla_data[tesla_data$Date >= input$startDate & tesla_data$Date <= input$endDate, ]
  })
  
  output$scatterPlot <- renderPlot({
    data <- filtered_data()
    plot(data$Date, data$Close, 
         main = "Scatter Plot of Closing Prices",
         xlab = "Date", ylab = "Closing Price", 
         pch = 19, col = "lightblue")
  })
  
  output$histogram <- renderPlot({
    data <- filtered_data()
    hist(data$Volume, 
         main = "Histogram of Trading Volume",
         xlab = "Volume",
         col = "lavender", breaks = 30)
  })
  
  output$linePlot <- renderPlot({
    data <- filtered_data()
    plot(data$Date, data$Open, 
         type = "l", col = "black", 
         xlab = "Date", ylab = "Price", 
         main = "Open vs Close Prices Over Time")
    lines(data$Date, data$Close, col = "purple")
    legend("topright", legend = c("Open", "Close"), col = c("black", "purple"), lty = 1)
  })
}

# Run the application
shinyApp(ui = ui, server = server)