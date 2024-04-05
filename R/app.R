library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  
  titlePanel("Google Trends Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose a variable:",
                  choices = c("Sepal Length" = "Sepal.Length", 
                              "Sepal Width" = "Sepal.Width",
                              "Petal Length" = "Petal.Length",
                              "Petal Width" = "Petal.Width")),
      sliderInput("n", "Number of observations:",
                  min = 10, max = 100, value = 50)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Generate random data based on user input
  data <- reactive({
    iris[sample(nrow(iris), input$n), ]
  })
  
  # Render plot based on selected variable
  output$plot <- renderPlot({
    ggplot(data(), aes_string(x = input$variable)) +
      geom_histogram(fill = "blue", color = "black") +
      labs(title = paste("Histogram of", input$variable))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
