library(shiny)
library(ggplot2)
library(reshape2)

# Define UI
ui <- fluidPage(
  titlePanel("Google Trends Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "data_type",
        "Choose a data type:",
        choices = c(
          "google trends",
          "mediacloud hurricanes",
          "mediacloud state",
          "mediacloud trump",
          "tv hurricane by network",
          "tv hurricanes",
          "tv state"
        )
      ),
      selectInput(
        "plot_type",
        "Choose a plot type:",
        choices = c(
          "Histogram / Bar Graph",
          "Column Distribution",
          "Correlation Matrix",
          "Scatter Matrix",
          "Line Chart",
          "Time Series Plot"
        ),
        selected = "Histogram / Bar Graph"
      )
    ),
    mainPanel(
      plotOutput("plot", width = "100%", height = "850px"),
      div(style = "font-size: 30px; animation: rainbow-glow 2s infinite;",
          textOutput("scatter_warning"))
    )
  ),
  tags$style(
    HTML(
      "
                @keyframes rainbow-glow {
                0% {
                font-size: 30px;
                color: red;
                }
                50% {
                font-size: 50px;
                color: green;
                }
                100% {
                font-size: 30px;
                color: blue;
                }
                }
                                "
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  data <- reactive({
    req(input$data_type)
    file_name <- switch(
      input$data_type,
      "google trends" = "csv/google_trends_data.csv",
      "mediacloud hurricanes" = "csv/mediacloud_hurricanes_data.csv",
      "mediacloud state" = "csv/mediacloud_state_data.csv",
      "mediacloud trump" = "csv/mediacloud_trump_data.csv",
      "tv hurricane by network" = "csv/tv_hurricane_by_network_data.csv",
      "tv hurricanes" = "csv/tv_hurricane_data.csv",
      "tv state" = "csv/tv_state_data.csv",
      "Line Chart" = "csv/tv_hurricane_by_network_data.csv",
      "Time Series Plot" = "csv/tv_hurricane_by_network_data.csv"
    )
    read.csv(file_name)
  })
  
  # Generate plot based on user input
  output$plot <- renderPlot({
    req(input$data_type)
    data <- data()
    
    if (input$plot_type == "Histogram / Bar Graph") {
      # Melt the data to long format
      melted_data <- melt(data, id.vars = "date")
      
      # Create separate bar graphs for each variable
      p <-
        ggplot(melted_data, aes(
          x = date,
          y = value,
          fill = as.factor(variable)
        )) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap( ~ variable, scales = "free") +
        theme(axis.text.x = element_text(
          angle = 60,
          vjust = 1,
          hjust = 1
        )) +
        labs(fill = "Variable")
      
      print(p)
    } else if (input$plot_type == "Column Distribution") {
      melted_data <- melt(data)
      ggplot(melted_data, aes(value)) +
        geom_histogram(binwidth = input$bins) +
        facet_wrap( ~ variable, scales = "free")
    } else if (input$plot_type == "Correlation Matrix") {
      numeric_data <- subset(data, select = -c(date))
      numeric_data <-
        numeric_data[, sapply(numeric_data, is.numeric)]
      
      corr_matrix <- cor(numeric_data)
      
      ggplot(melt(corr_matrix), aes(Var1, Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient(low = "purple4", high = "orange") +
        theme_minimal()
    } else if (input$plot_type == "Scatter Matrix") {
      if (input$data_type == "tv hurricane by network" ||
          input$data_type == "tv hurricanes") {
        return(paste("Scatter Plot cannot be plotted for this data"))
      } else {
        # Plot scatter matrix for numeric variables
        numeric_data <- subset(data, select = -c(date))
        
        plots <- vector("list", length = ncol(numeric_data) ^ 2)
        for (i in seq_along(plots)) {
          row_index <- ((i - 1) %/% ncol(numeric_data)) + 1
          col_index <- ((i - 1) %% ncol(numeric_data)) + 1
          
          x_var <- names(numeric_data)[col_index]
          y_var <- names(numeric_data)[row_index]
          plots[[i]] <-
            ggplot(data = numeric_data, aes_string(x = x_var, y = y_var)) +
            geom_point() +
            geom_density_2d() +
            theme_minimal()
        }
        # Use grid.arrange only if plots are available
        if (!all(sapply(plots, is.null))) {
          gridExtra::grid.arrange(grobs = plots, ncols = ncol(numeric_data))
        } else {
          return("No plots available for Scatter Matrix.")
        }
      }
    } else if (input$plot_type == "Line Chart") {
      if (!input$data_type == "tv by hurricane network") {
        return(NULL)
      } else {
        ggplot(data, aes(x = date)) +
          geom_line(aes(y = bbc_news, color = "BBC News")) +
          geom_line(aes(y = cnn, color = "CNN")) +
          geom_line(aes(y = fox_news, color = "Fox News")) +
          geom_line(aes(y = msnbc, color = "MSNBC")) +
          labs(color = "Network")
      }
    } else if (input$plot_type == "Time Series Plot") {
      if (input$data_type == "tv by hurricane network") {
        return("Cannot plot data for this type")
      } else {
        ggplot(data, aes(x = date)) +
          geom_line(aes(y = bbc_news, color = "BBC News")) +
          geom_line(aes(y = cnn, color = "CNN")) +
          geom_line(aes(y = fox_news, color = "Fox News")) +
          geom_line(aes(y = msnbc, color = "MSNBC")) +
          labs(color = "Network")
      }
    }
  })
  load_data <- function(data_type, plot_type) {
    switch(
      data_type,
      "google trends" = {
        switch(
          plot_type,
          "histogram" = read.csv("google_trends_data.csv"),
          "correlation matrix" = read.csv("google_trends_data.csv"),
          "scatter matrix" = read.csv("google_trends_data.csv"),
          "four column distribution" = read.csv("google_trends_data.csv"),
          read.csv("google_trends_data.csv")
        )
      },
      "mediacloud hurricanes" = {
        switch(
          plot_type,
          "histogram" = read.csv("mediacloud_hurricanes_data.csv"),
          "correlation matrix" = read.csv("mediacloud_hurricanes_data.csv"),
          "scatter matrix" = read.csv("mediacloud_hurricanes_data.csv"),
          "four column distribution" = read.csv("mediacloud_hurricanes_data.csv"),
          read.csv("mediacloud_hurricanes_data.csv")
        )
      },
      "mediacloud state" = {
        switch(
          plot_type,
          "histogram" = read.csv("mediacloud_state_data.csv"),
          "correlation matrix" = read.csv("mediacloud_state_data.csv"),
          "scatter matrix" = read.csv("mediacloud_state_data.csv"),
          "four column distribution" = read.csv("mediacloud_state_data.csv"),
          read.csv("mediacloud_state_data.csv")
        )
      },
      "mediacloud trump" = {
        switch(
          plot_type,
          "histogram" = read.csv("mediacloud_trump_data.csv"),
          "correlation matrix" = read.csv("mediacloud_trump_data.csv"),
          "scatter matrix" = read.csv("mediacloud_trump_data.csv"),
          "four column distribution" = read.csv("mediacloud_trump_data.csv"),
          read.csv("mediacloud_trump_data.csv")
        )
      },
      "tv hurricane by network" = {
        switch(
          plot_type,
          "four column distribution" = read.csv("tv_hurricane_by_network_data.csv"),
          "histogram" = read.csv("tv_hurricane_by_network_data.csv"),
          "correlation matrix" = read.csv("tv_hurricane_by_network_data.csv"),
          "scatter matrix" = NULL,
          # Scatter matrix not supported for this data type
          read.csv("tv_hurricane_by_network_data.csv")
        )
      },
      "tv hurricanes" = {
        switch(
          plot_type,
          "histogram" = read.csv("tv_hurricane_data.csv"),
          "correlation matrix" = read.csv("tv_hurricane_data.csv"),
          "scatter matrix" = read.csv("tv_hurricane_data.csv"),
          "four column distribution" = read.csv("tv_hurricane_data.csv"),
          read.csv("tv_hurricane_data.csv")
        )
      },
      "tv state" = {
        switch(
          plot_type,
          "histogram" = read.csv("tv_state_data.csv"),
          "correlation matrix" = read.csv("tv_state_data.csv"),
          "scatter matrix" = read.csv("tv_state_data.csv"),
          "four column distribution" = read.csv("tv_state_data.csv"),
          read.csv("tv_state_data.csv")
        )
      }
    )
  }
  loaded_data <- reactive({
    load_data(input$data_type, input$plot_type)
  })
  
  output$scatter_warning <- renderText({
    if (input$plot_type == "Scatter Matrix" &&
        (input$data_type == "tv hurricane by network" ||
         input$data_type == "tv hurricanes")) {
      "Scatter Plot cannot be plotted for this data"
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
