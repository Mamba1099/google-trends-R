library(shiny)
library(ggplot2)
library(reshape2)

# Define UI
ui <- fluidPage(titlePanel("Google Trends Analysis"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      "data_type",
                      "Choose a data type:",
                      choices = c(
                        "google trends",
                        "mediacloud hurricanes",
                        "mediacloud state",
                        "mediacloud top online news",
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
                        "Scatter Matrix"
                      ),
                      selected = "Histogram / Bar Graph"
                    ),
                    conditionalPanel(
                      condition = "input.plot_type == 'Histogram / Bar Graph'",
                      sliderInput(
                        "bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 10
                      ),
                      sliderInput(
                        "fill_value",
                        "Select fill value:",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.1
                      )
                    )
                  ),
                  mainPanel(
                    plotOutput("plot", width = "100%", height = "800px"),
                    verbatimTextOutput("report")
                  )
                ))

# Define server logic
server <- function(input, output, session) {
  data <- reactive({
    req(input$data_type)
    file_name <- switch(
      input$data_type,
      "google trends" = "csv/google_trends_data.csv",
      "mediacloud hurricanes" = "csv/mediacloud_hurricanes_data.csv",
      "mediacloud state" = "csv/mediacloud_state_data.csv",
      "mediacloud top online news" = "csv/mediacloud_top_online_news_data.csv",
      "mediacloud trump" = "csv/mediacloud_trump_data.csv",
      "tv hurricane by network" = "csv/tv_hurricane_by_network_data.csv",
      "tv hurricanes" = "csv/tv_hurricane_data.csv",
      "tv state" = "csv/tv_state_data.csv"
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
      ggplot(melted_data, aes(x = date, y = value)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap( ~ variable, scales = "free") +
        theme(axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        ))
    } else if (input$plot_type == "Column Distribution") {
      melted_data <- melt(data)
      ggplot(melted_data, aes(value)) +
        geom_histogram(binwidth = input$bins) +
        facet_wrap( ~ variable, scales = "free")
    } else if (input$plot_type == "Correlation Matrix") {
      numeric_data <- subset(data, select = -c(date))
      corr_matrix <- cor(numeric_data)
      ggplot(melt(corr_matrix), aes(Var1, Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient(low = "purple4", high = "orange1") +
        theme_minimal()
    } else if (input$plot_type == "Scatter Matrix") {
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
      gridExtra::grid.arrange(grobs = plots, ncols = ncol(numeric_data))
    }
  })
  
  # Generate report based on selected data type and plot type
  output$report <- renderPrint({
    req(input$data_type)
    req(input$plot_type)
    report_text <- paste("Report for", input$data_type)
    if (input$data_type == "google trends") {
      report_text <-
        paste(report_text, "with plot type:", input$plot_type)
      if (input$plot_type == "Histogram / Bar Graph") {
        # Add content for Histogram / Bar Graph plot type
        report_text <-
          paste(report_text,
                "Google Trends Histogram / Bar Graph report content...")
      } else if (input$plot_type == "Column Distribution") {
        # Add content for Column Distribution plot type
        report_text <-
          paste(report_text,
                "Google Trends Column Distribution report content...")
      } else if (input$plot_type == "Correlation Matrix") {
        # Add content for Correlation Matrix plot type
        report_text <-
          paste(report_text,
                "Google Trends Correlation Matrix report content...")
      } else if (input$plot_type == "Scatter Matrix") {
        # Add content for Scatter Matrix plot type
        report_text <-
          paste(report_text,
                "Google Trends Scatter Matrix report content...")
      }
    } else if (input$data_type == "mediacloud hurricanes") {
      report_text <-
        paste(report_text, "with plot type:", input$plot_type)
      if (input$plot_type == "Histogram / Bar Graph") {
        # Add content for Histogram / Bar Graph plot type
        report_text <-
          paste(report_text,
                "Google Trends Histogram / Bar Graph report content...")
      } else if (input$plot_type == "Column Distribution") {
        # Add content for Column Distribution plot type
        report_text <-
          paste(report_text,
                "Google Trends Column Distribution report content...")
      } else if (input$plot_type == "Correlation Matrix") {
        # Add content for Correlation Matrix plot type
        report_text <-
          paste(report_text,
                "Google Trends Correlation Matrix report content...")
      } else if (input$plot_type == "Scatter Matrix") {
        # Add content for Scatter Matrix plot type
        report_text <-
          paste(report_text,
                "Google Trends Scatter Matrix report content...")
      }
      
      report_text <-
        paste(report_text, "Mediacloud Hurricanes report content...")
    } else if (input$data_type == "mediacloud state") {
      report_text <-
        paste(report_text, "with plot type:", input$plot_type)
      if (input$plot_type == "Histogram / Bar Graph") {
        # Add content for Histogram / Bar Graph plot type
        report_text <-
          paste(report_text,
                "Google Trends Histogram / Bar Graph report content...")
      } else if (input$plot_type == "Column Distribution") {
        # Add content for Column Distribution plot type
        report_text <-
          paste(report_text,
                "Google Trends Column Distribution report content...")
      } else if (input$plot_type == "Correlation Matrix") {
        # Add content for Correlation Matrix plot type
        report_text <-
          paste(report_text,
                "Google Trends Correlation Matrix report content...")
      } else if (input$plot_type == "Scatter Matrix") {
        # Add content for Scatter Matrix plot type
        report_text <-
          paste(report_text,
                "Google Trends Scatter Matrix report content...")
      }
      report_text <-
        paste(report_text, "Mediacloud State report content...")
    }
    return(report_text)
    # Add more conditions for other data types if needed
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
