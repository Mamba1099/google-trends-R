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
      sliderInput(
        "bin_width",
        "Select Bin Width",
        min = 1,
        max = 100,
        value = 10
      ),
      sliderInput(
        "num_bins",
        "Number of bins",
        min = 1,
        max = 50,
        value = 20
      )
    ),
    mainPanel(
      div(
        plotOutput("plot", width = "100%", height = "850px"),
        tags$style(type = "text/css", "#plot {margin-bottom: 50px}")
      ),
      div(
        style = "font-size: 30px; animation: rainbow-glow 2s infinite; margin-top: -50px;",
        textOutput("scatter_warning"),
      ),
      div(
        textOutput("report_section"),
        style = "
          margin-top: 30px;
          border: 2px solid black;
          width: 610px;
          padding: 10px;
          height: 480px;
          border-radius: 6px;
          margin-left: -640px;
          margin-top: -500px;
        "
      )
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
# Define server logic
server <- function(input, output, session) {
  observe({
    updateSliderInput(session, "bin_width", value = input$bin_width)
    updateSliderInput(session, "num_bins", value = input$num_bins)
  })
  
  data <- reactive({
    req(input$data_type)
    file_name <- switch(
      input$data_type,
      "google trends" = "csv/google_trends_data.csv",
      "mediacloud hurricanes" = "csv/mediacloud_hurricanes_data.csv",
      "mediacloud state" = "csv/mediacloud_state_data.csv",
      "mediacloud trump" = "csv/mediacloud_trump_data.csv",
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
      p <-
        ggplot(melted_data, aes(
          x = date,
          y = value,
          fill = as.factor(variable)
        )) +
        geom_bar(width = input$bin_width, stat = "identity", ) +
        facet_wrap( ~ variable, scales = "free") +
        theme(axis.text.x = element_text(
          angle = 50,
          vjust = 0.5,
          hjust = 0.5
        )) +
        labs(fill = "Variable")
      
      print(p)
    } else if (input$plot_type == "Column Distribution") {
      melted_data <- melt(data)
      ggplot(melted_data, aes(value)) +
        geom_histogram(binwidth = input$bin_width) +
        facet_wrap( ~ variable, scales = "free")
    } else if (input$plot_type == "Correlation Matrix") {
      if (input$data_type == "tv hurricane by network") {
        return(NULL)
      } else {
        # Plot correlation matrix for numeric variables
        numeric_data <- subset(data, select = -c(date))
        corr_matrix <- cor(numeric_data)
        ggplot(melt(corr_matrix), aes(Var1, Var2, fill = value)) +
          geom_tile() +
          scale_fill_gradient(low = "purple4", high = "orange1") +
          theme_dark()
      }
      
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
  
  output$scatter_warning <- renderText({
    if (input$plot_type == "Scatter Matrix" &&
        (input$data_type == "tv hurricanes")) {
      "Scatter Matrix cannot be plotted for this data"
    }
  })
  output$report_section <- renderText({
    req(input$data_type, input$plot_type)
    
    report <- ""
    
    if (input$data_type == "google trends") {
      if (input$plot_type == "Histogram / Bar Graph") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Histogram / Bar Graph.")
      } else if (input$plot_type == "Column Distribution") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Column Distribution.")
      } else if (input$plot_type == "Correlation Matrix") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Correlation Matrix.")
      } else if (input$plot_type == "Scatter Matrix") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Scatter Matrix.")
      }
    }
    if (input$data_type == "mediacloud hurricanes") {
      if (input$plot_type == "Histogram / Bar Graph") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Histogram / Bar Graph.")
      } else if (input$plot_type == "Column Distribution") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Column Distribution.")
      } else if (input$plot_type == "Correlation Matrix") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Correlation Matrix.")
      } else if (input$plot_type == "Scatter Matrix") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Scatter Matrix.")
      }
    }
    if (input$data_type == "mediacloud state") {
      if (input$plot_type == "Histogram / Bar Graph") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Histogram / Bar Graph.")
      } else if (input$plot_type == "Column Distribution") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Column Distribution.")
      } else if (input$plot_type == "Correlation Matrix") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Correlation Matrix.")
      } else if (input$plot_type == "Scatter Matrix") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Scatter Matrix.")
      }
    }
    if (input$data_type == "mediacloud trump") {
      if (input$plot_type == "Histogram / Bar Graph") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Histogram / Bar Graph.
                ",
                )
      } else if (input$plot_type == "Column Distribution") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Column Distribution.")
      } else if (input$plot_type == "Correlation Matrix") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Correlation Matrix.")
      } else if (input$plot_type == "Scatter Matrix") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Scatter Matrix.")
      }
    }
    if (input$data_type == "tv hurricanes") {
      if (input$plot_type == "Histogram / Bar Graph") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Histogram / Bar Graph.")
      } else if (input$plot_type == "Column Distribution") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Column Distribution.")
      } else if (input$plot_type == "Correlation Matrix") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Correlation Matrix.")
      } else if (input$plot_type == "Scatter Matrix") {
        report <-
          paste("There is no report for",
                input$data_type,
                "with Scatter Matrix since it can't be plotted.")
      }
    }
    if (input$data_type == "tv state") {
      if (input$plot_type == "Histogram / Bar Graph") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Histogram / Bar Graph.")
      } else if (input$plot_type == "Column Distribution") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Column Distribution.")
      } else if (input$plot_type == "Correlation Matrix") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Correlation Matrix.")
      } else if (input$plot_type == "Scatter Matrix") {
        report <-
          paste("This is the report for",
                input$data_type,
                "with Scatter Matrix.")
      }
    }
    
    return(report)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
