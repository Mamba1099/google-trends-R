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
        value = 1
      ),
      sliderInput(
        "num_bins",
        "Number of bins",
        min = 1,
        max = 50,
        value = 4
      )
    ),
    mainPanel(
      div(
        plotOutput("plot", width = "100%", height = "850px"),
        tags$style(type = "text/css", "#plot {margin-bottom: 50px}")
      ),
      div(style = "font-size: 30px; animation: rainbow-glow 2s infinite; margin-top: -50px;",
          textOutput("scatter_warning")),
      div(
        uiOutput("report_section"),
        style = "
          margin-top: 30px;
          border: 2px solid black;
          width: 610px;
          padding: 10px;
          overflow-y: auto;
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
        geom_bar(width = input$bin_width, stat = "identity") +
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
        col_index <- ((i - 1) %/% ncol(numeric_data)) + 1
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
  output$report_section <- renderUI({
    if (input$data_type == "google trends") {
      if (input$plot_type == "Histogram / Bar Graph") {
        div(
          HTML(
            "
          Histograms permit us to higher take a look at the distribution of typhoon events over the years. Examining the histograms for each hurricane.-** <br></br>
            Frequency distribution ** :The y - axis represents the frequency of occasions, at the same time as the x - axis shows non-stop days. <br></br>
            - Skewed :We can see any skewness inside the distribution, that can imply a period of accelerated storm interest. <br></br>
            -** Comparison ** :A facet - with the aid of - facet contrast of histograms allows us to peer differences in the frequency and severity of storms in exceptional seasons. <br></br>
          For instance,<br></br>
          -Hurricane Harvey:The histogram indicates discrete activities with a few spikes indicating a duration of expanded hobby, inclusive of round past due August to early September <br></br>
          - Hurricane Irma:Shows a constant pattern peaking in mid - September,suggesting persevered interest over that length. <br></br>
          - Hurricane Maria: Shows fewer events average, with occasional peaks in past due September. <br></br>
          - Hurricane Jose:Like Maria, it hasfewer occasions in comparison to Harvey and Irma,peaking in September. <br></br>"
          )
        )
      } else if (input$plot_type == "Column Distribution") {
        div(
          HTML(
            "COLUMN DISTRIBUTION: <br></br>
          Column distribution plots offer insights into the distribution of hurricane occurrences across different dates. By examining the distribution of occurrences for each hurricane type over time, we can: <br></br>
          - **Temporal Patterns**: Identify temporal trends and patterns in hurricane occurrences, such as seasonal variations or specific event-related spikes. <br></br>
          - **Outliers**: Detect any outliers or anomalies in the data that might indicate exceptional weather events or reporting discrepancies. <br></br>
          - **Long-term Trends**: Assess long-term trends in hurricane activity, which can inform predictive models and disaster preparedness efforts. <br></br>
          For instance: <br></br>
          - We might observe a clustering of high occurrences around certain dates, indicating the impact of specific weather events or seasonal trends. <br></br>
          - Comparing the distributions across different hurricanes allows us to identify similarities or differences in their occurrence patterns. <br></br>"
          )
        )
      } else if (input$plot_type == "Correlation Matrix") {
        div(
          HTML(
            "CORRELATION MATRIX ANALYSIS: <br></br>
          The correlation matrix quantifies the relationship between pairs of hurricane occurrences. By analyzing the correlation coefficients, we can: <br></br>
          - **Strength of Relationship**: Determine the strength and direction of the relationship between different hurricane types. <br></br>
          - **Patterns**: Identify any consistent patterns or associations between specific hurricanes. <br></br>
          - **Implications**: Understand how the occurrence of one hurricane type may influence or coincide with the occurrence of another. <br></br>
          For example: <br></br>
          - A strong positive correlation between Hurricane Irma and Hurricane Jose suggests that they often occur together or in close temporal proximity. <br></br>
          - A weaker correlation between Hurricane Harvey and the other hurricanes might indicate independent or less synchronized occurrence patterns. <br></br>"
          )
        )
      } else if (input$plot_type == "Scatter Matrix") {
        div(
          HTML(
            "SCATTER MATRIX ANALYSIS: <br></br>
          The scatter matrix provides a visual representation of the relationship between pairs of hurricane occurrences. By examining the scatter plots, we can: <br></br>
          - **Bivariate Relationships**: Assess the bivariate relationships between different hurricane types. <br></br>
          - **Trends**: Identify any trends or patterns in the relationship between hurricane occurrences over time. <br></br>
          - **Outliers**: Detect any outliers or unusual occurrences that may warrant further investigation. <br></br>
          For instance: <br></br>
          - Scatter plots between Hurricane Irma and Hurricane Maria might reveal clusters of points indicating periods of synchronized activity or lack thereof. <br></br>
          - Examining the diagonal of the scatter matrix allows us to visualize the distribution of each hurricane occurrence individually and identify any outliers or extreme values. <br></br>
          By integrating these additional insights into the analysis, we can gain a more comprehensive understanding of the dynamics of hurricane occurrences, their interrelationships, and their implications for disaster management and resilience planning. <br></br>"
          )
        )
      }
    } else if (input$data_type == "mediacloud hurricanes") {
      if (input$plot_type == "Histogram / Bar Graph") {
        div(
          HTML(
            "HISTOGRAM: <br></br>
          Histograms offer a closer look at the distribution of hurricane-related media coverage over time. By examining the histograms for each hurricane type, we can observe:<br></br>
          - **Frequency Distribution**: The y-axis represents the frequency of media coverage, while the x-axis indicates the range of dates.<br>
          - **Skewness**: We can identify any skewness in the distribution, which might indicate periods of heightened media attention to hurricanes.<br>
          - **Comparison**: Comparing the histograms side by side allows us to discern differences in the frequency and intensity of media coverage across different time periods.<br></br>
          For example:<br></br>
          - Hurricane Harvey: The histogram shows sporadic media coverage with a few spikes indicating periods of increased attention, such as around late August to early September.<br>
          - Hurricane Irma: Exhibits a more consistent pattern with peaks in mid-September, suggesting sustained media attention during that period.<br>
          - Hurricane Maria: Shows relatively low media coverage overall, with occasional spikes in late September.<br>
          - Hurricane Jose: Similar to Maria, with fewer media coverage compared to Harvey and Irma, and peaks in September.<br></br>
          The histogram provides insights into the temporal distribution of media coverage, highlighting periods of heightened attention and comparing the intensity of coverage across different hurricanes."
          )
        )
      } else if (input$plot_type == "Column Distribution") {
        div(
          HTML(
            "COLUMN DISTRIBUTION: <br></br>
          Column distribution plots offer insights into the distribution of hurricane-related media coverage across different dates. By examining the distribution of media coverage for each hurricane type over time, we can:<br></br>
          - **Temporal Patterns**: Identify temporal trends and patterns in media coverage of hurricanes, such as seasonal variations or specific event-related spikes.<br>
          - **Outliers**: Detect any outliers or anomalies in the data that might indicate exceptional media attention or reporting discrepancies.<br>
          - **Long-term Trends**: Assess long-term trends in media coverage of hurricanes, which can inform communication strategies and public awareness efforts.<br></br>
          For instance:<br></br>
          - We might observe a clustering of high media coverage around certain dates, indicating the impact of specific weather events or seasonal trends.<br>
          - Comparing the distributions across different hurricanes allows us to identify similarities or differences in their media coverage patterns.<br></br>
          The column distribution plot provides a comprehensive view of the temporal distribution of media coverage, revealing trends, outliers, and long-term patterns."
          )
        )
      } else if (input$plot_type == "Correlation Matrix") {
        div(
          HTML(
            "CORRELATION MATRIX: <br></br>
          The correlation matrix quantifies the relationship between pairs of hurricane-related media coverage. By analyzing the correlation coefficients, we can:<br></br>
          - **Strength of Relationship**: Determine the strength and direction of the relationship between different hurricanes in terms of media coverage.<br>
          - **Patterns**: Identify any consistent patterns or associations between specific hurricanes and their media attention.<br>
          - **Implications**: Understand how the media coverage of one hurricane may influence or coincide with the coverage of another.<br></br>
          For example:<br></br>
          - A strong positive correlation between Hurricane Irma and Hurricane Jose suggests that they often receive similar levels of media attention or are covered together in news reports.<br>
          - A weaker correlation between Hurricane Harvey and the other hurricanes might indicate independent or less synchronized media coverage patterns.<br></br>
          The correlation matrix helps identify relationships and patterns in media coverage, providing insights into how different hurricanes are portrayed in the media and their potential interconnections."
          )
        )
      } else if (input$plot_type == "Scatter Matrix") {
        div(HTML("No data for this data type"))
      }
    } else if (input$data_type == "tv hurricanes") {
      if (input$plot_type == "Histogram / Bar Graph") {
        div(
          HTML(
            "HISTOGRAM: <br></br>
          Harvey: <br></br>
          Histogram for Hurricane Harvey suggests a sluggish boom in TV coverage starting from late August 2017, attaining its top round overdue August to early September.
          This spike in coverage possibly corresponds to the peak depth of the hurricane. <br></br>
          After the peak, TV coverage gradually decreases but remains considerable till mid-September before declining in addition. <br></br>
          Irma: <br></br>
          Hurricane Irma's TV insurance follows a comparable sample to Harvey, with a terrific boom in coverage starting from overdue August to early September 2017. <br></br>
          Unlike Harvey, Irma's coverage continues a exceedingly excessive level at some stage in mid-September, indicating sustained media interest. <br></br>
          Maria: <br></br>
          The histogram for Hurricane Maria indicates a one-of-a-kind trend in comparison to Harvey and Irma. TV insurance for Maria stays fantastically low in the course of the observed duration, with occasional spikes but no sustained boom in insurance. <br></br>
          This decrease stage of insurance indicates that Maria might not have acquired as a good deal media interest as Harvey and Irma for the duration of the same timeframe. <br></br>
          Jose: <br></br>
          Hurricane Jose's TV insurance is minimal compared to the alternative hurricanes, with sporadic spikes indicating quick periods of multiplied insurance. <br></br>
           overall insurance for Jose remains low in the course of the found length, indicating that it did now not entice as a whole lot media attention as Harvey and Irma."
          )
        )
      } else if (input$plot_type == "Column Distribution") {
        div(
          HTML(
            "COLUMN DISTRIBUTION: <br></br>
          Column distribution plots provide insights into the distribution of typhoon-related TV coverage across different dates. <br></br>
          By inspecting the distribution of TV coverage for each typhoon kind over the years, we can become aware of temporal developments and patterns in media coverage. <br></br>
          Temporal Patterns: <br></br>
          The column distribution plots reveal temporal trends in TV coverage, with awesome spikes corresponding to full-size occasions or periods of heightened media attention. <br></br>
          Outliers: <br></br>
          Anomalies or outliers within the statistics may suggest notable occasions or reporting discrepancies that warrant in addition investigation. <br></br>
          Long-term Trends: <br></br>
          Assessing lengthy-term tendencies in TV coverage can provide precious insights into how media attention to hurricanes evolves over time."
          )
        )
      } else if (input$plot_type == "Correlation Matrix") {
        div(
          HTML(
            "CORRELATION MATRIX: <br></br>
          The correlation matrix quantifies the connection between pairs of hurricane-associated TV coverage. <br></br>
          By analyzing the correlation coefficients, we can decide the electricity and path of the connection among different hurricanes in phrases of media insurance. <br></br>
          Strength of Relationship: <br></br>
          Strong tremendous correlations between sure typhoon pairs indicate that they often get hold of similar ranges of TV coverage or are covered together in information reports. <br></br>
          Patterns: <br></br>
          Identifying steady patterns or institutions among specific hurricanes and their TV coverage can offer insights into media reporting dynamics. <br></br>
          Implications: <br></br>
          Understanding how the TV coverage of one typhoon might also influence or coincide with the coverage of some other can shed light on media narratives and public perceptions of typhoon occasions."
          )
        )
      } else if (input$plot_type == "Scatter Matrix") {
        div(HTML("No data for this data type"))
      }
    }
  })
}
shinyApp(ui = ui, server = server)