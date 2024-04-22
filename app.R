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
          max-height: 100vh;
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
        # Plot correlation matrix for numeric variables
        numeric_data <- subset(data, select = -c(date))
        corr_matrix <- cor(numeric_data)
        ggplot(melt(corr_matrix), aes(Var1, Var2, fill = value)) +
          geom_tile() +
          scale_fill_gradient(low = "purple4", high = "orange1") +
          theme_dark()
      
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
          - A strong positive correlation between Hurricane Irma and Hurricane Jose suggests that they often receive similar levels of media attention or are covered together in news reports.<br></br>
          - A weaker correlation between Hurricane Harvey and the other hurricanes might indicate independent or less synchronized media coverage patterns.<br></br>
          The correlation matrix helps identify relationships and patterns in media coverage, providing insights into how different hurricanes are portrayed in the media and their potential interconnections."
          )
        )
      } else if (input$plot_type == "Scatter Matrix") {
        div(HTML(
          "
          Bivariate Relationships: <br></br>
          The scatter matrix provides a visible illustration of the bivariate relationships between pairs of hurricanes. Each scatter plot inside the matrix corresponds to a unique combination of hurricanes. For instance, one scatter plot may constitute the connection between Hurricane Harvey and Hurricane Irma, whilst any other may constitute the connection between Hurricane Maria and Hurricane Jose.
          Strength of Relationship: <br></br>
          By examining the scatter plots, we can determine the electricity and course of the relationship between unique pairs of hurricanes in terms of media insurance. A sturdy nice correlation in a scatter plot indicates that the media coverage of 1 storm tends to growth or lower in tandem with the insurance of any other hurricane. Conversely, a weak correlation shows that there may be very little constant relationship between the media insurance of the 2 hurricanes.
          Patterns: <br></br>
          The scatter matrix allows us identify any consistent patterns or associations among specific hurricanes in phrases of media coverage. Clusters or patterns of points in certain scatter plots may additionally imply intervals of synchronized media coverage or lack thereof for sure pairs of hurricanes. For example, if we examine a good cluster of points alongside a diagonal line in a scatter plot, it shows that the media insurance of each hurricane in that pair has a tendency to boom or decrease collectively over the years.
          Outliers: <br></br>
          Outliers in the scatter plots represent times of severe or unexpected media insurance for certain pairs of hurricanes. These outliers might also suggest enormous occasions or traits associated with the hurricanes that captured media attention to a more quantity than ordinary. By figuring out outliers, we can similarly look at the factors contributing to the unusual media insurance and investigate their effect on public notion and reaction to the hurricanes.
          Temporal Trends: <br></br>
          Analyzing the scatter matrix over the years lets in us to perceive temporal developments inside the courting between hurricane occurrences and media coverage. Patterns in the scatter plots can also reveal seasonal versions, cyclical styles, or different traits in media insurance for distinct pairs of hurricanes. Understanding those temporal developments can offer valuable insights into how media coverage of hurricanes evolves through the years and how it could affect public awareness and response.
          "
          ))
      }
    } else if (input$data_type == "tv hurricanes") {
      if (input$plot_type == "Histogram / Bar Graph") {
        div(
          HTML(
            "
            HISTOGRAM: <br></br>
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
            "
            COLUMN DISTRIBUTION: <br></br>
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
            "
            CORRELATION MATRIX: <br></br>
            The correlation matrix quantifies the connection between pairs of hurricane-associated TV coverage. <br></br>
            By analyzing the correlation coefficients, we can decide the electricity and path of the connection among different hurricanes in phrases of media insurance. <br></br>
            Strength of Relationship: <br></br>
            Strong tremendous correlations between sure typhoon pairs indicate that they often get hold of similar ranges of TV coverage or are covered together in information reports. <br></br>
            Patterns: <br></br>
            Identifying steady patterns or institutions among specific hurricanes and their TV coverage can offer insights into media reporting dynamics. <br></br>
            Implications: <br></br>
            Understanding how the TV coverage of one typhoon might also influence or coincide with the coverage of some other can shed light on media narratives and public perceptions of typhoon occasions.<br></br>
            "
          )
        )
      } else if (input$plot_type == "Scatter Matrix") {
        div(HTML("No data for this data type"))
      }
    } else if (input$data_type == "mediacloud state") {
      if (input$plot_type == "Histogram / Bar Graph") {
        div(
          HTML(
            "
            HISTOGRAM: <br>
            Frequency Distribution:
            - The y-axis represents the frequency of media insurance, at the same time as the x-axis suggests the variety of dates. <br></br>
            Skewness: <br>
            - We can discover any skewness in the distribution, which would possibly suggest durations of heightened media interest to hurricanes. <br>
            - Using facet allows us to parent differences in the frequency and depth of media insurance throughout exceptional time durations by comparing the histogram. <br>
            - We study various levels of media insurance for hurricanes in Texas, Puerto Rico, and Florida over the years from the histogram. <br>
            - Hurricane Harvey in Texas obtained tremendous media interest round late August to early September indicated through spikes in insurance. <br>
            - In evaluation, Hurricane Maria in Puerto Rico had especially low media coverage typical, with occasional spikes but no sustained increase. <br>
            - Hurricane Irma in Florida had a greater consistent sample with peaks in mid-September, suggesting sustained media interest throughout that duration. <br>
            "
          )
        )
      } else if (input$plot_type == "Column Distribution") {
        div(
          HTML(
            "
            COLUMN DISTRIBUTION: <br></br>
            Temporal Patterns:  <br></br>
            The column distribution plots monitor temporal developments in media insurance, with significant spikes similar to principal activities or durations of heightened media interest.  <br></br>
            Outliers:  <br></br>
            Anomalies or outliers inside the facts can also indicate big occasions or reporting discrepancies.  <br></br>
            Long-term Trends:  <br></br>
            Assessing long-term developments in media insurance presents insights into how media attention to hurricanes evolves through the years.  <br></br>
            By inspecting the distribution of media coverage for each storm kind over the years, we can identify temporal styles and outliers in media coverage.  <br></br>
            Significant spikes in coverage correspond to major activities or periods of heightened media attention, which includes hurricanes making landfall or full-size trends inside the aftermath.
            "
          )
        )
      } else if (input$plot_type == "Correlation Matrix") {
        div(
          HTML(
            "
            CORRELATION MATRIX: <br></br>
            Strength of Relationship:  <br></br>
            The correlation matrix quantifies the power and path of the relationship between distinct hurricanes in phrases of media coverage.  <br></br>
            Patterns:  <br></br>
            Identifying consistent patterns or institutions among unique hurricanes and their media interest presents insights into media reporting dynamics.  <br></br>
            Implications:  <br></br>
            Understanding how the media coverage of one storm may have an impact on or coincide with the insurance of every other sheds mild on media narratives and public perceptions of hurricane occasions.  <br></br>
            The correlation matrix allows perceive relationships and patterns in media insurance, supplying insights into how one-of-a-kind hurricanes are portrayed within the media and their ability interconnections.  <br></br>
            Strong positive correlations between positive storm pairs indicate they often get hold of comparable degrees of media coverage or are protected collectively in news reviews.
            "
          )
        )
      } else if (input$plot_type == "Scatter Matrix") {
        div(
          HTML(
            "
            SCATTER MATRIX: <br></br>
            Bivariate Relationships: <br></br>
            The scatter matrix lets in us to assess the bivariate relationships among media coverage of various hurricanes. <br></br>
            Trends: <br></br>
            Identifying tendencies or patterns inside the courting between media insurance of hurricanes over time enables understand media dynamics. <br></br>
            Outliers: <br></br>
            Detecting outliers or unusual occurrences in media coverage may additionally warrant in addition research. <br></br>
            Example Analysis: <br></br>
            Scatter plots between one-of-a-kind hurricanes display clusters of points indicating periods of synchronized media coverage or lack thereof. <br></br>
            Examining the diagonal of the scatter matrix permits visualization of the distribution of media insurance for every hurricane in my view, identifying any outliers or severe values.
            "
          )
        )
      }
    } else if (input$data_type == "mediacloud trump") {
      if (input$plot_type == "Histogram / Bar Graph") {
        div(
          HTML(
            "
              HISTOGRAM: <br></br>
              The histogram provides an in depth look at the frequency distribution of mentions of hurricanes and mentions associated with President Trump in Puerto Rico, Florida, and Texas over the discovered period. <br></br>
              Puerto Rico: <br></br>
              The histogram for Puerto Rico indicates sporadic mentions of hurricanes, with occasional spikes indicating periods of improved interest. Mentions related to President Trump are minimum throughout the period. <br></br>
              Florida: <br></br>
              Florida famous a extra regular pattern of hurricane mentions, with peaks indicating substantial activities or periods of heightened media coverage. Mentions associated with President Trump are also determined, albeit much less frequent in comparison to mentions of hurricanes. <br></br>
              Texas: <br></br>
              Texas suggests a similar sample to Florida, with great spikes in hurricane mentions corresponding to precise activities. Mentions related to President Trump are quite low for the duration of the observed duration.
            "
          )
        )
      } else if (input$plot_type == "Column Distribution") {
        div(
          HTML(
            "
              COLUMN DISTRIBUTION: <br></br>
              The column distribution plot gives insights into the distribution of mentions of hurricanes and mentions associated with President Trump throughout unique dates in every place. <br></br>
              Puerto Rico: The column distribution plot for Puerto Rico famous temporal styles in media coverage, with widespread spikes corresponding to principal events or durations of heightened media interest. <br></br>
              Mentions related to President Trump are rare. <br></br>
              Florida: <br></br>
              In Florida, the column distribution plot suggests a consistent pattern of media coverage for each hurricanes and mentions related to President Trump. Peaks in coverage coincide with substantial occasions or tendencies. <br></br>
              Texas: <br></br>
              Similarly, the column distribution plot for Texas illustrates temporal tendencies in media coverage, with notable spikes in coverage for hurricanes and occasional mentions related to President Trump.
            "
          )
        )
      } else if (input$plot_type == "Correlation Matrix") {
        div(
          HTML(
            "CORRELATION MATRIX: <br></br>
              The correlation matrix, quantifies the connection between mentions of hurricanes and mentions related to President Trump in each region. <br></br>
              Puerto Rico: <br></br>
              There is a weak effective correlation among mentions of hurricanes and mentions related to President Trump in Puerto Rico, indicating a moderate tendency for each kinds of mentions to arise together. <br></br>
              Florida: <br></br>
              In Florida, there is a slight wonderful correlation between mentions of hurricanes and mentions associated with President Trump, suggesting a more potent courting between these variables in comparison to Puerto Rico. <br></br>
              Texas: <br></br>
              Texas indicates a similar slight high-quality correlation among mentions of hurricanes and mentions related to President Trump, indicating a constant relationship among these variables within the region."
          )
        )
      } else if (input$plot_type == "Scatter Matrix") {
        div(
          HTML(
            "
            SCATTER MATRIX: <br></br>
              The scatter matrix provides a visual representation of the bivariate relationships between mentions of hurricanes and mentions related to President Trump in each location. <br></br>

              Puerto Rico: <br></br>
              Scatter plots between mentions of hurricanes and mentions related to President Trump in Puerto Rico show scattered points, indicating a lack of strong linear relationship between the variables. <br></br>
              Florida: <br></br>
              Scatter plots for Florida reveal a more pronounced positive linear relationship between mentions of hurricanes and mentions related to President Trump, suggesting a stronger association between these variables compared to Puerto Rico. <br></br>
              Texas: <br></br>
              Similarly, scatter plots for Texas also show a positive linear relationship between mentions of hurricanes and mentions related to President Trump, indicating a consistent association between these variables in the region.
            "
          )
        )
      }
    } else if (input$data_type == "tv state") {
      if (input$plot_type == "Histogram / Bar Graph") {
        div(
          HTML(
            "
            HISTOGRAM: <br></br>
            The histogram provides insights into the distribution of mentions of hurricanes throughout Florida, Texas, and Puerto Rico over the observed length. <br></br>

            Florida: <br></br>
            The histogram for Florida suggests a extensive range of mentions of hurricanes, with peaks indicating durations of improved interest or media insurance. <br></br>
            Texas: <br></br>
            Texas exhibits a similar pattern to Florida, with various levels of typhoon mentions in the course of the determined length. <br></br>
            Puerto Rico: <br></br>
            Puerto Rico suggests incredibly low ranges of typhoon mentions as compared to Florida and Texas, with occasional spikes indicating periods of heightened interest.
            "
          )
        )
      } else if (input$plot_type == "Column Distribution") {
        div(
          HTML(
            "
            COLUMN DISTRIBUTION: <br></br>
            The column distribution plot gives a visualization of the distribution of storm mentions across one-of-a-kind dates for each place. <br></br>

            Florida: <br></br>
            In Florida, the column distribution plot exhibits temporal patterns in storm mentions, with terrific spikes similar to enormous activities or intervals of heightened media coverage. <br></br>
            Texas: <br></br>
            Texas shows a comparable pattern to Florida, with peaks in storm mentions coinciding with precise dates. <br></br>
            Puerto Rico: <br></br>
            Puerto Rico exhibits fewer typhoon mentions in comparison to Florida and Texas, with a exceedingly flat distribution across dates.
            "
          )
        )
      } else if (input$plot_type == "Correlation Matrix") {
        div(
          HTML(
            "
            CORRELATION MATRIX: <br></br>
            The correlation matrix quantifies the relationship between hurricane mentions across Florida, Texas, and Puerto Rico. <br></br>

            Correlation between Florida and Texas: <br></br>
            There is a moderate positive correlation between hurricane mentions in Florida and Texas, indicating a consistent relationship between these variables across the two locations. <br></br>
            Correlation between Florida and Puerto Rico: <br></br>
            The correlation between hurricane mentions in Florida and Puerto Rico is weak, suggesting a limited relationship between these variables in the two regions. <br></br>
            Correlation between Texas and Puerto Rico: <br></br>
            Similarly, the correlation between hurricane mentions in Texas and Puerto Rico is weak, indicating a relatively independent pattern of hurricane mentions in the two locations.
            "
          )
        )
      } else if (input$plot_type == "Scatter Matrix") {
        div(
          HTML(
            "
            SCATTER MATRIX: <br></br>
            The scatter matrix offers a visible illustration of the bivariate relationships between hurricane mentions across Florida, Texas, and Puerto Rico. <br></br>

            Florida vs. Texas: <br></br>
            Scatter plots between hurricane mentions in Florida and Texas show a superb linear courting, indicating a consistent pattern of typhoon mentions throughout the 2 locations. <br></br>
            Florida vs. Puerto Rico: <br></br>
            Scatter plots among typhoon mentions in Florida and Puerto Rico exhibit scattered points, suggesting a weak relationship among hurricane mentions within the  areas. <br></br>
            Texas vs. Puerto Rico: <br></br>
            Similarly, scatter plots between typhoon mentions in Texas and Puerto Rico additionally display scattered factors, indicating a confined courting between typhoon mentions inside the two locations.
            "
          )
        )
      }
    }
  })}

shinyApp(ui = ui, server = server)