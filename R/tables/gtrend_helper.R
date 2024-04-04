# Load required packages
library(gtrendsR)

# Define global parameters
keywords <- c(
  "NHL",   # National Hockey League in North America
  "NBA",   # National Basketball Association in North America
  "MLB",   # Major League Baseball in North America
  "MLS"    # Major League Soccer in North America
)
geo <- "US"                   # Geographic region
gprop <- c("web", "news", "images", "froogle", "youtube")
category <- 0
hl <- "en-US"
compared_breakdown <- TRUE
low_search_volume <- FALSE
cookie_url <- "http://trends.google.com/Cookies/NID"
onlyInterest <- TRUE

# Function to retrieve Google Trends data for a specific time range
retrieve_gtrends_data <- function(keyword, time_range) {
  tryCatch({
    gtrends_data <- gtrends(
      keyword = keyword,
      geo = geo,
      time = time_range,
      gprop = gprop,
      category = category,
      hl = hl,
      compared_breakdown = compared_breakdown,
      low_search_volume = low_search_volume,
      cookie_url = cookie_url,
      onlyInterest = onlyInterest
    )
    if (length(gtrends_data) > 0) {
      return(gtrends_data)
    } else {
      print("No valid data present")
      return(NULL)
    }
  }, error = function(e) {
    print("Error occurred during gtrends() function call:")
    print(e)
    return(NULL)
  })
}

# Function to load and process Google Trends data
load_and_process_gtrends_data <- function(gtrends_data) {
  if (!is.null(gtrends_data)) {
    source("interest_over_time.R")
  }
}

# Retrieve and process Google Trends data for different time periods
retrieve_gtrends_time_data <- function() {
  # Define time ranges
  time_ranges <- c("now 1-H", "now 4-H", "now 7-d", "today 1-m", "today 12-m")
  
  # Loop through each time range
  for (time_range in time_ranges) {
    print(paste("Retrieving data for time range:", time_range))
    gtrends_data <- retrieve_gtrends_data(keywords, time_range)
    load_and_process_gtrends_data(gtrends_data)
  }
}

# Execute the function to retrieve and process Google Trends data
retrieve_gtrends_time_data()
