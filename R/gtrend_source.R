# Load required packages
library(gtrendsR)

# Define global parameters
parameters <- list(
  technology = list(
    keywords = c("Android"),
    filename = "csv/technology_data.csv"
  ),
  entertainment = list(
    keywords = c("Netflix"),
    filename = "csv/entertainment_data.csv"
  ),
  finance = list(
    keywords = c("Bitcoin"),
    filename = "csv/finance_data.csv"
  ),
  geo = "US", # Geographic region
  gprop = c("web", "news", "images", "froogle", "youtube"),
  category = 0,
  hl = "en-US",
  low_search_volume = FALSE,
  cookie_url = "http://trends.google.com/Cookies/NID"
)

# Function to retrieve Google Trends data for a specific keyword list
retrieve_gtrends_data <- function(keyword_list, filename, time_range = "today+5-y") {
  Sys.sleep(10)  # Wait 10 seconds between requests to avoid rate limits
  gtrends_data <- gtrends(
    keyword = keyword_list,
    geo = parameters$geo,
    time = time_range,
    gprop = parameters$gprop,
    category = parameters$category,
    hl = parameters$hl,
    low_search_volume = parameters$low_search_volume,
    cookie_url = parameters$cookie_url
  )
  if (length(gtrends_data) > 0) {
    write.csv(
      gtrends_data$interest_over_time,
      file = filename,
      append = FALSE,
      row.names = FALSE
      )
    return(gtrends_data)
  } else {
    print("No valid data present")
    return(NULL)
  }
}

for (category_name in names(parameters)[1:3]) {
  category <- parameters[[category_name]]
  for (i in 1:length(category$keywords)) {
    keyword <- category$keywords[i]
    filename <- category$filename
    gtrends_data <- retrieve_gtrends_data(keyword, filename)
    if (is.null(gtrends_data)) {
      print(paste("No valid data present for", keyword))
    } else {
      print(paste("Google Trends data retrieved for", keyword))
      # Additional processing if needed
    }
  }
}

