# Load the fivethirtyeight package
library(fivethirtyeight)

# Access the mediacloud_hurricanes dataset
google_trend_data <- fivethirtyeight::google_trends

# Write the formatted table to a CSV file
write.csv(
  google_trend_data,
  "C:/Users/mamba/google-trends-R/csv/google_trends_data.csv",
  row.names = FALSE
)
