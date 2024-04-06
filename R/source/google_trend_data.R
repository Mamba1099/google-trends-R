# Load the fivethirtyeight package
library(fivethirtyeight)

# Access the mediacloud_hurricanes dataset
google_trend_data <- fivethirtyeight::google_trends

# Render the dataset as a formatted table using knitr::kable()
formatted_table <- knitr::kable(google_trend_data)

# Print the formatted table
print(formatted_table)

# Write the formatted table to a CSV file
write.csv(
  formatted_table,
  "C:/Users/mamba/google-trends-R/R/csv/google_trends_data.csv",
  row.names = FALSE
)
