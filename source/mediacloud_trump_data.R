# Load the fivethirtyeight package
library(fivethirtyeight)

# Access the mediacloud_hurricanes dataset
mediacloud_trump_data <- fivethirtyeight::mediacloud_trump

# Write the formatted table to a CSV file
write.csv(
  mediacloud_trump_data,
  "C:/Users/mamba/google-trends-R/csv/mediacloud_trump_data.csv",
  row.names = FALSE
)
