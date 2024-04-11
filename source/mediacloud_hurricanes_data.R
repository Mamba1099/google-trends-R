# Load the fivethirtyeight package
library(fivethirtyeight)

# Access the mediacloud_hurricanes dataset
mediacloud_hurricane_data <- fivethirtyeight::mediacloud_hurricanes

# Write the formatted table to a CSV file
write.csv(
  mediacloud_hurricane_data,
  "C:/Users/mamba/google-trends-R/csv/mediacloud_hurricanes_data.csv",
  row.names = FALSE
)
