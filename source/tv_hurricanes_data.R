# Load the fivethirtyeight package
library(fivethirtyeight)

# Access the mediacloud_hurricanes dataset
tv_hurricane_data <- fivethirtyeight::tv_hurricanes

# Write the formatted table to a CSV file
write.csv(
  tv_hurricane_data,
  "C:/Users/mamba/google-trends-R/csv/tv_hurricane_data.csv",
  row.names = FALSE
)

