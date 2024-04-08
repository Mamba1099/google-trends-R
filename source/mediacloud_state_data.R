# Load the fivethirtyeight package
library(fivethirtyeight)

# Access the mediacloud_hurricanes dataset
mediacloud_state_data <- fivethirtyeight::mediacloud_states

# Write the formatted table to a CSV file
write.csv(
  mediacloud_state_data,
  "C:/Users/mamba/google-trends-R/csv/mediacloud_state_data.csv",
  row.names = FALSE
)
