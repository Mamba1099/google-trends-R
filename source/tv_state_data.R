# Load the fivethirtyeight package
library(fivethirtyeight)

# Access the mediacloud_hurricanes dataset
tv_state_data <- fivethirtyeight::tv_states

# Write the formatted table to a CSV file
write.csv(
  tv_state_data,
  "C:/Users/mamba/google-trends-R/csv/tv_state_data.csv",
  row.names = FALSE
)
