# Load the fivethirtyeight package
library(fivethirtyeight)

# Access the mediacloud_hurricanes dataset
tv_state_data <- fivethirtyeight::tv_states

# Render the dataset as a formatted table using knitr::kable()
formatted_table <- knitr::kable(tv_state_data)

# Print the formatted table
print(formatted_table)

# Write the formatted table to a CSV file
write.csv(
  formatted_table,
  "C:/Users/mamba/google-trends-R/R/csv/tv_state_data.csv",
  row.names = FALSE
)
