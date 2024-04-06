# Load the fivethirtyeight package
library(fivethirtyeight)

# Access the mediacloud_hurricanes dataset
mediacloud_top_online_news_data <- fivethirtyeight::mediacloud_online_news

# Render the dataset as a formatted table using knitr::kable()
formatted_table <- knitr::kable(mediacloud_top_online_news_data)

# Print the formatted table
print(formatted_table)

# Write the formatted table to a CSV file
write.csv(
  formatted_table,
  "C:/Users/mamba/google-trends-R/R/csv/mediacloud_top_online_news_data.csv",
  row.names = FALSE
)
