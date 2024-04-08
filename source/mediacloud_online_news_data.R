
# Load the fivethirtyeight package
library(fivethirtyeight)

# Access the mediacloud_hurricanes dataset
mediacloud_online_news_data <- fivethirtyeight::mediacloud_online_news

# Write the formatted table to a CSV file
write.csv(
  mediacloud_online_news_data,
  "C:/Users/mamba/google-trends-R/csv/mediacloud_online_news_data.csv",
  row.names = FALSE
)
