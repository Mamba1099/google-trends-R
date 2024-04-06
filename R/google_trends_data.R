# load the data set
library(fivethirtyeight)

# access the google trends data set
get_mediacloud_hurricanes_data <- fivethirtyeight::mediacloud_hurricanes

write.csv(
  get_mediacloud_hurricanes_data,
  "C:/Users/mamba/google-trends-R/R/csv/mediacloud_hurricanes_data.csv",
  row.names = FALSE
  )

