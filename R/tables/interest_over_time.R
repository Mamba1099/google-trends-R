# Retrieve Google Trends data
source("gtrend_helper.R")

# Extract interest over time data
interest_over_time_data <- retrieve_gtrends_data$interest_over_time

# Convert to data frame
interest_over_time_table <- data.frame(Date = interest_over_time_data$time,
                                       Interest = interest_over_time_data$interest)
print(interest_over_time_table)

