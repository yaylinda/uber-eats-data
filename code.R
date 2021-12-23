library(ggplot2)
library(zoo)
library(lubridate)
library(reshape2)
library(scales)
library(ggthemes)
library(tidyr)

setwd("~/Developer/uber-eats-data")

############################################## 
########## READ DATA & ADD COLUMNS ########### 
##############################################

data = read.csv("processed_data_long.csv", na.strings = c("","NA"))

# Convert date column to date object
data$date = as.Date(data$date, "%Y-%m-%d")

# Add date helper columns
data$year = year(data$date)
data$week = week(data$date)
data$month = format(data$date,"%B")
data$month = factor(data$month, list(
  "January", "February", "March", 
  "April", "May", "June", 
  "July", "August", "September", 
  "October", "November", "December"
))
data$yearmonth = factor(as.yearmon(data$date))
data$day_of_week = factor(weekdays(data$date), list(
  "Sunday", 
  "Monday", 
  "Tuesday", 
  "Wednesday", 
  "Thursday", 
  "Friday", 
  "Saturday"
))
data$month_num = as.numeric(format(data$date,"%m"))
data$month_row = ifelse(
  data$month_num >= 1 & data$month_num <= 3, 1, 
  ifelse(
    data$month_num >= 4 & data$month_num <= 6, 2, 
    ifelse(
      data$month_num >= 7 & data$month_num <= 9, 3, 4)))

# Subset data by year to calculate monthweek for each year
data_2020 = subset(data, year(data$date) == 2020)
data_2020 = calculate_month_week(data_2020)

data_2021 = subset(data, year(data$date) == 2021)
data_2021 = calculate_month_week(data_2021)

# Then recombine the data
data_combined = rbind(data_2020, data_2021)

# Create a new data frame of restaurant counts (only restaurant_1)
data_combined$count_1 = ifelse(is.na(data_combined$restaurant_1), 0, 1)
restaurant_count = aggregate(
  data_combined$count_1, 
  by = list(
    restaurant = data_combined$restaurant_1
  ), 
  sum
)

# Order restaurant_count by number of times ordered
restaurant_count = restaurant_count[order(-restaurant_count$x), ]

# Restaurants with only 1 order will be labelled as "other"
other_restaurants = restaurant_count[which(restaurant_count$x == 1), 1]
labelled_restaurants = restaurant_count[which(restaurant_count$x > 1), 1]

# Add column for restaurant label on data_combined
data_combined$restaurant_1_label = ifelse(
  data_combined$restaurant_1 %in% other_restaurants, 
  "Other", 
  data_combined$restaurant_1
)

# Make the restaurant_1_label column a factor. 
# Restaurants in order of most ordered to least ordered
# "Other" is last
data_combined$restaurant_1_label_factor = factor(
  data_combined$restaurant_1_label, 
  c(labelled_restaurants, c("Other")))

# Create list of colors for each restaurant factor (in order)
restaurant_colors = c(
  '#ffd400', # halal guys
  '#c42925', # mcdonalds
  ''
)

############################################## 
################# PLOT DATA ################## 
##############################################

ggplot(
  data_combined, 
  aes(
    day_of_week, 
    monthweek, 
    fill = restaurant_1_label_factor
  )
) + 
  facet_grid(
    year ~ month,
    switch = "y", 
    space = "free"
  ) +
  coord_equal(ratio = 1) + 
  geom_tile(color = "white") +
  labs(
    x = "",
    y = "",
    title = "Uber Eats Orders",
    subtitle = "",
    fill = ""
  ) + 
  theme(
    text = element_text(family = "mono"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(),
    legend.position='bottom'
  )


############################################## 
############## HELPER FUNCTIONS ############## 
##############################################

# Calculate week of month, where sunday is a new week
calculate_month_week = function(data) {
  weekofmonth = rep(0, length(data$date))
  weekNum = 1
  currentMonth = 1
  for (i in (1:length(data$date))) {
    date = data$date[i]
    if (weekdays(date) == "Sunday") {
      weekNum = weekNum + 1
    }
    if (month(date) > currentMonth) {
      currentMonth = currentMonth + 1
      weekNum = 1
    }
    weekofmonth[i] = weekNum
  }
  data$monthweek = weekofmonth
  data$monthweek = factor(data$monthweek, list(6, 5, 4, 3, 2, 1))
  data
}
