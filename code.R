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

############################################## 
################# PLOT DATA ################## 
##############################################

ggplot(
  data_combined, 
  aes(
    day_of_week, 
    monthweek, 
    fill = as.factor(restaurant_1)
  )
) + 
facet_grid(
  year ~ month,
  switch = "y", 
  space = "free"
) +
coord_equal(ratio = 1) + 
geom_tile(color = "white")
  





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

variable_labels = c(
  `Goldenpot` = 'Goldenpot',
  `Firehouse.Subs` = 'Firehouse Subs',
  `Taco.Bell` = 'Taco Bell',
  `North.Italia` = 'North Italia',
  `Chick.fil.A` = 'Chick-fil-a',
  `The.Halal.Guys` = 'The Halal Guys',
  `Panera` = 'Panera',
  `Jack.in.the.Box` = 'Jack in the Box',
  `KFC` = 'KFC',
  `McDonald.s` = 'McDonald\'s',
  
  `January` = "Jan", 
  `February` = "Feb", 
  `March` = "Mar", 
  `April` = "Apr", 
  `May` = "May", 
  `June` = "Jun", 
  `July` = "Jul", 
  `August` = "Aug", 
  `September` = "Sep", 
  `October` = "Oct", 
  `November` = "Nov", 
  `December` = "Dec"
)



