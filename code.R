library(ggplot2)
library(zoo)
library(lubridate)
library(reshape2)
library(scales)
library(ggthemes)
library(tidyr)

setwd("~/Developer/uber-eats-data")

################################# 
########### FUNCTIONS ########### 
################################# 

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
  
}

# Fill in days with missing data
fill_missing_days = function(dataset, year) {
  starting_date = as.Date(paste(year, "01", "01", sep="-"))
  
  while(year(starting_date) == year) {
    if (dim(dataset[dataset$date == starting_date, ])[1] == 0) {
      dataset = rbind(dataset, data.frame(
        date = as.Date(starting_date),
        order_id = "",
        Restaurant.ID = "",
        restaurant_name = "",
        order_time = "",
        x = 0
      ))
    }
    
    starting_date = starting_date + 1
  }
  
  dataset
}

# Melt the data and plot it so that each restaurant is 1 row
melt_and_plot = function(data, subtitle) {
  melt = melt(
    data = data, 
    id = names(data)[1:4])
  
  ggplot(
    melt, 
    aes(
      day_of_week, 
      monthweek, 
      fill = as.factor(value)
    )
  ) + 
    coord_equal(ratio = 1) + 
    geom_tile(color = "white") + 
    facet_grid(
      variable ~ month, 
      switch = "y", 
      space = "free", 
      labeller = as_labeller(variable_labels)
    ) +
    labs(
      y = "",
      x = "",
      title = "2020* in Booleans",
      subtitle = subtitle,
      fill = "Legend",
      caption = "* Forgot to record data for a couple days in mid-November, which made me too sad to continue."
    ) + 
    theme(
      text = element_text(family = "mono", color = "white"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(), 
      axis.ticks = element_blank(),
      plot.title = element_text(size = 60, face = "bold", margin = margin(b = 40)),
      # plot.subtitle = element_text(size = 40, margin = margin(t = 20, b = 40)),
      plot.subtitle = element_blank(),
      plot.caption = element_text(size = 30, margin = margin(t = 30, b = 20), hjust = 0),
      strip.text.x = element_text(size = 25, face = "bold"),
      strip.text.y = element_text(size = 20, face = "bold"),
      legend.title = element_text(size = 30, face = "bold"),
      legend.text = element_text(size = 30),
      legend.box.margin = margin(l = 40),
      legend.background = element_rect(fill = "black"),
      plot.margin = margin(t = 50, r = 80, b = 20, l = 80),
      plot.background = element_rect(fill = "black")
    )
}

############################################## 
########### READ AND CLEAN DATASET ########### 
##############################################

# Read in restaurants data
restaurants = read.csv("data/Eats/eats_restaurant_names.csv")
restaurants = restaurants[, 2:3]
restaurants = unique(restaurants)
restaurants$restaurant_name = restaurants$Restaurant.Name
restaurants$restaurant_name[which(grepl("McDonald's", restaurants$Restaurant.Name))] = "McDonald's"
restaurants$restaurant_name[which(grepl("The Halal Guys", restaurants$Restaurant.Name))] = "The Halal Guys"
restaurants$restaurant_name[which(grepl("Chick-fil-A", restaurants$Restaurant.Name))] = "Chick-fil-A"
restaurants$restaurant_name[which(grepl("Panera", restaurants$Restaurant.Name))] = "Panera"
restaurants$restaurant_name[which(grepl("KFC", restaurants$Restaurant.Name))] = "KFC"
restaurants$restaurant_name[which(grepl("Firehouse Subs", restaurants$Restaurant.Name))] = "Firehouse Subs"
restaurants$restaurant_name[which(grepl("Taco Bell", restaurants$Restaurant.Name))] = "Taco Bell"
restaurants$restaurant_name[which(grepl("Jack in the Box", restaurants$Restaurant.Name))] = "Jack in the Box"
restaurants$restaurant_name[which(grepl("North Italia", restaurants$Restaurant.Name))] = "North Italia"
restaurants$restaurant_name = factor(restaurants$restaurant_name)
restaurants = restaurants[, c(1,3)]
restaurants = unique(restaurants)

# Read in orders data
orders = read.csv("data/Eats/eats_order_details.csv")

# Aggregate by order_id. One row per order_id, and one row per day.
orders_agg_by_order_id = aggregate(
  orders$Order.Price, 
  by = list(
    order_id = orders$Order.ID,
    Restaurant.ID = orders$Restaurant.ID,
    order_time = orders$Order.Time
  ), 
  max
);
orders_agg_by_order_id$date = as.Date(orders_agg_by_order_id$order_time, format = "%Y-%m-%d %H:%M:%S ")

# Merge with restaurants data to get restaurant name from restaurant_id
orders_agg_by_order_id = merge(x=orders_agg_by_order_id, y=restaurants, by="Restaurant.ID", all.x=T)

# Fill in missing days rows for 2020 and 2021
orders_agg_by_order_id = fill_missing_days(orders_agg_by_order_id, 2020)
orders_agg_by_order_id = fill_missing_days(orders_agg_by_order_id, 2021)

# Convert data from long to wide form so that each restaurant has its own column with price
orders_agg_by_order_id_long = spread(orders_agg_by_order_id, restaurant_name, x)

# Only keep the columns of interest (date, and restaurant names)
data = orders_agg_by_order_id_long[, 5:length(names(orders_agg_by_order_id_long))-1]

# Add custom date helper columns
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

# Subset data by year
data_2020 = subset(data, year(data$date) == 2020)
data_2021 = subset(data, year(data$date) == 2021)


############################################## 
########### SUBSET DATA AND PLOT ########### 
##############################################

melted = 



