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

data = read.csv("processed_data.csv")
data$date = as.Date(data$date, "%Y-%m-%d")

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


############################################## 
############ SUBSET DATA AND PLOT ############ 
##############################################

# Subset data by year
data_2020 = subset(data, year(data$date) == 2020)
data_2020 = calculate_month_week(data_2020)

data_2021 = subset(data, year(data$date) == 2021)
data_2021 = calculate_month_week(data_2021)

# Collect the dataset for 2020 to plot
data_2020_main = data.frame(
  "date" = data_2020$date, 
  "month" = data_2020$month, 
  "day_of_week" = data_2020$day_of_week, 
  "monthweek" = data_2020$monthweek,
  
  "The.Halal.Guys" = data_2020$The.Halal.Guys,
  "McDonald.s" = data_2020$McDonald.s,
  "Goldenpot" = data_2020$Goldenpot,
  "Firehouse.Subs" = data_2020$Firehouse.Subs,
  "Taco.Bell" = data_2020$Taco.Bell,
  "North.Italia" = data_2020$North.Italia,
  "Chick.fil.A" = data_2020$Chick.fil.A,
  "Panera" = data_2020$Panera,
  "Jack.in.the.Box" = data_2020$Jack.in.the.Box,
  "KFC" = data_2020$KFC
)
melt_and_plot(data_2020_main, "2020")


# Collect the dataset for 2021 to plot
data_2021_main = data.frame(
  "date" = data_2021$date, 
  "month" = data_2021$month, 
  "day_of_week" = data_2021$day_of_week, 
  "monthweek" = data_2021$monthweek,
  
  "The.Halal.Guys" = data_2021$The.Halal.Guys,
  "McDonald.s" = data_2021$McDonald.s,
  "Goldenpot" = data_2021$Goldenpot,
  "Firehouse.Subs" = data_2021$Firehouse.Subs,
  "Taco.Bell" = data_2021$Taco.Bell,
  "North.Italia" = data_2021$North.Italia,
  "Chick.fil.A" = data_2021$Chick.fil.A,
  "Panera" = data_2021$Panera,
  "Jack.in.the.Box" = data_2021$Jack.in.the.Box,
  "KFC" = data_2021$KFC
)
melt_and_plot(data_2021_main, "2021")









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


# Helper function to melt and plot the data
# Assumes that the first four columns of the data are date/month/related
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
      title = "2020 Uber Eats Deliveries",
      subtitle = subtitle,
      fill = "Legend",
    ) + 
    theme(
      text = element_text(family = "mono", color = "white"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(), 
      axis.ticks = element_blank(),
      plot.subtitle = element_blank(),
      plot.background = element_rect(fill = "black")
    )
}



