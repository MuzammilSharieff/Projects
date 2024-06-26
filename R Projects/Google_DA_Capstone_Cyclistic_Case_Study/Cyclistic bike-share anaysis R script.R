## Setting up R Environment to work with data

install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
install.packages("forecast")
install.packages("scales")
library(tidyverse)
library(janitor)
library(lubridate)
library(forecast)
library(scales)

## Loading data into R studio

D1 <- read.csv("./Data/Data 1.csv")
D2 <- read.csv("./Data/Data 2.csv")
D3 <- read.csv("./Data/Data 3.csv")
D4 <- read.csv("./Data/Data 4.csv")
D5 <- read.csv("./Data/Data 5.csv")
D6 <- read.csv("./Data/Data 6.csv")
D7 <- read.csv("./Data/Data 7.csv")
D8 <- read.csv("./Data/Data 8.csv")
D9 <- read.csv("./Data/Data 9.csv")
D10 <- read.csv("./Data/Data 10.csv")
D11 <- read.csv("./Data/Data 11.csv")
D12 <- read.csv("./Data/Data 12.csv")

## Combining Multiple Data to a Single Data set and Clean.

bike_rides_data <- rbind(D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12)
bike_rides_data <- bike_rides_data %>% 
  remove_empty(which = c("rows", "cols"))

## Converting Data time to Date/Time Format For analysis

bike_rides_data$Ymd <- as.Date(bike_rides_data$started_at)
bike_rides_data$started_at <- ymd_hms(bike_rides_data$started_at)
bike_rides_data$ended_at <- ymd_hms(bike_rides_data$ended_at)

bike_rides_data$start_hour <- hour(bike_rides_data$started_at)
bike_rides_data$end_hour <- hour(bike_rides_data$ended_at)

## To Find The Duration of Each ride in Hours/Mins

bike_rides_data$Hours <- difftime(bike_rides_data$ended_at, bike_rides_data$started_at, units = c("hours"))
bike_rides_data$Minutes <- difftime(bike_rides_data$ended_at, bike_rides_data$started_at, units = c("mins"))

# Cleaned bike rides data
bike_rides_data2 <- bike_rides_data %>% 
  filter(Minutes >0) %>%
  drop_na()

## Creating a Summary Data Frame
bikesrides2 <- bike_rides_data2%>%
  group_by(Weekly = floor_date(Ymd,"week"), start_hour) %>% 
  summarise(
    Minutes = sum(Minutes),
    Mean = mean(Minutes),
    Median = median(Minutes),
    Max = max(Minutes),
    Min = min(Minutes),
    Count = n()
  ) %>% ungroup()

## Forecasting Memberships of Riders
bikesrides2$CountMembership <- forecast::ma(bikesrides2$Count,28)

## Plot of Rides By Date
#### Summary Stats: Counts

#*Summary of Hourly Counts (RMD file)
#Summary of Hourly Counts
summary(bikesrides2$Count)

#*Table of Counts by Hour (RMD File)
# Table of Count by Hour
xtabs(bikesrides2$Count~bikesrides2$start_hour)

#Visualization
bikesrides2$Monthly <- month(bikesrides2$Weekly)

bikesrides2 %>% ggplot() + geom_col(mapping = aes(x=Weekly, y=Count)) + 
  scale_y_continuous(labels = comma) +
  labs(title = "Count of Riders Per Day",
       subtitle = "Based on 28 days Moving Average",
       y="Average Rides Per Day")
