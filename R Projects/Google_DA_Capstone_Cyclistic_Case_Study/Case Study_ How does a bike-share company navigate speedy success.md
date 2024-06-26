---
title: "Case Study: How does a bike-share company navigate speedy success"
author: "Muzammil Sharieff"
date: "2024-06-20"
---

## Background

#### **Cyclistic**: A bike-share program that features more than 5,800 bicycles and 600 docking stations. Cyclistic sets itself apart by also offering reclining bikes, hand tricycles, and cargo bikes, making bike-share more inclusive to people with disabilities and riders who can't use a standard two-wheeled bike. The majority of riders opt for traditional bikes; about 8% of riders use the assistive options. Cyclistic users are more likely to ride for leisure, but about 30% use them to commute to work each day.

#### Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members. Cyclistic's finance analysts have concluded that annual members are much more profitable than casual riders.

#### Moreno (Director of Marketing) has set a clear goal: Design marketing strategies aimed at converting casual riders into annual members. In order to do that, however, the marketing analyst team needs to better understand how annual members and casual riders differ, why casual riders would buy a membership, and how digital media could affect their marketing tactics. Moreno and her team are interested in analyzing the Cyclistic historical bike trip data to identify trends.

## Step 1: Getting Information of The Task

### Business Task

#### To identify how do annual members and casual riders use Cyclistic differently.

### Stakeholders

-   **Primary**: Lily Monero, Director of Marketing

-   **Secondary**: Cyclistic executive team (approves recommended marketing program)

## Step 2: Collection and Preparation of Data

### Data & Source Integrity

#### Data is stored in remote server [url](https://divvy-tripdata.s3.amazonaws.com/index.html) and provided by Motivate International Inc. and separated in chunks of querterly .csv files.

#### Data has been protected under data-privacy license mentioned here: [license](https://divvybikes.com/data-license-agreement). This license states that this data has been provided "As is" as per bikeshares sole discretion. So reliability of the data can be vetted eventhough this has been provided by third party.

#### This data cannot be connected to the individual riders and their credit card numbers as unique rider ID has been used to record ride data.

### Observations

#### 1. There are 13 columns in the data files. These are: ride_id, rideable_type, started_at, ended_at, start_station_name, end_station_name, start_station_id, end_station_id, start_station, start_lat, start_lng, end_lat, end_lng, member_casual

#### 2. started_at and ended_at columns contains date-time data and formatted as YYYY-MM-DD HH:MM:SS format.

#### 3. start_station_id and end_station_id has discrepancy. Some of the IDs contain alphabets at the beginning (12 char length) and some contains only numbers (variable length 3-8).

#### 4. Although some of the csv files did not have start_station_name, start_station_id, end_station_name, end_station_id; these rows contain latitude and longitude data. This can be used to fill these empty values.

#### 5. member_casual column contains 2 types of membership data: member or casual.

## Step 3: Processing Data (Cleaning and Transformation)

**Tool Used:**

-   I have Used R to Process, Clean, Analyse and Visualize

## Cleaning and Transformation of Data

Setting Up R Environment for Cleaning and Transformation of Data

```{r}
library(tidyverse)
library(janitor)
library(lubridate)
library(forecast)
library(scales)
```

### Loading the required last 12 months dataset CSV file into R

```{r}
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
```

#### There were a total of 3,356,661 Rows and 18 Columns in bike_rides_data

```{r}
## Combining Multiple Data to a Single Data set and Clean.

bike_rides_data <- rbind(D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12)
bike_rides_data <- bike_rides_data %>% 
  remove_empty(which = c("rows", "cols")) %>% filter(start_station_name !="")
```

#### Converting Data time to Date/Time Format and also adding date and time columns for analysis

```{r}
## Converting Data time to Date/Time Format For analysis

bike_rides_data$Ymd <- as.Date(bike_rides_data$started_at)
bike_rides_data$started_at <- ymd_hms(bike_rides_data$started_at)
bike_rides_data$ended_at <- ymd_hms(bike_rides_data$ended_at)

bike_rides_data$start_hour <- hour(bike_rides_data$started_at)
bike_rides_data$end_hour <- hour(bike_rides_data$ended_at)
```

#### Creating Hours and Minutes Columns to find the duration of each bike trips

```{r}
## To Find The Duration of Each ride in Hours/Mins

bike_rides_data$Hours <- difftime(bike_rides_data$ended_at, bike_rides_data$started_at, units = c("hours"))
bike_rides_data$Minutes <- difftime(bike_rides_data$ended_at, bike_rides_data$started_at, units = c("mins"))
```

#### Creating a dataset (bike_rides_data2) and Removing Negative and Empty Cells

```{r}
## Cleaned bike rides data

bike_rides_data2 <- bike_rides_data %>% 
  filter(Minutes >0) %>%
  drop_na()
```

### Summary Stats: Counts

#### Creating a summary Data Frame to get a overview of the dataset(bike_rides_data2)

```{r}
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
```

#### Summary of Hourly Counts

```{r}
#Summary of Hourly Counts

summary(bikesrides2$Count)
```
![Summary of Hourly Count](R Projects/Google_DA_Capstone_Cyclistic_Case_Study/Screenshots/Summary of Hourly Count.JPG)

#### Table of Counts by Hour

```{r}
# Table of Count by Hour

xtabs(bikesrides2$Count~bikesrides2$start_hour)
```

#### From the above table We can observe that how many rides tool place in a span of 24 hours

## Step 4: Vizualization

### Plot of Rides By Date and Time

### Count of Rides per Day Based on 28 days Moving Average

```{r}

bikesrides2$Monthly <- month(bikesrides2$Weekly)

bikesrides2 %>% ggplot() + geom_col(mapping = aes(x=Weekly, y=Count)) + 
  scale_y_continuous(labels = comma) +
  labs(title = "Count of Riders Per Day",
       subtitle = "Based on 28 days Moving Average",
       y="Average Rides Per Day")

```

**Speculation:**

Number of rides made by both the casual riders and the annual members vary as per month. Such as, during winter riders of less likely to use a bike. On the other hand, although both casual and annual members ride counts are higher during summer, casual riders are more likely to use bikes (June, July, August) compared to annual members. Which is visualized in upcoming graph.

### Count of Rides by Hours

```{r}

bikesrides2 %>% ggplot() + geom_col(mapping = aes(x=start_hour, y=Count)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Count of Rides by Hours",
       x="Hours",
       y="Rides per Hour")

```

**Speculation:**

From the above visualization we can observe that most of the rides took place in between 10 to 20 Hour time. It is clear that 10 to 20 hour time indicates that most of the members using Cyclistic service are the working people and using as their office rides.

### Count of Rides by Bike Types

#### Summary of Bike Types

```{r}

bikestype <- bike_rides_data %>% group_by(member_casual ,rideable_type, Weekly = floor_date(Ymd,"week")) %>% 
  summarise(
    Minutes = sum(Minutes),
    Mean = mean(Minutes),
    Median = median(Minutes),
    Max = max(Minutes),
    Min = min(Minutes),
    Count = n()
  ) %>% ungroup()

```

### Count by Bike Types by Week

```{r}

ggplot(bikestype) +
  geom_area(mapping = aes(x=Weekly, y=Count, fill=rideable_type)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Count of Rides by Bike Type",
       subtitle = "From 2020-04-01 to 2021-03-31")

```

```{r}

ggplot(bikestype) +
  geom_smooth(mapping = aes(x=Weekly, y=Count, col=member_casual)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Count of Rides by Rider Type")

```

**Speculation:**

From the above visualization we can observe that most of the rides use docked_bike and it is often used during the span of May to November. during winter casual riders are less likely to use bikes rather than the annual members. On the other hand, although both casual and annual members ride counts are higher during summer, casual riders are more likely to use bikes (June, July, August) compared to annual members.

```{r}

bike_rides_data %>%
  count(start_station_name, member_casual, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(start_station_name, n), y = n, fill = member_casual)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Start Station by Ride Count",
    x = "Station Name", y = "Count of Rides"
  ) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("member" = "blue", "casual" = "orange"))

```

**Speculation:**

From the above plots it can be seen that Streeter Dr & Grand Ave followed by lake shore Dr & MOnroe St has the largest casual rider gathering. This spot can be used to advertise for the annual membership.

### Preferred Choice of Bikes

```{r}
## Preferred Choice of Bikes
df_bike_choice <- bike_rides_data %>%
  select(member_casual, rideable_type) %>%
  mutate(rideable_type = case_when(
    rideable_type == "classic_bike" ~ "Classic Bike",
    rideable_type == "electric_bike" ~ "Electric Bike",
    rideable_type == "docked_bike" ~ "Docked Bike",
    TRUE ~ rideable_type
  )) %>%
  group_by(rideable_type, member_casual) %>%
  summarize(count = n())
```

### Visualizing Preferred Choice of Bikes

```{r}
ggplot(data = df_bike_choice) +
  geom_col(mapping = aes(x = rideable_type, y = count, fill = member_casual)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Choice of Bikes",
    subtitle = "Choice of bike types between rider types",
    x = "Type of Bikes", y = "No. of Riders"
  )
```

**Speculation**

Casual riders use all three types of bikes whereas annual members prefer classic bikes mostly.

## Step 5: Summarizing Findings

The key business question here was to identify the differences between usage pattern between the 2 types of rider (casual and annual members) of Cyclistic. There has been several differences observed from the above mentioned analysis of 12 month usage data. From the above mentioned analysis 3 major patterns have been identified. These are:

Casual riders spend more time than the annual members riding bikes. This can be indicative of annual members using bikes for specific and routinized purpose.

Most of the casual riders use bike during afternoon whereas, annual members use bikes as work transport which can be clearly seen by the peak during office starting and ending hours.

Usage of bikes in both user types is seasonal. Both casual riders and annual members ride less in during Winter. On the other hand, although ride counts by both types of riders are high during Summer, casual riders are more likely to use bikes.

### Conclusion

In conclusion, there are several features / patterns can be observed from this data set. As per these observations following recommendations can be considered:

During summer the number of bike rides by casual riders are more than the annual members. Moreover, average bike riding time is greater in case of casual riders. This can be a good opportunity to offer discounts to become annual members with cheap rides. The longer the subscription the lower will be the down payment.

In order to attract working casual riders to use bikes for their daily ride to the workplace, special discounts can be offered to them for using electric bikes so that they can reach their workplace quickly without having to face hassle of busy public transport as well as saving some time in the morning and in the evening to spare.

Annual members can get discounts for riding average distance traveled by a casual member distance which can stir interest among the casual members to get annual subscription.
