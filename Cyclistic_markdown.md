google-data-analytics-capstone-project
================
amirsyml
2023-01-15

## Packages

``` r
library(tidyverse) # For data wrangling, tidying and anlysing

library(ggplot2) # For data visualisation
```

## Working Directories

``` r
getwd() # Display working directory
```

    ## [1] "C:/Users/amirs/OneDrive/Documents/google-data-analytics-capstone-project"

``` r
setwd("D:/Personal/Education/Digital_Literacy/Google_Data_Analytics/R/R_Data/Divvy_Data") # Sets working directory to calls data
```

## Step 1: Collect Data

``` r
Jan_2021 <- read.csv("202101-divvy-tripdata.csv")
Feb_2021 <- read.csv("202102-divvy-tripdata.csv")
Mar_2021 <- read.csv("202103-divvy-tripdata.csv")
Apr_2021 <- read.csv("202104-divvy-tripdata.csv")
May_2021 <- read.csv("202105-divvy-tripdata.csv")
Jun_2021 <- read.csv("202106-divvy-tripdata.csv")
Jul_2021 <- read.csv("202107-divvy-tripdata.csv")
Aug_2021 <- read.csv("202108-divvy-tripdata.csv")
Sep_2021 <- read.csv("202109-divvy-tripdata.csv")
Oct_2021 <- read.csv("202110-divvy-tripdata.csv")
Nov_2021 <- read.csv("202111-divvy-tripdata.csv")
Dec_2021 <- read.csv("202112-divvy-tripdata.csv")
```

## Step 2: Wrangle Data and Combine into a single file

``` r
# Inspect the dataframes
glimpse(Jan_2021)
glimpse(Feb_2021)
glimpse(Mar_2021)
```

``` r
# Combining individual data frame into one big data frame
all_trips <- bind_rows(Jan_2021, Feb_2021, Mar_2021, Apr_2021, May_2021, 
                       Jun_2021, Jul_2021, Aug_2021, Sep_2021, Oct_2021, 
                       Nov_2021, Dec_2021)
```

``` r
# Removing irrelevant column 
all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))
```

## Step 3: Cleaning Data and Addition of Data for Analysis

``` r
# Inspect the new table that has been created
colnames(all_trips) #List of column names
nrow(all_trips) #How many rows are in data frame?
dim(all_trips) #Dimensions of the data frame?
head(all_trips) #See the first 6 rows of data frame. Also tail(all_trips)
str(all_trips) #See list of columns and data types (numeric, character, etc)
summary(all_trips) #Statistical summary of data. Mainly for numerics
```

``` r
# Adding additional columns of data, such as date, day, month, year 
# This will allow us to aggregate ride data for each day, month or year
all_trips$date <- as.Date(all_trips$started_at) 
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
```

``` r
# Adding "ride_length" to the entire dataframe for consistency
# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
```

``` r
# Inspect the structure of columns
str(all_trips)
```

``` r
# Convert "ride_length" from Factor to numeric so we can run calculations on data
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
```

## Step 4: Conduct Descriptive Analysis

``` r
# Descriptive analysis on ride_length (in seconds)
mean(all_trips$ride_length) #straight average (total ride length / rides)
median(all_trips$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips$ride_length) #longest ride
min(all_trips$ride_length) #shortest ride
```

``` r
# Condense the four lines above to one line using summary()
summary(all_trips$ride_length)
```

``` r
# Compare members and casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = mean)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = median)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = max)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week,
          FUN = mean)
```

``` r
all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c(
  "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```

``` r
# analyze ridership data by type and weekday
all_trips %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using wday()
  group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n() #calculates the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(member_casual, weekday)
```

``` r
# Visualize the number of rides by rider type
all_trips %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
```

``` r
# Visualization for average duration
all_trips %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
```

## Step 5: Export Summary File for Further Analysis

``` r
# Create a csv file that we will visualise in Tableau or PowerBI
counts <- aggregate(all_trips$ride_length ~ all_trips$member_casual +
                      all_trips$day_of_week, FUN = mean)
write.csv(counts, file = 
            'D:/Personal/Education/Digital_Literacy/Google_Data_Analytics/R/R_Data/Divvy_Data/avg_ride_length.csv')
write.csv(all_trips, file = 
            'D:/Personal/Education/Digital_Literacy/Google_Data_Analytics/R/R_Data/Divvy_Data/all_trips.csv') 
```
