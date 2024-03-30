# Cyclistic-Case-Study-using-R


### #installing necessary packages and reading files into dataframes 

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")


### #Set working directory to the folder where bike data is held

setwd("D:/Google data analyst/Portfolio/Cyclistic/DATA/CSV")

### #Upload Data

trips_202301 <- read.csv("202301-divvy-tripdata.csv")

trips_202302 <- read.csv("202302-divvy-tripdata.csv")

trips_202303 <- read.csv("202303-divvy-tripdata.csv")

trips_202304 <- read.csv("202304-divvy-tripdata.csv")

trips_202305 <- read.csv("202305-divvy-tripdata.csv")

trips_202306 <- read.csv("202306-divvy-tripdata.csv")

trips_202307 <- read.csv("202307-divvy-tripdata.csv")

trips_202308 <- read.csv("202308-divvy-tripdata.csv")

trips_202309 <- read.csv("202309-divvy-tripdata.csv")

trips_202310 <- read.csv("202310-divvy-tripdata.csv")

trips_202311 <- read.csv("202311-divvy-tripdata.csv")

trips_202312 <- read.csv("202312-divvy-tripdata.csv")


### #Binding Data Together

trip2023 <- rbind(trips_202301, trips_202302, trips_202303, trips_202304, trips_202305, trips_202306, trips_202307, trips_202308, trips_202309, trips_202310, trips_202311, trips_202312)

### #Checking Data Structure - 5 719 877 rows
str(trip2023)


### #Checking empty rows
null_count <- colSums(is.na(trips_all))
print(null_count)

![null](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/991cbdc3-0c0c-459b-8477-697455787718)

null_count2 <- colSums(trips_all == "")
print(null_count2)

![null2](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/75c680a1-d327-4762-a82e-f592db5789fa)


### #Checking duplicates - there weren't any

duplicates <- trips_all[duplicated(trips_all), ]
print(duplicates)

![duplicates](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/6cf6a283-f2b0-4893-b906-68f0fbf87f05)

### #Removing empty rows

trips_all01 <- trips_all[complete.cases(trips_all), ]

trips_all02 <- trips_all01[trips_all01$end_station_id != "", ]

trips_all03 <- trips_all02[trips_all02$start_station_id != "", ]


### #Checking data after removing values : 4,331,707 rows so 1 388 170 null rows were removed

glimpse(trips_all03)

![glimps](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/af724d05-2923-4b02-acff-2300e0548024)


### #Checking data types

summary(trips_all03)

![summary](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/258e05f7-d8df-434e-895c-5ecf99f02234)


Looks like the started_at an ended_at columns are type chr. We need to convert them to the date column type using the as.POSIXct() function.

### #Changing Time and Data Format (from character to date and time)

trips_all03$started_at <- as.POSIXct(trips_all03$started_at, format = "%Y-%m-%d %H:%M:%S")
trips_all03$ended_at <- as.POSIXct(trips_all03$ended_at, format = "%Y-%m-%d %H:%M:%S")


### #Checking Data Structure again
str(trips_all03)

### #Adding the individual columns for date, day, month, year, day of the week to ease the in-depth analysis

trips_all04 <- trips_all03
trips_all04$year <- format(trips_all04$started_at, "%Y")
trips_all04$month <- format(trips_all04$started_at, "%m")
trips_all04$day <- format(trips_all04$started_at, "%d")
trips_all04$hour <- format(trips_all04$started_at, "%H")

### #Create column for day of the week

trips_all04$day_of_week <- format(trips_all04$started_at,"%A")

### #Convert to factor w/levels, specify order of days

trips_all04$day_of_week <- factor(trips_all04$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

### #Adding Trip Duration
trips_all05 <- trips_all04
trips_all05$trip_duration_seconds <- difftime(trips_all05$ended_at, trips_all05$started_at, units = "secs") 
trips_all05$trip_duration_seconds <- as.numeric(as.character(trips_all05$trip_duration_seconds))

### #Preview Data
head(trips_all05)

### #Filter the Data - remove trips < 120 minutes

trips_duration <- trips_all05 %>%
  filter(trip_duration_seconds > 120)


### #Create separate data frames for casual riders and members; may be useful for analysis.
  data_member <- trips_duration %>% 
    filter(member_casual == "member")
  data_casual <- trips_duration %>% 
    filter(member_casual == "casual")
