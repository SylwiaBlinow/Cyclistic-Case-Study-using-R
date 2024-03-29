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

### #Changing TIme and Data Format (from character to date and time)
trip2023$started_at = strptime(trip2023$started_at,"%Y-%m-%d %H:%M:%S")
trip2023$ended_at = strptime(trip2023$ended_at,"%Y-%m-%d %H:%M:%S")

### #Checking Data Structure again
str(trip2023)

### #Adding Trip Duration
trip2023<-mutate(trip2023,tripduration=difftime(ended_at,started_at, units = "secs"))

### #Preview Data
head(trip2023)

### #Filter the Data
