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

### #Checking Data Structure
str(trip2023)
