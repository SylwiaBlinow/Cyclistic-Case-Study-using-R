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

trips_v4 <- trips_all03
trips_v4$year <- format(as.Date(trips_v4$started_at), "%Y")
trips_v4$month <- format(as.Date(trips_v4$started_at), "%m")
trips_v4$day <- format(as.Date(trips_v4$started_at), "%d")
trips_v4$hour <- format(as.Date(trips_v4$started_at), "%H")

### #Create column for day of the week

trips_v4$day_of_week <- format(as.Date(trips_v4$started_at),"%A")


### #Adding Trip Duration
trips_v5 <- mutate(trips_v4, ride_length = difftime(ended_at, started_at, units = "mins"))
str(trips_v5)

### #Preview Data
head(trips_v5)

### #Filter the Data - remove trips <= 2 minutes

trips_v6 <- trips_v5[!trips_v5$ride_length <= 2,]

### #I checked again number of row and the results was 4 166 576

str(trips_v6)

![diff](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/5d7950f6-3835-4f78-99bd-4f64abbc402a)


### #Create a mapping between Polish and English day names

polish_to_english <- c(
  "poniedziałek" = "Monday",
  "wtorek" = "Tuesday",
  "środa" = "Wednesday",
  "czwartek" = "Thursday",
  "piątek" = "Friday",
  "sobota" = "Saturday",
  "niedziela" = "Sunday"
)

### #Replace Polish day names with English day names using the mapping
trips_v6$day_of_week <- polish_to_english[trips_v6$day_of_week]


### #Convert to factor w/levels, specify order of days

trips_v7$day_of_week <- factor(trips_v6$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

### #Removing NA values and checking data again : 4,166,514
trips_v8<-trips_v7 %>% 
  na.omit()
str(trips_v8)


### #Create separate data frames for casual riders and members; may be useful for analysis.
  
  data_member <- trips_v7 %>% 
  filter(member_casual == "member")
data_casual <- trips_v7 %>% 
  filter(member_casual == "casual")


### #Summarize number of trips by membership
 num_trips_by_membership <- table(trips_v7$member_casual) %>% 
  as.data.frame()
  
### #Create pie chart to show number of trips by membership.

ggplot(num_trips_by_membership, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1, color="white") +
  coord_polar("y", start = 0, direction = -1) +
  theme_void() +
  theme(axis.line = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Trips per Membership Type") +
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')) +
  labs(fill = "Membership Type", x = NULL, y = NULL) +
  geom_text(aes(label = paste(round(Freq / sum(Freq) * 100, 1), "%")), position = position_stack(vjust = 0.5))

![pie](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/d91f19fe-c375-4e1a-bf36-604d1f560e73)

### #Calculating Total No. of Rides

![noofrides](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/0244d045-c9e2-43ac-8b39-607952ffb144)

### #Create bar plot to show number of trips by membership       
trips_v8 %>%
  filter(!is.na(member_casual)) %>%
  group_by(member_casual) %>%
  summarise(ride_count = length(ride_id)) %>%
  ggplot() +
  geom_col(mapping = aes(x = member_casual, y = ride_count, fill = member_casual)) +
  geom_text(mapping = aes(label = comma(ride_count), x = member_casual, y = ride_count), position = position_stack(vjust = 0.5)) +  # Add labels with formatted values
  labs(title = "Total No of Rides") +
  scale_y_continuous(labels = comma)

![chart](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/462b7846-b44a-43a1-8762-c149c3cbcd5f)

### #Calculating average ride length and no. of rides as per day of the week

![noweeks](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/442dac43-5aed-49bd-a3bf-ffa4a8e5c382)

### #Create bar plot to show the Days of the Week with No. of rides taken by Riders

trips_v8 %>% 
  filter(!is.na(member_casual)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides=n(), .groups = "drop") %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title = "Total Rides vs.Day of The Week") +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = function(x) format(x,scientific = FALSE))

![chart2](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/6b9566ff-9044-46c4-8876-588b00a2870e)

### #Calculating mean, median, max., min. for ride_length

trips_v8 %>% 
  group_by(member_casual) %>% 
summarise(average_ride_length=mean(ride_length),median_ride_length=median(ride_length),max_ride_length=max(ride_length),min_ride_length=min(ride_length))

![mean](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/984866e6-8bb9-4cbc-97d9-7f111f5de924)


### #Adding another column for different periods in a day

breaks<-hour((hm("00:00", "6:00", "12:00", "18:00", "23:59")))

labels <- c("Night", "Morning", "Afternoon", "Evening")

### #Defining Time of the Day

trips_v8$time_of_the_trip <-cut(x=hour(trips_v8$started_at), breaks = breaks, labels = labels, include.lowest = "true")


### #Summarize average trip duration by time of day, for each membership type.

trips_v8 %>% 
  group_by(time_of_the_trip, member_casual) %>%
  summarize(average_ride_length=mean(ride_length)) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame() %>% 
  print(n = nrow(48))


### #create dataframe for summarized data.
average_ride_length <- trips_v8 %>%
  group_by(time_of_the_trip, member_casual) %>%
  summarize(average_ride_length = mean(ride_length), .groups = "drop") %>%
  mutate_if(is.numeric, round, 2)

print(average_ride_length)

### #plot average trip duration by time of day, for each membership type.
ggplot(data = average_ride_length, aes(x = time_of_the_trip, y = average_ride_length, color = member_casual, group = member_casual)) +
  geom_line(size = 2) +
  coord_cartesian(ylim = c(10, 25)) +  # Adjusting y-axis limits
  labs(x = "Time of Day", y = "Mean Trip Duration (minutes)", color = "Membership Type") +
  ggtitle("Mean Trip Duration by Time of Day") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, face = 'bold')) +
  guides(color = guide_legend(title = "Membership Type"))

![time](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/85ac0b8e-fd36-4b63-a57c-3256d9616ded)


### #Comparing Ride Lengths between different Times of the Day

trips_v8 %>% 
  group_by(member_casual,time_of_the_trip) %>% 
  summarise(number_of_rides=n(),average_trip_duration=mean(ride_length),.groups = "drop")

![clocl](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/08a96a79-4534-4819-bed5-b6e5855312d4)


### #summarize number of trips per time of day, for each membership type.
table(trips_v8$time_of_the_trip, trips_v8$member_casual)


### #create data frame of trips per time of day, for each membership type.
num_trips_by_time<- trips_v8 %>% 
  group_by(time_of_the_trip, member_casual) %>%
  summarize(n=n()) %>%
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame() %>% 
  print(n = nrow(24))

![time2](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/c6455272-efdf-4e96-8b46-6145d500a564)

num_trips_by_time %>% 
  ggplot() +
  geom_line(aes(x=time_of_the_trip, y=n, color=member_casual, group=member_casual), size = 2) +
  scale_y_continuous(name = "Number of Trips", labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  labs(x = "Time of Day", y = "Number of Trips", fill = "Membership Type") +
  ggtitle("Number of Trips per Time of Day") +
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')) +
  guides(color=guide_legend("Membership Type"))


 ![time3](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/71b18583-fef3-47fa-9d66-744fc76fbcda)

### #Average Trip Duration vs. Day of The Week
trips_v8 %>% 
  group_by(member_casual,day_of_week) %>% 
  summarise(average_ride_length=mean(ride_length), .groups = "drop") %>% 
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  labs(title = "Average Trip Duration vs. Day of The Week")

![durationinweek](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/3af9a0f9-c4ee-46f2-841c-95c2c732ee87)


### #Average ride length vs. month

trips_v8 %>% 
  group_by(member_casual, month) %>%
  summarise(average_ride_length = mean(ride_length), .groups = "drop") %>%
  ggplot(aes(x = month, y = average_ride_length, fill = member_casual)) +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  labs(title = "Average ride length vs. month")


![month](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/c127b142-be5a-45e8-b15f-d5bc3e0e9800)

### #Comparing Casual and Member Rides by Distance

trips_v8 %>% 
  group_by(member_casual) %>%
  summarise(average_ride_distance = mean(ride_length)) %>%
  ggplot() + geom_col(mapping = aes(x = member_casual, y = average_ride_distance, fill = member_casual), show.legend = FALSE) +
  labs(title = "Mean Distance")

![meandis](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/cc38a4c6-aee2-48ab-9402-1416a0ea7470)


### #Comparing different types of ride

trips_v8 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_rides = n(), .groups = "drop") %>%
  ggplot() + 
  geom_col(mapping = aes(x = rideable_type, y = number_of_rides, fill = member_casual), show.legend = TRUE) +
  labs(title = "Ride type and no. of rides") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

![types](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/5871236e-b08e-48a4-bea2-c88a70a60507)


### #list most popular station
 num_trips_by_station <- trips_v8  %>% 
  group_by(start_station_name) %>% 
  summarize(n=n()) 
  num_trips_by_station[order(num_trips_by_station$n, decreasing = TRUE),]



### #create bubble map
install.packages("leaflet")

library(leaflet)
install.packages("htmlwidgets")
library(htmlwidgets)
install.packages("htmltools")
library(htmltools)


map_data <- trips_v8 %>%
  select( start_station_name, 
          start_lat, 
          start_lng) %>%
  group_by(start_station_name) %>%
  mutate(numtrips = n()) %>%
  distinct(start_station_name, .keep_all = TRUE)


![stations](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/0972d207-c1f8-480a-ad3d-8fec439fdb56)




map_bins <- seq(0, 50000, by = 5000)

my_palette <- colorBin(palette ="viridis", domain = map_data$numtrips, na.color = "transparent", bins = map_bins, reverse = TRUE)

map_text <- paste("Station name: ", map_data$start_station_name, "<br/>","Number of trips: ", map_data$numtrips, sep = "") %>%
  lapply(htmltools::HTML)


trips_per_station_map <- leaflet(map_data) %>% 
  addTiles() %>%  
  

setView(lng = -87.6298, lat = 41.8781, zoom = 10.5) %>% 
  

addProviderTiles("Esri.WorldGrayCanvas") %>%
  
addCircleMarkers(~ start_lng, ~ start_lat, 
fillColor = ~ my_palette(numtrips),
fillOpacity = 0.6, 
color = "white", 
radius = 6,
stroke = FALSE,
label = map_text,
labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
textsize = "13px", 
direction = "auto")) %>%

addLegend( 
pal = my_palette, 
values = ~ numtrips, 
opacity = 0.8,
title = "No. of trips", 
position = "bottomright")

trips_per_station_map

![map](https://github.com/SylwiaBlinow/Cyclistic-Case-Study-using-R/assets/156024627/4fbefc61-5ae6-40c3-9825-9bc1bfe6485e)


### Recommendations

We can conclude that member users and casual users are quite different group of people:
* casual users use bike for casual reasons such as sightseeing or leisure. They have peak season of summertime, peak usage on weekend and off-rush hour and spend more time on bike. 
* member users use bike at commuting hours in commercial areas with heavy traffic. They use bike more often but for short time.

To convert casual riders into annual members, the following marketing strategies can be implemented:
* online advertising with app benefits for memebrs
* social media marketing showing users and they trips 
* customer referral marketing with discounts
* offering loyalty programs especially during summertime and weekends
* organizing sightseeing events for members
