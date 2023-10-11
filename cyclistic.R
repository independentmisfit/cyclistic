install.packages("tidyverse")
install.packages("lubridate")
install.packages("plotly")
library(tidyverse)
library(lubridate)
library(plotly)







# import the data
df_01 <- read.csv("C:/Users/HP/Documents/cylistic_data/202301-divvy-tripdata.csv")
df_02 <- read.csv("C:/Users/HP/Documents/cylistic_data/202302-divvy-tripdata.csv")
df_03 <- read.csv("C:/Users/HP/Documents/cylistic_data/202303-divvy-tripdata.csv")
df_04 <- read.csv("C:/Users/HP/Documents/cylistic_data/202304-divvy-tripdata.csv")
df_05 <- read.csv("C:/Users/HP/Documents/cylistic_data/202305-divvy-tripdata.csv")
df_06 <- read.csv("C:/Users/HP/Documents/cylistic_data/202306-divvy-tripdata.csv")

# compare coulmn names
colnames(df_01)
colnames(df_02)
colnames(df_03)
colnames(df_04)
colnames(df_05)
colnames(df_06)

# inspect the dataframes for incongruencies

str(df_01)
str(df_02)
str(df_03)
str(df_03)
str(df_04)
str(df_05)
str(df_06)

#inspect combined data frame
trip_data <- list.files("C:/Users/HP/Documents/cylistic_data", pattern="*.csv") %>% 
  lapply(read_csv) %>% 
  bind_rows
str(trip_data)
nrow(trip_data)
dim(trip_data)
summary(trip_data)


# Add columns for ride length, day of the week and month ride started

# The time variable is to get the time difference in seconds between the time a ride ends and when it started
# The format variable is to express the time in seconds in the time variable in the h:m:s format.
# The time and format variables are necessary so we can easily integrate the functions in the variable in the mutuate function in the new_df data frame
time <- seconds_to_period(trip_data$ended_at - trip_data$started_at)
format <- sprintf('%2d:%2d:%2d', time@hour, minute(time), second(time))

new_trip_data <- trip_data %>% mutate(ride_length = format, day_of_week = wday(started_at), 
                                      day_of_week_abb = wday(started_at, label=TRUE), 
                                      mon = month(started_at, label=TRUE)) 

str(new_trip_data)

# convert ride_length column to numeric data type

new_trip_data$ride_length <- as.numeric(seconds(hms(new_trip_data$ride_length)))

# remove rows with negative ride_length values
new_trip_data_v2 <- new_trip_data[!(new_trip_data$ride_length < 0), ]
str(new_trip_data_v2)

#descriptive analysis

summary(new_trip_data_v2$ride_length)

# ride length by rider type



new_trip_data_v2 %>%  aggregate(ride_length ~ member_casual, FUN = summary)

# number of rides by users
new_trip_data_v2 %>%  group_by(member_casual) %>% count()

# number of rides by users by day of the week

ride_summary<- new_trip_data_v2 %>% group_by(member_casual, day_of_week_abb) %>% summarise(number_of_rides = n(), 
                                                                                           average_duration = mean(ride_length))



ggplotly(ggplot(data = ride_summary) +
           geom_col(mapping = aes(x = day_of_week_abb, y = average_duration, fill = member_casual), position = "dodge") +
           labs(title = "Bar Plot of Average Ride Duration by Day of the Week",
                x = "Day of the Week",
                y = "Average Ride Duration in Secs"))



ggplotly(ggplot(data = ride_summary) +
           geom_col(mapping = aes(x = day_of_week_abb, y = number_of_rides, fill = member_casual), position = "dodge") +
           labs(title = "Bar Plot of Total Number of Rides by Day of the Week",
                x = "Day of the Week",
                y = "Total number of Rides"))


# rides by months of the year 
ride_sum_months <- new_trip_data_v2 %>% group_by(member_casual, mon) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length))

ride_sum_months

ggplotly(ggplot(data = ride_sum_months, aes(x = mon, y = number_of_rides, group = member_casual, color = member_casual)) +
           geom_line(size = 0.5) +
           geom_point()+                  
           labs(title = "Line Graph of Total Number of Rides by Day of the Week",
                x = "Months",
                y = "Total number of Rides"))


ggplotly(ggplot(data = ride_sum_months, aes(x = mon, y = average_duration, group = member_casual, color = member_casual)) +
           geom_line(size = 0.5) +
           geom_point()+                  
           labs(title = "Line Graph of Average Duration of Rides by Day of the Week",
                x = "Months",
                y = "Total number of Rides"))

# Visualization of total rides rides by member riders in diff. hours of Tuedays in the first half of 2023


ggplotly(new_trip_data_v2 %>% group_by(day_of_week_abb) %>% filter(day_of_week_abb == "Tue", member_casual == "member") %>% 
           mutate(hour = hour(started_at)) %>% ggplot(aes(x = hour)) +
           geom_bar(fill = "deepskyblue1") +
           labs(title = "No. of Rides Started at Diff Hours by Member Riders on Tuesdays",
                x = "Hour",
                y = "Number of Rides"))


# Visualization of total rides rides by member riders in diff. hours of Wednesdays in the first half of 2023

ggplotly(new_trip_data_v2 %>% group_by(day_of_week_abb) %>% filter(day_of_week_abb == "Wed", member_casual == "member") %>% 
           mutate(hour = hour(started_at)) %>% ggplot(aes(x = hour)) +
           geom_bar(fill = "deepskyblue1") +
           labs(title = "No. of Rides Started at Diff Hours by Member Riders on Wednesdays",
                x = "Hour",
                y = "Number of Rides"))


# Visualization of total rides rides by member riders in diff. hours of all weekdays in the first half of 2023


ggplotly(new_trip_data_v2 %>% group_by(day_of_week_abb) %>% filter( member_casual == "member") %>% 
           mutate(hour = hour(started_at)) %>% ggplot(aes(x = hour))+
           geom_bar(fill ="deepskyblue1") +
           facet_grid(~day_of_week_abb) +
           labs(title = "No. of Rides Started at Diff. Hours on All Weekdays",
                x = "Hour",
                y = "Number of Rides"))



# Visualization of total rides rides by Casual rides in diff. hours of Tuesdays in the first half of 2023


ggplotly(new_trip_data_v2 %>% group_by(day_of_week_abb) %>% filter(day_of_week_abb == "Tue", member_casual == "casual") %>% 
           mutate(hour = hour(started_at)) %>% ggplot(aes(x = hour))+
           geom_bar(fill = "indianred2") +
           labs(title = "No. of Rides Started at Diff Hours by Casuals Riders on Tuesdays",
                x = "Hour",
                y = "Number of Rides"))

# Visualization of total rides rides by Casual rides in diff. hours of Wednesdays in the first half of 2023

ggplotly(new_trip_data_v2 %>% group_by(day_of_week_abb) %>% filter(day_of_week_abb == "Wed", member_casual == "casual") %>% 
           mutate(hour = hour(started_at)) %>% ggplot(aes(x = hour))+
           geom_bar(fill = "indianred2") +
           labs(title = "No. of Rides Started at Diff Hours by Casuals Riders on Tuesdays",
                x = "Hour",
                y = "Number of Rides"))

# Visualization of total rides rides by Casual rides in diff. hours of weekdays in the first half of 2023

ggplotly(new_trip_data_v2 %>% group_by(day_of_week_abb) %>% filter( member_casual == "casual") %>% 
           mutate(hour = hour(started_at)) %>% ggplot(aes(x = hour))+
           geom_bar(fill = "indianred2") +
           facet_grid(~day_of_week_abb) +
           labs(title = "No. of Rides Started at Diff. Hours By Casual Riders on All Weekdays",
                x = "Hour",
                y = "Number of Rides")) 
