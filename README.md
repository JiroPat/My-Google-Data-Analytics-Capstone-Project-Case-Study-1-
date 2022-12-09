# My-Google-Data-Analytics-Capstone-Project-Case-Study-1-
This project forms part of the requirements for having a complete certificate in the Google Data Analytics Professional Certificate. I sought guidance from the repositories of ctan1237 and kellyjadams on this same case study.

#=================================================
# Install required packages
# tidyverse for data import and wrangling
# libridate for date functions
# ggplot for visualization
#=================================================

library(tidyverse)
library(lubridate)
library(ggplot2)
 
getwd()
[1] "C:/Users/UDCL/Documents"

setwd("~/Data Analytics Project Case Study")
 
#=====================================
#        COLLECT DATA
#=====================================
 
dec21_df <-read_csv("202112-divvy-tripdata.csv")
jan22_df <-read_csv("202201-divvy-tripdata.csv")
feb22_df <-read_csv("202202-divvy-tripdata.csv")
mar22_df <-read_csv("202203-divvy-tripdata.csv")
apr22_df <-read_csv("202204-divvy-tripdata.csv")
may22_df <-read_csv("202205-divvy-tripdata.csv")
jun22_df <-read_csv("202206-divvy-tripdata.csv")
jul22_df <-read_csv("202207-divvy-tripdata.csv")
aug22_df <-read_csv("202208-divvy-tripdata.csv")
sep22_df <-read_csv("202209-divvy-tripdata.csv")
oct22_df <-read_csv("202210-divvy-tripdata.csv")
nov22_df <-read_csv("202211-divvy-tripdata.csv")

#Merge/stack all monthly dataframes into one
#=========================================
 
Cyclists <- bind_rows(dec21_df,jan22_df,feb22_df,mar22_df,apr22_df,may22_df,jun22_df,jul22_df,aug22_df,sep22_df,oct22_df,nov22_df)
 
#Remove monthly dataframes to create space in the Environment
#==========================================================
 
remove(dec21_df,jan22_df,feb22_df,mar22_df,apr22_df,may22_df,jun22_df,jul22_df,aug22_df,sep22_df,oct22_df,nov22_df)
 
 
#====================================
 #       PROCESS DATA
#====================================
 
#Remove all columns not needed for this analysis
#===============================================
 
Cyclists <- Cyclists %>%
select (-c(start_lat, start_lng, end_lat, end_lng))


#Calculate Ride_length (by subtracting ended_at time from started_at time and converting it to minutes)
Cyclists$ride_length <- difftime(Cyclists$ended_at, Cyclists$started_at, units = "mins")
Cyclists$ride_length <- round(Cyclists$ride_length, digits = 1)

# Clean Data
#===========
 
Cyclists <- na.omit(Cyclists) #remove rows with NA values
Cyclists <- distinct(Cyclists) #remove duplicate rows 
Cyclists <- Cyclists[!(Cyclists$ride_length <=0),] #remove where ride_length is 0 or negative
  
# Add columns that list the date, month, day, and year of each ride
# This will aggregate ride data for each month, day, or year
#=================================================================================

Cyclists$date <- as.Date(Cyclists$started_at) #The default format is yyyy-mm-dd
Cyclists$month <- format(as.Date(Cyclists$date), "%m")
Cyclists$day <- format(as.Date(Cyclists$date), "%d")
Cyclists$year <- format(as.Date(Cyclists$date), "%Y")
Cyclists$day_of_week <- format(as.Date(Cyclists$date), "%A")
 
 
 # Create a copy of the dataframe (for analysis) and rename it
 #============================================================
 
 Riders <- Cyclists
 
 
#===============================================
#  DESCRIPTIVE ANALYSIS
#===============================================
 
# Descriptive analysis on ride_length (all figures in minutes)

mean(Riders$ride_length) #straight average (total ride length / rides)
median(Riders$ride_length) #midpoint number in the ascending array of ride lengths
max(Riders$ride_length) #longest ride
min(Riders$ride_length) #shortest ride

 
# Compare mean, median, max, min for members and casual users respectively
#==========================================================================
 
aggregate(Riders$ride_length ~ Riders$member_casual, FUN = mean)
aggregate(Riders$ride_length ~ Riders$member_casual, FUN = median)
aggregate(Riders$ride_length ~ Riders$member_casual, FUN = max)
aggregate(Riders$ride_length ~ Riders$member_casual, FUN = min)


# See the daily average ride time for members vs casual users
#============================================================
 
aggregate(Riders$ride_length ~ Riders$member_casual + Riders$day_of_week, FUN = mean)


# Sort the day of week to beginning from Sunday and end on Saturday
#==================================================================

Riders$day_of_week <- ordered(Riders$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# Check if the order has been sorted
#====================================
 
aggregate(Riders$ride_length ~ Riders$member_casual + Riders$day_of_week, FUN = mean)

# ///Sorting is successful\\\

 
# analyze ridership data by type and weekday
#===========================================
 
Riders %>% 
     mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field #using wday()
      group_by(member_casual, weekday) %>%  #groups by usertype and weekday
    
 
     summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 	
     arrange(member_casual, weekday)	

# Visualize the number of rides by rider type
#============================================
 
 Riders %>% 
     mutate(weekday = wday(started_at, label = TRUE)) %>% 
     group_by(member_casual, weekday) %>% 
     summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
     arrange(member_casual, weekday)  %>% 
     ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
     geom_col(position = "dodge")
     
     
 
# Create a visualization for average duration
#============================================
 
 Riders %>% 
     mutate(weekday = wday(started_at, label = TRUE)) %>% 
     group_by(member_casual, weekday) %>% 
     summarise(number_of_rides = n()
               ,average_duration = mean(ride_length)) %>% 
     arrange(member_casual, weekday)  %>% 
     ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
     geom_col(position = "dodge")

Riders %>% 
     mutate(weekday = wday(started_at, label = TRUE)) %>% 
     group_by(rideable_type, weekday) %>% 
     summarise(number_of_rides = n()
              ,average_duration = mean(ride_length/60)) %>% 
     arrange(rideable_type, weekday)  %>% 
     ggplot(aes(x = weekday, y = average_duration, fill = rideable_type)) +
     geom_col(position = "dodge") + 
     labs(title = "Table 5: Average Ride Duration by Day and Bike Type") + 
     ylab("Average Duration (minutes)") + 
     xlab("Day of Week")

 Riders %>% 
     group_by(member_casual, month) %>% 
     summarise(number_of_rides = n()
               ,average_duration = mean(ride_length)) %>% 
     arrange(member_casual, month)  %>% 
     ggplot(aes(x = month, y = number_of_rides, group = member_casual)) +
     geom_line(aes(color = member_casual)) + 
     geom_point() +
     labs(title = "Table 6: Number of Rides by Month and Rider Type") + 
     ylab("Number of Rides (1e+05 = 100,000)") + 
     xlab("Month")

#=================================================
 # EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================

# Create a csv file that can be visualized in Excel, Tableau, or any presentation software
#=========================================================================================

counts <- aggregate(Riders$ride_length ~ Riders$member_casual + Riders$day_of_week, FUN = mean)
write.csv(counts,"C:\\Users\\UDCL\\Documents\\Data Analytics Project Case Study\\avg_ride_length.csv")
