#Install packages and set directory
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
install.packages("plyr")
library(plyr)
library(dplyr)
getwd()
setwd("/Users/ashafil/Documents/Data_Analytics/Case_Studies/Case_Study_1/Cycling_RawCSV_files")

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload data
April_2020 <- read.csv("202004-divvy-tripdata.csv")
May_2020 <-  read.csv("202005-divvy-tripdata.csv")
June_2020 <-  read.csv("202006-divvy-tripdata.csv")
July_2020 <-  read.csv("202007-divvy-tripdata.csv")
August_2020 <-  read.csv("202008-divvy-tripdata.csv")
September_2020 <-  read.csv("202009-divvy-tripdata.csv")
October_2020 <-  read.csv("202010-divvy-tripdata.csv")
November_2020 <-  read.csv("202011-divvy-tripdata.csv")
December_2020 <-  read.csv("202012-divvy-tripdata.csv")
January_2021 <-  read.csv("202101-divvy-tripdata.csv")
February_2021 <-  read.csv("202102-divvy-tripdata.csv")
March_2021 <-  read.csv("202103-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names because we want to join them into one file later and need to make sure they're all the same. Can look at them individually.
colnames(April_2020)
colnames(May_2020)
colnames(June_2020)
colnames(July_2020)
colnames(August_2020)
colnames(September_2020)
colnames(October_2020)
colnames(November_2020)
colnames(December_2020)
colnames(January_2021)
colnames(February_2021)
colnames(March_2021)

# Faster way of checking if column names are the same is by using a lapply and then length(unique)
dataImport_list <- list(April_2020, May_2020, June_2020, July_2020, August_2020, September_2020,
                        October_2020, November_2020, December_2020, January_2021,
                        February_2021, March_2021)
column_names <- lapply(dataImport_list, colnames)

View(column_names)

length(unique(column_names)) == 1

# Column names were the same. If they were not, we would have to rename them using the 'rename' function

# Examine the structure of the dataframes to ensure they are the same
janitor::compare_df_cols(dataImport_list)
# Or view individually
str(April_2020)
str(May_2020)
str(June_2020)
str(July_2020)
str(August_2020)
str(September_2020)
str(October_2020)
str(November_2020)
str(December_2020)
str(January_2021)
str(February_2021)
str(March_2021)
# Or view at the same time in different format
for(i in dataImport_list) {
  str(i)
}
# Some data frames have 'start_station_id' and 'end_station_id' as 'int' when they should be 'chr' since some data frame IDs have numbers and letters. 
# Going to convert them to 'chr' so that they can stack correctly. 
April_2020 <- mutate(April_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
May_2020 <- mutate(May_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
June_2020 <- mutate(June_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
July_2020 <- mutate(July_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
August_2020 <- mutate(August_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
September_2020 <- mutate(September_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
October_2020 <- mutate(October_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
November_2020 <- mutate(November_2020, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

# Stack the individual data frames into one large data frame
all_trips <- bind_rows(April_2020, May_2020, June_2020, July_2020, August_2020, September_2020,
                       October_2020, November_2020, December_2020, January_2021,
                       February_2021, March_2021)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame
tail(all_trips)#See the last 6 rows of data frame
str(all_trips)  #See list of columns and data types (numeric, character, etc)

# Make sure that there are only two categories of customers
unique(all_trips$member_casual) 
# or
table(all_trips$member_casual)

### *Note* If there was more than two categories of customers (for example had 'subscriber' instead of 'member'), we would reassign the values by:
# alltrips <- all_trips %>% 
#mutate(member_casual = recode(member_casual
#                              ,"subscriber" - "member"))

# Want to split up day, month, and year of the ride 
# https://www.statmethods.net/input/dates.html 
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Want to calculate the length of the ride (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns to make sure the length of ride is numeric and not a factor
str(all_trips)
is.factor(all_trips$ride_length)

# Make sure there are no negative values for the ride length (when bikes were taken out of docks and checked for quality)
count(all_trips$ride_length < 0)

# There are some negative values so we will create a new data frame to not include the ones that are negative
all_trips_v2 <- all_trips[!(all_trips$ride_length < 0),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# Compare members and casual users
# Mean
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
# Median
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
# Maximum
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
# Minimum
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# The days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  dplyr::summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% # calculates the number of rides and average duration and calculates the average duration
  arrange(member_casual, weekday)			# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  dplyr::summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  dplyr::summarise(number_of_rides = n()
            ,average_duration = mean(ride_length/60)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + xlab("Weekday") + ylab("Average Duration (minutes)")

# Let's create a visualization for number of rides each month 
ggplot(all_trips_v2) +
  geom_bar(mapping = aes(x = month, fill = member_casual), position = "dodge", width = 0.6) + 
  xlab("Month") + ylab("Number of Rides")

# Let's create a visualization for average duration for each month 
all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  dplyr::summarise(number_of_rides = n()
                   ,average_duration = mean(ride_length/60)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.65) + xlab("Month") + ylab("Average Duration (minutes)")

## Look if type of bike has influence
ggplot(all_trips_v2) +
  geom_bar(mapping = aes(x = rideable_type, fill = member_casual), position = "dodge", width = 0.6) + 
  xlab("Type of Bike") + ylab("Number of Rides")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will use to visualize in Tableau
#First create data set that only has member type, day of the week, and ride length 
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '/Users/ashafil/Documents/Data_Analytics/Case_Studies/Case_Study_1/avg_ride_length.csv')

# Then create data set that has ride type, member type, the latitude and longitude of ride 
#start and end, day of week, and ride length

condensed <- all_trips_v2 %>% 
  select(rideable_type, start_lat, start_lng, end_lat, end_lng, member_casual, day_of_week, ride_length)
write.csv(condensed, file = '/Users/ashafil/Documents/Data_Analytics/Case_Studies/Case_Study_1/Trips_condensed.csv')

# Can also have all of the data exported, but this creates a large file

write.csv(all_trips_v2, file = '/Users/ashafil/Documents/Data_Analytics/Case_Studies/Case_Study_1/Trips_condensed.csv')





