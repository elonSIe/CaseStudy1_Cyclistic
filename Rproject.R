# Required packages :
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# scales for visualization terms

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

#Sys.setlocale(category = "LC_ALL", locale = "english")
#Sys.setlocale(locale = "ru_RU.utf8")
#sessionInfo()

#=====================
# STEP 1: IMPORT ALL DATA
#=====================

# Upload all data
q2_2019 <- read.csv("q2_2019.csv")
q3_2019 <- read.csv("q3_2019.csv")
q4_2019 <- read.csv("q4_2019.csv")
q1_2020 <- read.csv("q1_2020.csv")



#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================

# Compare column names each of the files
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file

# Rename columns  to make them consistent with q1_2020 (as this will be the supposed going-forward table format)
q2_2019 <- rename(q2_2019
                  ,ride_id = "X01...Rental.Details.Rental.ID"
                  ,rideable_type = "X01...Rental.Details.Bike.ID"
                  ,started_at = "X01...Rental.Details.Local.Start.Time"
                  ,ended_at = "X01...Rental.Details.Local.End.Time"
                  ,start_station_name = "X03...Rental.Start.Station.Name"
                  ,start_station_id = "X03...Rental.Start.Station.ID"
                  ,end_station_name = "X02...Rental.End.Station.Name"
                  ,end_station_id = "X02...Rental.End.Station.ID"
                  ,member_casual = "User.Type")

q3_2019 <- rename(q3_2019
                  ,ride_id = trip_id
                  ,rideable_type = bikeid
                  ,started_at = start_time
                  ,ended_at = end_time
                  ,start_station_name = from_station_name
                  ,start_station_id = from_station_id
                  ,end_station_name = to_station_name
                  ,end_station_id = to_station_id
                  ,member_casual = usertype)

q4_2019 <- rename(q4_2019
                  ,ride_id = trip_id
                  ,rideable_type = bikeid 
                  ,started_at = start_time  
                  ,ended_at = end_time  
                  ,start_station_name = from_station_name 
                  ,start_station_id = from_station_id 
                  ,end_station_name = to_station_name 
                  ,end_station_id = to_station_id 
                  ,member_casual = usertype)

# Inspect the dataframes and look for incongruencies
str(q2_2019)
str(q3_2019)
str(q4_2019)
str(q1_2020)

# Convert ride_id and rideable_type to character so that they can stack correctly
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))

# Main action in this step :
# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

# Remove lat, long, birthyea, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng
            , birthyear, gender, "X01...Rental.Details.Duration.In.Seconds.Uncapped"
            , "X05...Member.Details.Member.Birthday.Year", "Member.Gender", "tripduration"))



#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================

# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
'''
1. In the "member_casual" column, there are two names for members
("member" and "Subscriber") and two names for casual riders ("Customer"
and "casual"). We will need to consolidate that from four to two labels.
2. The data can only be aggregated at the ride-level, which is too granular.
We will want to add some additional columns of data -- such as day, month,
year -- that provide additional opportunities to aggregate the data.
3. We will want to add a calculated field for length of ride since
the 2020Q1 data did not have the "tripduration" column.
We will add "ride_length" to the entire dataframe for consistency.
4. There are some rides where tripduration shows up as negative, including
several hundred rides where Divvy took bikes out of circulation
for Quality Control reasons. We will want to delete these rides.
'''

#1
# Begin by seeing how many observations fall under each user type
table(all_trips$member_casual)
# Before 2020, different labels for these two types of riders as below. Make our data-frame consistent with their current nomenclature
# In the "member_casual" column, replace "Subscriber" -> "member" and "Customer" -> "casual"
# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level

# Reassign to the desired values (we will go with the current 2020 labels)
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)
# All is changed perfectly with just two labels; 'casual', 'member'

#2
# Add columns that list the date, month, day, and year of each ride
# Aggregate ride data for each month/day/year ... since we could only aggregate at the ride level
all_trips$date <- as.Date(all_trips$started_at) # The default format : yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#3
# Add a "ride_length" calculation to all_trips
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#4
# Remove "bad" data
# The data-frame includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# Create a new version of the data-frame version 2 since data is being removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" 
                            | all_trips$ride_length < 0),]
str(all_trips_v2)

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

#A
# Descriptive analysis on ride_length
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

#B
# Condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual
          + all_trips_v2$day_of_week, FUN = mean)
# The days of the week are out of order

# Fix the days of the week in order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week
                                    ,levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Run the average ride time by each day for members vs casual users again to check
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual
          + all_trips_v2$day_of_week, FUN = mean)

# Analyze ridership data by user_type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by user_type and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# "Total Number of Rides by Day"
# Add visualization of the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  scale_y_continuous(labels = comma) +
  geom_col(position = "dodge") +
  labs(title = "Total Number of Rides by Day"
       ,x = "Weekday", y = "Number of Rides", fill = "Membership") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# "Average Duration by Day"
# Create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Duration by Day"
       ,x = "Weekday", y = "Average Duration", fill = "Membership") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#---------------------------------------------------------------------------

# Compare usage patterns between weekdays and weekends
rides_casual_weekday <- NROW(filter(all_trips_v2, member_casual == "casual" 
                                    & !(day_of_week == "Saturday" | day_of_week == "Sunday")))
rides_casual_weekend <- NROW(filter(all_trips_v2, member_casual == "casual" 
                                    & (day_of_week == "Saturday" | day_of_week == "Sunday")))
rides_casual_weekday
rides_casual_weekend
# Result : [1] 511346 [1] 390836

# Percentage of casual riders on weekdays and weekends
weekly <- c("Monday-Friday", "Saturday-Sunday")
weekly_casual_ride <- c(rides_casual_weekday, rides_casual_weekend)
percentage <- round(100 * weekly_casual_ride / sum(weekly_casual_ride), 1)
lbls <- paste(weekly, percentage)
lbls_casual_weekly <- paste(lbls, "%", sep="")
lbls_casual_weekly
# Result : [1] "Monday-Friday 56.7%"  [2] "Saturday-Sunday 43.3%"

# "Total Number of Rides by Month"
# Analyze ridership data by user_type and each month
all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)	%>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  scale_y_continuous(labels = comma) +
  geom_col(position = "dodge") +
  labs(title="Total Number of Rides by Month"
       ,x = "Month", y = "Number of Rides", fill = "Membership") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# "Total Number of Rideable Type Rides by Month"
# Analyze riderable type data by type and month
all_trips_v2 %>% 
  group_by(rideable_type, month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 		
  arrange(rideable_type, month)	%>% 
  ggplot(aes(x = month, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge") + 
  labs(title="Total Number of Member Rides by Month"
       ,x = "Month", y = "Number of Member Rides", fill = "Rideable Type") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# "Total Number of Rideable Type Rides by Day"
# Analyze riderable type data by type and month
all_trips_v2 %>% 
  group_by(rideable_type, day) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 		
  arrange(rideable_type, month)	%>% 
  ggplot(aes(x = month, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge") + 
  labs(title="Total Number of Member Rides by Day"
       ,x = "Month", y = "Number of Member Rides", fill = "Rideable Type") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



'''
#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel or Tableau
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/Desktop/Divvy_Exercise/avg_ride_length.csv')
'''
