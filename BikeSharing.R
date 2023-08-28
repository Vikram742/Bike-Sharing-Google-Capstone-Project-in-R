library(tidyverse)
library(janitor)
library(lubridate)

#creating data frames for each month
df1<- read.csv("D:/Data Analytics/Google Analytics Case Study/Case Study 1/Cyclistic Trip Data/2021 Jan-Dec Data/202101-divvy-tripdata.csv")
df2<- read.csv("D:/Data Analytics/Google Analytics Case Study/Case Study 1/Cyclistic Trip Data/2021 Jan-Dec Data/202102-divvy-tripdata.csv")
df3<- read.csv("D:/Data Analytics/Google Analytics Case Study/Case Study 1/Cyclistic Trip Data/2021 Jan-Dec Data/202103-divvy-tripdata.csv")
df4<- read.csv("D:/Data Analytics/Google Analytics Case Study/Case Study 1/Cyclistic Trip Data/2021 Jan-Dec Data/202104-divvy-tripdata.csv")
df5<- read.csv("D:/Data Analytics/Google Analytics Case Study/Case Study 1/Cyclistic Trip Data/2021 Jan-Dec Data/202105-divvy-tripdata.csv")
df6<- read.csv("D:/Data Analytics/Google Analytics Case Study/Case Study 1/Cyclistic Trip Data/2021 Jan-Dec Data/202106-divvy-tripdata.csv")
df7<- read.csv("D:/Data Analytics/Google Analytics Case Study/Case Study 1/Cyclistic Trip Data/2021 Jan-Dec Data/202107-divvy-tripdata.csv")
df8<- read.csv("D:/Data Analytics/Google Analytics Case Study/Case Study 1/Cyclistic Trip Data/2021 Jan-Dec Data/202108-divvy-tripdata.csv")
df9<- read.csv("D:/Data Analytics/Google Analytics Case Study/Case Study 1/Cyclistic Trip Data/2021 Jan-Dec Data/202109-divvy-tripdata.csv")
df10<- read.csv("D:/Data Analytics/Google Analytics Case Study/Case Study 1/Cyclistic Trip Data/2021 Jan-Dec Data/202110-divvy-tripdata.csv")
df11<- read.csv("D:/Data Analytics/Google Analytics Case Study/Case Study 1/Cyclistic Trip Data/2021 Jan-Dec Data/202111-divvy-tripdata.csv")
df12<- read.csv("D:/Data Analytics/Google Analytics Case Study/Case Study 1/Cyclistic Trip Data/2021 Jan-Dec Data/202112-divvy-tripdata.csv")
# combine all the data frames into one

bike_rides<- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
dim(bike_rides)
glimpse(bike_rides)

#convert column names "started_at" and "ended_at" to date time format as they
# are originally shown as character type

bike_rides$started_at<- lubridate::ymd_hms(bike_rides$started_at)
bike_rides$ended_at<- lubridate::ymd_hms(bike_rides$ended_at)


class(bike_rides$started_at)
class(bike_rides$ended_at)


#removing empty rows
bike_rides<-na.omit(bike_rides)
duplicated(bike_rides$ride_id)
which(duplicated(bike_rides))

#create columns for trip_duration,
bike_rides$trip_duration<- difftime(bike_rides$ended_at,bike_rides$started_at)

# ensure that we want to work with trip duration that are not less than 0
bike_rides<- bike_rides %>% 
  filter(trip_duration>=0)

#check data type for column trip_duration & convert to numeric

class(bike_rides$trip_duration)

# convert trip_duration to numeric type
bike_rides$trip_duration<- as.numeric(as.character(bike_rides$trip_duration))
is.numeric(bike_rides$trip_duration)

# create column date, month, day_of_week, day, year
bike_rides$date <- as.Date(bike_rides$started_at)
bike_rides$month<- format(as.Date(bike_rides$date),"%B")
bike_rides$year<-format(as.Date(bike_rides$date),"%Y")
bike_rides$day<-format(as.Date(bike_rides$date),"%d")
bike_rides$day_of_week<-format(as.Date(bike_rides$date),"%A")

#run few calculations to find out min,max,median,mean trip duration
# but it makes more sense to find out the above for member and casual
bike_rides %>% 
  group_by(member_casual) %>% 
summarise(average_trip_duration = mean(trip_duration),
          min_trip_duration = min(trip_duration),
          max_trip_duration = max(trip_duration),
          median_trip_duration = median(trip_duration))
  
# find out the number of rides taken by member and casual

bike_rides %>% 
  group_by(member_casual) %>% 
summarise(ride_count = length(ride_id)) 

# plot the comparison via bar chart
bike_rides %>% 
  ggplot(aes(x = member_casual, fill = member_casual))+
  geom_bar()+
  labs(title = 'Comparison between casual and member by total number of rides',
       x = 'Casuals vs Members', y = 'Number of Rides')+
  scale_y_continuous(labels = scales::comma)

# find out the average trip duration on the day of the week between member & casual
# we need to fix the order of the day of the week

bike_rides$day_of_week <- ordered(bike_rides$day_of_week, 
                                      levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
bike_rides %>% 
  group_by(member_casual,day_of_week) %>% 
  summarise(number_of_rides = n(),average_trip_duration = mean(trip_duration),.groups = "drop") %>% 
  arrange(member_casual,day_of_week)
  
#visualize the day of week rides between casual and member

bike_rides %>% 
  group_by(member_casual,day_of_week) %>% 
  summarise(number_of_rides = n(),.groups = "drop") %>% 
  arrange(member_casual,day_of_week) %>% 
  ggplot(aes(x= day_of_week , y= number_of_rides, fill = member_casual))+
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  labs(title = "Total Rides of Casual and Member vs Day of the Week ",
       x = "Day of The Week" , y = "Number of Rides")+
  theme_bw()+
  scale_y_continuous(labels = scales::comma)
  
bike_rides %>% 
  group_by(member_casual,day_of_week) %>% 
  summarise(average_trip_duration = mean(trip_duration),.groups = "drop") %>% 
  ggplot(aes(x= day_of_week , y= average_trip_duration, fill = member_casual))+
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  labs(title = "Avg Trip Duration of Casual and Member vs Day of the Week ",
       x = "Day of The Week" , y ="Avg Trip Duration" )+
  theme_bw()+
  scale_y_continuous(labels = scales::comma)
  
# find out the number of trips and average trip duration between casual and
#member during each month

bike_rides$month<- ordered(bike_rides$month,levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) 
  
bike_rides %>% 
  group_by(member_casual,month) %>% 
  summarise(number_of_rides = n(),.groups = "drop") %>% 
  arrange(member_casual,month) %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual))+
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  labs(title = "Total Rides of Casual and Member vs Month ",
       x = "Month" , y ="Number of Rides" ) +
  theme(axis.text.x = element_text(angle = 30))+
scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

  bike_rides %>% 
    group_by(member_casual,month) %>% 
    summarise(average_trip_duration = mean(trip_duration),.groups = "drop") %>% 
    ggplot(aes(x = month, y = average_trip_duration, fill = member_casual))+
    geom_col(width=0.5, position = position_dodge(width=0.5)) +
    labs(title = "Avg Trip Duration of Casual and Member vs Month ",
         x = "Month" , y ="Avg Trip Duration" ) +
    theme(axis.text.x = element_text(angle = 30))
    scale_y_continuous(labels = scales::comma)
  
  