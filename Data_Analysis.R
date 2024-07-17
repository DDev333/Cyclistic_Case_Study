#Install packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")
install.packages("data.table")
install.packages("readr")
install.packages("psych")
install.packages("hrbrthemes")
install.packages("ggplot2")

#Load packages
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(hrbrthemes)
library(ggplot2)

#Import cleaned data
clean_final_data <- read_csv("clean_final_data.csv")
str(clean_final_data)
names(clean_final_data)

#Order the data
clean_final_data$month <- ordered(clean_final_data$month, levels = c("Apr_20", "May_20", "Jun_20", "Jul_20",
                                                                     "Aug_20", "Sep_20", "Oct_20", "Nov_20",
                                                                     "Dec_20", "Jan_21", "Feb_21", "Mar_21"))

clean_final_data$week_day <- ordered(clean_final_data$week_day, levels = c("Sunday", "Monday", "Tuesday",
                                                                           "Wednesday", "Thursday",
                                                                           "Friday", "Saturday"))

#Analysis - min, max, median, average
View(describe(clean_final_data$ride_length, fast = TRUE))

#Total no.of customers
View(table(clean_final_data$customer_type))

#Total rides for each customer type in minutes
View(setNames(aggregate(ride_length ~ customer_type, clean_final_data, sum), c("customer_type", "total_ride_len(mins)")))

#Differences between members and casual riders in terms of ride length
View(clean_final_data %>% 
       group_by(customer_type) %>% 
       summarise(min_length_mins = min(ride_length), max_length_min = max(ride_length),
                 median_length_min = median(ride_length), mean_length_mins = mean(ride_length)))

#Average ride length for users by day of week and no.of toal rides by day of week
View(clean_final_data %>% 
       group_by(week_day) %>% 
       summarise(Avg_length = mean(ride_length),
                 number_of_ride = n()))

#Average ride length by month
View(clean_final_data %>% 
       group_by(month) %>% 
       summarise(Avg_length = mean(ride_length),
                 number_of_ride = n()))

#Average ride length comparison by each week day according to each customer type
View(aggregate(clean_final_data$ride_length ~ clean_final_data$customer_type + 
                 clean_final_data$week_day, FUN = mean))

#Analyze rider length data by customer type and weekday
View(clean_final_data %>% 
       group_by(customer_type, week_day) %>% 
       summarise(number_of_ride = n(),
                 average_duration = mean(ride_length),
                 median_duraion = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length)))

#Analyze rider length data by customer type and month
View(clean_final_data %>% 
       group_by(customer_type, month) %>% 
       summarise(number_of_ride = n(),
                 average_duration = mean(ride_length),
                 median_duraion = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length)))

#Save data for data visualization
write.csv(clean_final_data, file = "clean_final_data_tableau.csv", row.names = FALSE)