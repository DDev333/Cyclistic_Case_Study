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

#Import Data
april_2020 <- read.csv("202004-divvy-tripdata.csv")
may_2020 <- read.csv("202005-divvy-tripdata.csv")
june_2020 <- read.csv("202006-divvy-tripdata.csv")
july_2020 <- read.csv("202007-divvy-tripdata.csv")
august_2020 <- read.csv("202008-divvy-tripdata.csv")
septempber_2020 <- read.csv("202009-divvy-tripdata.csv")
october_2020 <- read.csv("202010-divvy-tripdata.csv")
november_2020 <- read.csv("202011-divvy-tripdata.csv")
december_2020 <- read.csv("202012-divvy-tripdata.csv")
january_2021 <- read.csv("202101-divvy-tripdata.csv")
february_2021 <- read.csv("202102-divvy-tripdata.csv")
march_2021 <- read.csv("202103-divvy-tripdata.csv")

#Data Validation
colnames(april_2020)
colnames(may_2020)
colnames(june_2020)
colnames(july_2020)
colnames(august_2020)
colnames(septempber_2020)
colnames(october_2020)
colnames(november_2020)
colnames(december_2020)
colnames(january_2021)
colnames(february_2021)
colnames(march_2021)

#Total no.of rows
total_rows <- sum(nrow(april_2020) + nrow(may_2020) + nrow(june_2020) + 
      nrow(july_2020) + nrow(august_2020) + nrow(septempber_2020) + 
      nrow(october_2020) + nrow(november_2020) + nrow(december_2020) + 
      nrow(january_2021) + nrow(february_2021) + nrow(march_2021))
total_rows

#Combining data of 12 months into one for smooth workflow
final_data <- rbind(april_2020, may_2020, june_2020,
                    july_2020, august_2020, septempber_2020,
                    october_2020, november_2020, december_2020,
                    january_2021, february_2021, march_2021)

#Save the combined data
write.csv(final_data, file = "final_data.csv", row.names = FALSE)

#Setting global variable size to inf
options(future.globals.maxSize = Inf)

#Final data validation
str(final_data)
head(final_data)
tail(final_data)
dim(final_data)
summary(final_data)
names(final_data)

#Data cleaning

#Count rows with "na" values
colSums(is.na(final_data))

#Remove rows with missing values
clean_final_data <- final_data[complete.cases(final_data), ]

#Remove duplicates
clean_final_data <- distinct(clean_final_data)

#Remove data with greater started_at than ended_at
clean_final_data <- clean_final_data %>% 
  filter(started_at < ended_at)

#Remove na
clean_final_data <- drop_na(clean_final_data)
clean_final_data <- remove_empty(clean_final_data)
clean_final_data <- remove_missing(clean_final_data)

#Check cleaned data
colSums(is.na(clean_final_data))
View(filter(clean_final_data, clean_final_data$started_at > clean_final_data$ended_at))

#Renaming columns for better context
clean_final_data <- rename(clean_final_data, customer_type = member_casual, bike_type = rideable_type)

#Separate date in date, day, month, year for better analysis
clean_final_data$date <- as.Date(clean_final_data$started_at)
clean_final_data$week_day <- format(as.Date(clean_final_data$date), "%A")
clean_final_data$month <- format(as.Date(clean_final_data$date), "%b_%y")
clean_final_data$year <- format(clean_final_data$date, "%Y")

#Separate column for time
clean_final_data$time <- as.POSIXct(clean_final_data$started_at, format = "%Y-%m-%d %H:%M:%S")
clean_final_data$time <- format(clean_final_data$time, format = "%H:%M")

#Add ride length column
clean_final_data$ride_length <- difftime(clean_final_data$ended_at, clean_final_data$started_at, units = "mins")

#Select the data we are going to use
clean_final_data <- clean_final_data %>% 
  select(bike_type, customer_type, month, year, time, started_at, week_day, ride_length)

#Remove stolen bikes
clean_final_data <- clean_final_data[!clean_final_data$ride_length > 1440, ]
clean_final_data <- clean_final_data[!clean_final_data$ride_length < 5, ]

#Save the cleaned data
write.csv(clean_final_data, file = "clean_final_data.csv", row.names = FALSE)