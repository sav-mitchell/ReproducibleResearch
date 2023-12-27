##Loading the data into R
unzip("activity.zip")
activity_data <- read.csv("activity.csv")

##Loading necessary packages
library(ggplot2)
library(dplyr)
library (lubridate)

###Answering: What is the mean total steps taken each day?

##Calculating the total steps per day
steps_total <- activity_data %>%
  group_by(date) %>%
  summarise(daily_steps = sum(steps, na.rm = TRUE))

##Creating a histogram for total steps per day
ggplot(steps_total, aes(daily_steps)) + geom_histogram(binwidth = 2000) +
  xlab("Total Steps") + 
  ylab("Frequency")

##Calculating the mean of total steps
mean_steps = mean(steps_total$daily_steps, na.rm=TRUE)
median_steps = median(steps_total$daily_steps, na.rm=TRUE)

###Answering: What is the average daily activity pattern?

## Creating a Histogram for Total Steps Per Day
interval_steps <- activity_data %>% 
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm =TRUE))

ggplot(data=interval_steps, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("Five Minute Intervals") +
  ylab("Steps Taken per Day Average")

###Imputing Missing Values

##Calculating and Reporting Total Missing Values 
missing_values <- !complete.cases(activity_data)

##Filling Missing Values in the Dataset
imputed_data <- activity_data %>%
  mutate(
    steps = case_when(
      is.na(steps) ~ interval_steps$steps[match(activity_data$interval, interval_steps$interval)],      
      TRUE ~ as.numeric(steps)
    ))

##Creating a Histogram for Daily Steps Using Imputed Data
imputed_total <- imputed_data %>% group_by(date) %>% summarise(daily_steps = sum(steps))

ggplot(imputed_total, aes(daily_steps)) + 
  geom_histogram(binwidth = 2000) + 
  xlab("Steps Taken per Day Total") + 
  ylab("Frequency")

##Calculating the Mean and Median for the Imputed Data
imputed_mean = mean(imputed_total$daily_steps, na.rm=TRUE)
imputed_median = median(imputed_total$daily_steps, na.rm=TRUE)

##Calculating the Difference Between the Original and Imputed Data Steps
mean_difference <- mean_steps - imputed_mean
median_difference <- median_steps - imputed_median

###Answering: Is there a Difference Between Weekday and Weekend Activity 

##Calculating the Difference Between Weekday and Weekend Activity
day <- imputed_data %>%
  mutate(
    date = ymd(date),
    weekday_or_weekend = case_when(wday(date) %in% 2:6 ~ "Weekday",
                                   wday(date) %in% c(1,7) ~ "Weekend")
  ) %>% select(-date) %>%
  group_by(interval, weekday_or_weekend) %>%
  summarise(
    steps = mean(steps)
  )

##Creating a Histogram for the Activity Difference
ggplot(day, aes(interval, steps)) + 
  geom_line() + 
  facet_wrap(~weekday_or_weekend, nrow = 2) +
  xlab("Five Minute Intervals") + 
  ylab("Step Average")