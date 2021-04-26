library(dplyr)
library(ggplot2)
library(knitr)
library(lubridate)
library(scales)

activity <- read.csv("activity.csv",header = TRUE, sep = ',')
activity$date <- ymd(activity$date)
steps <- activity %>% 
  filter(!is.na(steps)) %>%
  group_by(date) %>% 
  summarize(steps = sum(steps))

####Grafico no ggplot
#Code for reading in the dataset and/or processing the data
#Histogram of the total number of steps taken each day

ggplot(steps, aes(x = steps)) +
  geom_histogram(fill = "3", col=I("4"), binwidth = 1000) +
  labs(title = "Total Number of Steps Taken Each Day",
       x = "Steps per Day", 
       y = "Occurances")+theme_bw()


##Mean and median number of steps taken each day
mean <- round(mean(steps$steps, na.rm=TRUE))
median<- round(median(steps$steps, na.rm=TRUE))
paste("The Mean number of steps is",mean,"and the Median is",median)


activity$interval <- as.POSIXct(strptime(sprintf("%04d", activity$interval), "%H%M"))
interval <- activity %>% 
  filter(!is.na(steps)) %>% 
  group_by(interval) %>% 
  summarize(steps = mean(steps))

#Time series plot of the average number of steps taken
#The 5-minute interval that, on average, contains the maximum number of steps
ggplot(interval, aes(x=interval, y=steps)) +
  geom_line(color = "2") +
  scale_x_datetime(breaks = date_breaks("2 hour"), 
                   labels = date_format("%H:%M"),
                   limits = c(interval$Interval[1], interval$Interval[288])) +
  labs(title = "Average Number of Steps taken (Averaged Across All Days)", 
       x = "Time of Day", 
       y = "Average Steps")+theme_bw()

#Code to describe and show a strategy for imputing missing data

max.interval <- interval[which.max(interval$steps),]
paste("The interval with the maximum number of steps is",
      round(max.interval$steps[1]), "at",
      strftime(max.interval$interval[1], format="%H:%M:%S"))


missing.steps <- sum(is.na(activity$steps))
paste("The number of missing values in the dataset is",missing.steps)


tofill.activity <- activity
missing.activity <- is.na(tofill.activity$steps)
mean.interval <- tapply(tofill.activity$steps,
                        tofill.activity$interval, 
                        mean, na.rm=TRUE, simplify=TRUE)

tofill.activity$steps[missing.activity] <- mean.interval[as.character(tofill.activity$interval[missing.activity])]


#Histogram of the total number of steps taken each day after missing values are imputed

filled_steps <- tofill.activity %>%
  filter(!is.na(steps)) %>% 
  group_by(date) %>% 
  summarize(steps = sum(steps))
ggplot(filled_steps, aes(x = steps)) +
  geom_histogram(fill = "yellow3",colour="4",binwidth = 1000) +
  labs(title = "Total Number of Steps Each Day (missing values replaced)", 
       x = "Steps per day",
       y = "Occurances")+theme_bw()

#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
mean. <- round(mean(filled_steps$steps, na.rm=TRUE))
median.<- round(median(filled_steps$steps, na.rm=TRUE))
paste("The Mean number of steps is",mean.,"and the Median is",median.)



imputed <- mutate(tofill.activity, 
                  weektype = ifelse(weekdays(tofill.activity$date) == "Saturday" |
                                      weekdays(tofill.activity$date) == "Sunday", "Weekend", "Weekday"))
imputed$weektype <- as.factor(imputed$weektype)
imputed.full <- imputed %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps)) 
ggplot(imputed.full, aes(x=interval, y=steps, color = weektype)) +
  geom_line(color = "2") +
  facet_wrap(~weektype, ncol = 1, nrow=2)+
  scale_x_datetime(breaks = date_breaks("2 hour"),
                   labels = date_format("%H:%M"),
                   limits = c(imputed.full$Interval[1],
                              imputed.full$Interval[288])) +
  labs(title = "Average Number of Steps taken (Weekdays vs. Weekends)",
       x = "Time of Day", 
       y = "Average Steps")+theme_bw()
