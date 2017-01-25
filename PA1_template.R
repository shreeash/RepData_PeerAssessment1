#Data loading 
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "data.zip")
fitdata <- unzip("data.zip")
fit <- read.csv(fitdata, header = T, na.strings = "NA")
fit$date <- as.Date(fit$date)
fit$steps <- as.numeric(as.character(fit$steps))

#Unique day will occupy only one row and the corresponding step values will be added up
library(dplyr)
fit <- fit %>% group_by(date) %>% summarise(steps = sum(steps))

#Plot total number of steps taken each day
barplot(fit$steps, names.arg = fit$date, main = "Total no. of Steps per Day", xlab = "Days", ylab = "Steps", col="red")

#mean and median of number of steps taken each day
mean(fit$steps, na.rm = T)
median(fit$steps, na.rm = T)

#Reload original data
fit <- read.csv(fitdata, header = T, na.strings = "NA")
fit$date <- as.Date(fit$date)
fit$steps <- as.numeric(as.character(fit$steps))
fit$interval <- as.numeric(as.character(fit$interval))

#Unique interval number will occupy only one row and the corresponding steps value will be added up across all days
fit <- na.omit(fit)
library(dplyr)
fit <- fit %>% group_by(interval) %>% summarise(steps = mean(steps))

#Generate the line plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
plot(fit$interval, fit$steps, type = 'l', col = "blue", main="Average number of steps taken across all days", xlab="Interval", 
     ylab="Average number of steps taken")


# The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps
fit[which.max(fit$steps), ]

#Reload original data
fit <- read.csv(fitdata, header = T, na.strings = "NA")
fit$date <- as.Date(fit$date)
fit$steps <- as.numeric(as.character(fit$steps))
fit$interval <- as.numeric(as.character(fit$interval))

#Number of missing values in the dataset
sum(is.na(fit))

#Number of missing values in each column
sum(is.na(fit$steps))
sum(is.na(fit$date))
sum(is.na(fit$interval))


#Imputing missing values for steps with mean by intervals
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

#Create a new dataset that is equal to the original dataset but with the missing data filled in
fitimp <- fit %>%  group_by(interval) %>% mutate(steps = impute.mean(steps))

#Plot total number of steps taken each day after imputing
library(dplyr)
fitimp <- fitimp %>% group_by(date) %>% summarise(steps = sum(steps))

#histogram of the total number of steps taken each day after imputing
barplot(fitimp$steps, names.arg = fitimp$date, main = "Total no. of Steps per Day(imp)", xlab = "Days", ylab = "Steps", col="red")

#mean and median of number of steps taken each day after imputing
mean(fitimp$steps, na.rm = T)
median(fitimp$steps, na.rm = T)

#Reload original data
fit <- read.csv(fitdata, header = T, na.strings = "NA")
fit$date <- as.Date(fit$date)
fit$steps <- as.numeric(as.character(fit$steps))
fit$interval <- as.numeric(as.character(fit$interval))

#Create a factor variable to indicate the weekday for the dates
fit$day <- weekdays(as.Date(fit$date))
fitweekday <- fit[!fit$day == c("Saturday", "Sunday"),]
fitweekend <- fit[fit$day == c("Saturday", "Sunday"),]

fitweekday <- na.omit(fitweekday)
fitweekend <- na.omit(fitweekend)
library(dplyr)
fitweekday <- fitweekday %>% group_by(interval) %>% summarise(steps = mean(steps))
fitweekend <- fitweekend %>% group_by(interval) %>% summarise(steps = mean(steps))

#Plot timeseries to compare activty between weekday and weekend
par(mfrow = c(1,2))
plot(fitweekday$interval, fitweekday$steps, type = 'l', col = "green", main="Average number of steps taken across all Weekdays", xlab="Interval", 
     ylab="Avg number of steps taken during Weekdays")
plot(fitweekend$interval, fitweekend$steps, type = 'l', col = "red", main="Average number of steps taken across all Weekends", xlab="Interval", 
     ylab="Average number of steps taken during Weekends")
