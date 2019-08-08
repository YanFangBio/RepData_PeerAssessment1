---
title: "PA1_template"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

This assignment makes use of data from a personal activity monitoring device. 

## 1. Load the data and show the top 6 rows:

```{r echo = TRUE}
data = read.csv("activity 2.csv")
library(lubridate)
data$date <- ymd(data$date)
head(data)
```

## 2. What is mean total number of steps taken per day?
Make a histogram of the total number of steps taken each day:
```{r echo=TRUE}
sumdata = c()
sumdata$date = unique(data$date)
sumdata$steps = c()
sumdata = as.data.frame(sumdata)
for (i in 1: nrow(sumdata)) {
  sumdata[i, "steps"] = sum(data[data$date == sumdata[i, "date"], "steps"], na.rm = TRUE)
}
hist(sumdata$steps, xlab = "Sum of steps each day", main = "The total number of steps taken each day")

```
 
Calculate and report the mean and median total number of steps taken per day:
```{r echo=TRUE}
mean_total = mean(sumdata$steps)
median_total = median(sumdata$steps)
```
The mean total number of steps taken per day is `r mean_total`, and the median total number of steps taken per day is `r median_total`.

## 3. What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r echo=TRUE}
interval_data = c()
interval_data$interval = unique(data$interval)
interval_data$steps = c()
interval_data = as.data.frame(interval_data)
for (i in 1: nrow(interval_data)) {
  interval_data[i, "steps"] = mean(data[data$interval == interval_data[i, "interval"], "steps"], na.rm = TRUE)
}
plot(interval_data$interval, interval_data$steps, type = "l")
```
  
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r echo=TRUE}
max_interval = interval_data[which.max(interval_data$steps),"interval"]
```
The `r max_interval` interval contains the maximum number of step.

## 4. Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset:

```{r}
NA_num = sum(is.na(data$steps))
```
The total number of rows with NAs is `r NA_num`.

Create a new dataset that is equal to the original dataset but with the missing data filled in by the the mean for that 5-minute interval.
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
newdata = data
for (i in 1:nrow(data)) {
  tmp = data[i, "steps"]
  if(is.na(tmp) == TRUE){
    t.interval = data[i, "interval"]
    mean_step = interval_data[interval_data$interval == t.interval, "steps"]
    newdata[i, "steps"] = mean_step
  }
}
newsumdata = c()
newsumdata$date = unique(data$date)
newsumdata$steps = c()
newsumdata = as.data.frame(newsumdata)
for (i in 1: nrow(newsumdata)) {
  newsumdata[i, "steps"] = sum(data[data$date == newsumdata[i, "date"], "steps"], na.rm = TRUE)
}
hist(newsumdata$steps, xlab = "Sum of steps each day", main = "The new total number of steps taken each day")

newmean_total = mean(newsumdata$steps)
newmedian_total = median(newsumdata$steps)
```

The mean total number of steps taken per day is `r newmean_total`, and the median total number of steps taken per day is `r newmedian_total`. These two value does not change. Thus, the imputing missing data does not influence the estimates of the total daily number of steps.

## 5. Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

```{r}
newdata$week = weekdays(newdata$date, abbreviate = F)
newdata$week[newdata$week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")] = "weekday"
newdata$week[newdata$week %in% c("Saturday", "Sunday")] = "weekend"
weekday = subset(newdata, week == "weekday")
weekend = subset(newdata, week == "weekend")
weekday_interval = c()
weekday_interval$interval = unique(weekday$interval)
weekday_interval$steps = c()
weekday_interval = as.data.frame(weekday_interval)
for (i in 1: nrow(weekday_interval)) {
  weekday_interval[i, "steps"] = mean(weekday[weekday$interval == weekday_interval[i, "interval"], "steps"], na.rm = TRUE)
}
weekend_interval = c()
weekend_interval$interval = unique(weekend$interval)
weekend_interval$steps = c()
weekend_interval = as.data.frame(weekend_interval)
for (i in 1: nrow(weekend_interval)) {
  weekend_interval[i, "steps"] = mean(weekend[weekend$interval == weekend_interval[i, "interval"], "steps"], na.rm = TRUE)
}
par(mar = c(4,4,2,2))
par(mfrow = c(2,1))
plot(weekday_interval$interval, weekday_interval$steps, type = "l", main = "weekday", xlab = "interval", ylab = "average steps")
plot(weekend_interval$interval, weekend_interval$steps, type = "l", main = "weekend", xlab = "interval", ylab = "average steps")
```


