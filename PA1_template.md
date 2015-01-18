---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

### Reproducible Research: Peer Assessment 1


```r
library(ggplot2)
library(lattice)
```

## Loading and preprocessing the data

Show any code that is needed to:

1. Load the data (i.e. read.csv())


```r
unzip("activity.zip")
data <- read.csv("activity.csv")
```

Just a feel of the dataset, to be able to decide the transformations:


```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
data$date <- as.Date(data$date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.


```r
data_full <- na.omit(data)
```

1. Make a histogram of the total number of steps taken each day


```r
steps_per_day <- aggregate(steps ~ date, data = data_full, FUN = sum)
ggplot(steps_per_day) + aes(x=steps) + geom_histogram(binwidth=1000) + labs(title="Histogram (Steps Per Day)")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```


```r
median(steps_per_day$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avg_steps <- aggregate(steps ~ interval, data = data_full, FUN = mean)
plot(avg_steps, type = "l", main="Interval Time Series", xlab = "interval", ylab = "steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
row_id <- which.max(avg_steps$steps)
avg_steps$interval[row_id]
```

```
## [1] 835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
length(which(is.na(data)))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
### We are going to to an interpolation from two intervals before: notice that positions 1 and 2 are the mean
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data_fill <- data
data_fill[1, ]$steps = mean(data_full$steps)
data_fill[2, ]$steps = mean(data_full$steps)
for (i in 3:nrow(data_fill)) {
    if (is.na(data_fill[i, ]$steps)) {
        data_fill[i, ]$steps <- (data_fill[i-2, ]$steps + data_fill[i-1, ]$steps)/2
    }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps_fill_per_day <- aggregate(steps ~ date, data = data_fill, FUN = mean)
hist(steps_fill_per_day$steps, main="Histogram FILL (Steps per Day)", xlab="Steps")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

We see that this strategy clearly increases the **mean** and the **median**. 


```r
mean(steps_fill_per_day$steps)
```

```
## [1] 33.09279
```


```r
median(steps_fill_per_day$steps)
```

```
## [1] 36.24653
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
data$day <- weekdays(data$date)                             
for (i in 1:nrow(data)) {
    if (data[i,]$day %in% c("Saturday","Sunday")) {
        data[i,]$day<-"weekend"
    }
    else{
        data[i,]$day<-"weekday"
    }
}
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
steps_weekdays = aggregate(steps ~ interval + day, data = data, FUN = mean)
xyplot(steps ~ interval | factor(day), data = steps_weekdays, aspect = 1/2, type = "l")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png) 

