# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

1. Load the data

```r
activity <- read.csv("activity.csv")
```

2. Process/transform the data to convert date

```r
activity$date <- as.Date(activity$date)
```

Load required libraries

```r
library(ggplot2)
library(lattice)
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
total_steps_perday <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
```

2. Make a histogram of the total number of steps taken each day

```r
hist(total_steps_perday, main="Histogram - Total number of steps taken each day", xlab = "total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(total_steps_perday)
```

```
## [1] 9354.23
```


```r
median(total_steps_perday)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
ggplot(activity) +
  stat_summary(aes(x=interval, y=steps),
               fun.y = function(y) mean(y),
               geom = "line") +
  labs(x = "Avg steps taken") +
  labs(title = "Average Daily Activity")
```

```
## Warning: Removed 2304 rows containing non-finite values (stat_summary).
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
  
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
mean_steps_perinterval <- tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
mean_steps_perinterval[mean_steps_perinterval == max(mean_steps_perinterval)]
```

```
##      835 
## 206.1698
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset - Fill missing values with mean for that 5-minute interval

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_full <- activity
activity_na <- is.na(activity_full$steps)
activity_full[activity_na, "steps"] <- mean_steps_perinterval[as.character(activity_full[activity_na,"interval"])]
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
total_steps_perday2 <- tapply(activity_full$steps, activity_full$date, sum, na.rm=TRUE)
hist(total_steps_perday2, main="Histogram - Total number of steps taken each day", xlab = "total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
  

```r
mean(total_steps_perday2)
```

```
## [1] 10766.19
```


```r
median(total_steps_perday2)
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment?
Yes, they differ

What is the impact of imputing missing data on the estimates of the total daily number of steps?
The values are improved. Minor improvement in mean and more improvement in median.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
wd <- weekdays(activity_full$date)
activity_full$day_type <- sapply(wd, function(wd) if (wd=="Saturday" || wd == "Sunday") {"weekend"} else {"weekday"})
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
agg_activity <- aggregate(x = activity_full$steps, by = list(interval = activity_full$interval, day_type = activity_full$day_type), FUN = mean)
agg_activity$day_type <- factor(agg_activity$day_type)
xyplot(agg_activity$x ~ agg_activity$interval | agg_activity$day_type, layout = c(1,2), type="l",
       xlab = "interval", ylab = "avg steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

