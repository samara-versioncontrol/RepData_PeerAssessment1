---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

``` r
if(!file.exists("activity.csv")){
    unzip("activity.zip")
}
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, "%Y-%m-%d")


## What is mean total number of steps taken per day?

steps_per_day <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)

hist(steps_per_day$steps, 
     main = "Total Steps Taken per Day", 
     xlab = "Steps per Day", 
     col = "darkgreen", 
     breaks = 20)
```

![](PA1_template_files/figure-html/load_data-1.png)<!-- -->

``` r
mean_steps <- mean(steps_per_day$steps)
median_steps <- median(steps_per_day$steps)

## What is the average daily activity pattern?

steps_per_interval <- aggregate(steps ~ interval, activity, mean, na.rm = TRUE)

plot(steps_per_interval$interval, steps_per_interval$steps, 
     type = "l", 
     col = "blue",
     xlab = "5-Minute Interval", 
     ylab = "Average Number of Steps",
     main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/load_data-2.png)<!-- -->

``` r
max_interval <- steps_per_interval[which.max(steps_per_interval$steps), ]

## Imputing missing values

total_nas <- sum(is.na(activity$steps))

imputed_activity <- activity

for(i in 1:nrow(imputed_activity)) {
    if(is.na(imputed_activity$steps[i])) {
        interval_val <- imputed_activity$interval[i]
        replacement <- steps_per_interval[steps_per_interval$interval == interval_val, "steps"]
        imputed_activity$steps[i] <- replacement
    }
}

imputed_steps_per_day <- aggregate(steps ~ date, imputed_activity, sum)

hist(imputed_steps_per_day$steps, 
     main = "Total Steps Taken per Day (Imputed Data)", 
     xlab = "Steps per Day", 
     col = "purple", 
     breaks = 20)
```

![](PA1_template_files/figure-html/load_data-3.png)<!-- -->

``` r
mean_imputed <- mean(imputed_steps_per_day$steps)
median_imputed <- median(imputed_steps_per_day$steps)

## Are there differences in activity patterns between weekdays and weekends?

imputed_activity$day_type <- weekdays(imputed_activity$date)
imputed_activity$day_type <- ifelse(imputed_activity$day_type %in% c("Saturday", "Sunday"), 
                                    "weekend", "weekday")
imputed_activity$day_type <- as.factor(imputed_activity$day_type)

library(lattice)

weekday_weekend_summary <- aggregate(steps ~ interval + day_type, imputed_activity, mean)

xyplot(steps ~ interval | day_type, 
       data = weekday_weekend_summary, 
       type = "l", 
       layout = c(1, 2),
       xlab = "Interval", 
       ylab = "Number of steps",
       main = "Comparison of Activity: Weekdays vs. Weekends")
```

![](PA1_template_files/figure-html/load_data-4.png)<!-- -->
