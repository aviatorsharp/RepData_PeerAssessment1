# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

### Code for reading in the dataset and/or processing the data



```r
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
setwd("C:/Users/Ian/Desktop/DataScience")
activity <- read.csv("./data/activity.csv")
daily <-
        activity %>%
        group_by(date) %>%
        summarise(steps = sum(steps))
timeofday <-
        activity[!is.na(activity[,1]),] %>%
        group_by(interval) %>%
        summarise(steps = mean(steps))
```

### Histogram of the total number of steps taken each day



```r
library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.2
```

```r
ggplot(daily, aes(steps)) + 
        geom_histogram(breaks = seq(0, 22000, by = 2000), na.rm = TRUE, 
                       fill = heat.colors(11), col = "grey52") +
        labs(x = "Total Number of Steps", y = "Number of Days", 
             title = "Total Number of Steps Taken Per Day")
```

![](PA1_template_files/figure-html/hist-1.png)<!-- -->


## What is mean total number of steps taken per day?


```r
summary(daily$steps)[3:4]
```

```
## Median   Mean 
##  10760  10770
```

## What is the average daily activity pattern?

### Time series plot of the average number of steps taken



```r
ggplot(timeofday, aes(interval, steps)) + 
        geom_line(lwd = 1, col = "chartreuse4") +
        labs(x = "Interval Corresponding to Time of Day", y = "Total Number of Steps",
             title = "Number of Steps per Time of Day") +
        coord_cartesian(xlim = c(0,2400))
```

![](PA1_template_files/figure-html/intervalsteps-1.png)<!-- -->

### The 5-minute interval that, on average, contains the maximum number of steps



```r
as.double(timeofday[which(timeofday$steps == max(timeofday$steps)),1])
```

```
## [1] 835
```

## Imputing missing values

### Code to describe and show a strategy for imputing missing data



```r
sum(is.na(activity))
```

```
## [1] 2304
```

```r
#NA values replaced by the mean number of steps for that particular interval
activity2 <- activity
activity2$steps <- replace(activity$steps, is.na(activity$steps), timeofday$steps)
sum(is.na(activity2))
```

```
## [1] 0
```

### Histogram of the total number of steps taken each day after missing values are imputed



```r
daily2 <-
        activity2 %>%
        group_by(date) %>%
        summarise(steps = sum(steps))
ggplot(daily2, aes(steps)) + 
        geom_histogram(breaks = seq(0, 22000, by = 2000), na.rm = TRUE, 
                       fill = heat.colors(11), col = "grey52") +
        labs(x = "Total Number of Steps", y = "Number of Days", 
             title = "Total Number of Steps Taken Per Day")
```

![](PA1_template_files/figure-html/hist2-1.png)<!-- -->

### Mean and Median with and without NAs



```r
rbind("With NAs" = summary(daily$steps)[3:4], 
      "Without NAs" = summary(daily2$steps)[3:4])
```

```
##             Median  Mean
## With NAs     10760 10770
## Without NAs  10770 10770
```

## Are there differences in activity patterns between weekdays and weekends?


```r
activity2$date <- strptime(as.character(activity2$date), format = "%Y-%m-%d")
activity2$weekday <- weekdays(activity2$date)
weekdaylogical <- activity2$weekday == "Saturday" | activity2$weekday == "Sunday"
activity2$weekday <- replace(activity2$weekday, weekdaylogical, "weekend")
activity2$weekday <- replace(activity2$weekday, !weekdaylogical, "weekday")
activity2$weekday <- as.factor(activity2$weekday)
activity2$date <- as.Date(activity2$date)
activity3 <-
        activity2 %>%
        group_by(weekday, interval) %>%
        summarise(steps = mean(steps))
ggplot(activity3, aes(interval, steps)) +
        geom_line(col = "chartreuse4", lwd = 0.6) + 
        facet_grid(weekday~.)
```

![](PA1_template_files/figure-html/weekday-1.png)<!-- -->
