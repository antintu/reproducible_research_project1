Introduction
============

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a **Fitbit**, **Nike Fuelband**, or **Jawbone Up**. These type of devices are part of the quantified self movement, a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

-   Dataset: Activity monitoring data \[52K\] (<https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>)

The variables included in this dataset are:

-   steps: Number of steps taking in a 5-minute interval (missing values are coded as ????????)
-   date: The date on which the measurement was taken in YYYY-MM-DD format
-   interval: Identifier for the 5-minute interval in which measurement was taken The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Loading and preprocessing the data
==================================

Set working directory, load packages and load file

``` r
setwd("~/Documents/Andrei/Werk/Cursusen/Data science Johns Hopkins/Files")
library(data.table)
library(ggplot2)
activity <- read.csv("/Users/andrei/Documents/Andrei/Werk/Cursusen/Data science Johns Hopkins/Files/activity.csv")
```

Read "activity" into a data table and do some basic stats

``` r
activity_total <- data.table::fread(input = "/Users/andrei/Documents/Andrei/Werk/Cursusen/Data science Johns Hopkins/Files/activity.csv")
head(activity_total)
```

    ##    steps       date interval
    ## 1:    NA 2012-10-01        0
    ## 2:    NA 2012-10-01        5
    ## 3:    NA 2012-10-01       10
    ## 4:    NA 2012-10-01       15
    ## 5:    NA 2012-10-01       20
    ## 6:    NA 2012-10-01       25

``` r
summary(activity_total)
```

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:17568       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0  
    ##  NA's   :2304

``` r
str(activity_total)
```

    ## Classes 'data.table' and 'data.frame':   17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

What is mean total number of steps taken per day?
=================================================

1.  Calculate (=sum) the total number of steps taken per day

``` r
daysteps_sum <- activity_total[, c(lapply(.SD, sum, na.rm = TRUE)), 
                               .SDcols = c("steps"), by = .(date)] 
head(daysteps_sum, 10)
```

    ##           date steps
    ##  1: 2012-10-01     0
    ##  2: 2012-10-02   126
    ##  3: 2012-10-03 11352
    ##  4: 2012-10-04 12116
    ##  5: 2012-10-05 13294
    ##  6: 2012-10-06 15420
    ##  7: 2012-10-07 11015
    ##  8: 2012-10-08     0
    ##  9: 2012-10-09 12811
    ## 10: 2012-10-10  9900

1.  Make histogram of total steps per day

``` r
g <- ggplot(daysteps_sum, aes(x = steps))
g <- g + geom_histogram(fill = "red", binwidth = 1000, show.legend = TRUE) 
g <- g + labs(title = "Daily Steps", x = "Total Steps", y = "Steps Frequency")
print(g)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

1.  Calculate and report the mean and median of the total number of steps taken per day

``` r
mean_steps <- daysteps_sum[, .(Mean_Steps = mean(steps, na.rm = TRUE))]
median_steps <- daysteps_sum[, .(Median_Steps = median(steps, na.rm = TRUE))]
summary_steps <- c(mean_steps, median_steps)
```

What is the average daily activity pattern?
===========================================

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
interval_activity <- activity_total[, c(lapply(.SD, mean, na.rm = TRUE)), 
                                    .SDcols = c("steps"), by = .(interval)]
head(interval_activity, 10)
```

    ##     interval     steps
    ##  1:        0 1.7169811
    ##  2:        5 0.3396226
    ##  3:       10 0.1320755
    ##  4:       15 0.1509434
    ##  5:       20 0.0754717
    ##  6:       25 2.0943396
    ##  7:       30 0.5283019
    ##  8:       35 0.8679245
    ##  9:       40 0.0000000
    ## 10:       45 1.4716981

``` r
g_i <- ggplot(interval_activity, aes(x = interval, y = steps))
g_i <- g_i + geom_line(col = "red", show.legend = TRUE) 
g_i <- g_i + labs(title = "Daily Steps", x = "Interval", y = "Mean Steps per day")
print(g_i)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-6-1.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
interval_activity[which.max(interval_activity$steps),]
```

    ##    interval    steps
    ## 1:      835 206.1698

Imputing missing values
=======================

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with ????????s)

``` r
sum(is.na(activity_total))
```

    ## [1] 2304

``` r
# alternative
sum(!complete.cases(activity_total))
```

    ## [1] 2304

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

``` r
activity_total[is.na(steps), "steps"] <-                                                         activity_total[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
data.table::fwrite(x = activity_total, file = "tidyData.csv", quote = FALSE)
```

Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
# Total number of steps taken per day
Total_Steps <- activity_total[, c(lapply(.SD, sum)), .SDcols = c("steps"), 
                              by = .(date)] 

# mean and median total number of steps taken per day
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]
```

    ##    Mean_Steps Median_Steps
    ## 1:    9354.23        10395

``` r
## make plot
gg <- ggplot(Total_Steps, aes(x = steps))
gg <- gg + geom_histogram(fill = "green", binwidth = 999)
gg <- gg + labs(title = "Daily Steps", x = "Steps", y = "Frequency")
print(gg)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-11-1.png)

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

Create a new factor variable in the dataset with two levels: - weekday - weekend indicating whether a given date is a weekday or weekend day.

``` r
activity_total[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activity_total[, `Day of Week`:= weekdays(x = date)]
activity_total[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", 
                     x = `Day of Week`), "weekday or weekend"] <- "weekday"
activity_total[grepl(pattern = "Saturday|Sunday", 
                     x = `Day of Week`), "weekday or weekend"] <- "weekend"
activity_total[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(activity_total, 10)
```

    ##     steps       date interval Day of Week weekday or weekend
    ##  1:     0 2012-10-01        0      Monday            weekday
    ##  2:     0 2012-10-01        5      Monday            weekday
    ##  3:     0 2012-10-01       10      Monday            weekday
    ##  4:     0 2012-10-01       15      Monday            weekday
    ##  5:     0 2012-10-01       20      Monday            weekday
    ##  6:     0 2012-10-01       25      Monday            weekday
    ##  7:     0 2012-10-01       30      Monday            weekday
    ##  8:     0 2012-10-01       35      Monday            weekday
    ##  9:     0 2012-10-01       40      Monday            weekday
    ## 10:     0 2012-10-01       45      Monday            weekday

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` r
activity_total[is.na(steps), "steps"] <- activity_total[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
interval_activity <- activity_total[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 
gg <- ggplot(interval_activity , aes(x = interval , y = steps, color=`weekday or weekend`))
gg <- gg + geom_line()
gg <- gg + labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps")
gg <- gg + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
print(gg)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-13-1.png)
