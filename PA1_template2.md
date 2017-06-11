Setting knitr options
=====================

    library(knitr)

    ## Warning: package 'knitr' was built under R version 3.3.3

    base.dir <- "/Users/Magdy/Documents"
    fig.path <- "/Users/Magdy/Documents"
    opts_knit$set(base.dir = base.dir, self.contained=FALSE)
    knitr::opts_chunk$set(cache = TRUE, echo = TRUE, message = FALSE, 
                          warning = FALSE, fig.path = fig.path, base.dir = base.dir) 

Loading and preprocessing data
==============================

    if(!file.exists('activity.csv')){
        unzip('activity.zip')
    }
    activityData <- read.csv('activity.csv')

Calculating mean total number of steps taken per day
====================================================

    total.steps <- tapply(activityData$steps, activityData$date, FUN=sum, na.rm=TRUE)
    hist(total.steps, xlab="total number of steps taken each day")

![](/Users/Magdy/Documents/figure/unnamed-chunk-2-1.png)

    mean(total.steps, na.rm=TRUE)

    ## [1] 9354.23

    median(total.steps, na.rm=TRUE)

    ## [1] 10395

Calculating/Plotting Average daily activity pattern
===================================================

1. Time series plot
-------------------

    library(ggplot2)
    averages <- aggregate(x=list(steps=activityData$steps), by=list(interval=activityData$interval),
                          FUN=mean, na.rm=TRUE)
    ggplot(data=averages, aes(x=interval, y=steps)) + geom_line() + xlab("5-minute interval") +
        ylab("average number of steps taken")

![](/Users/Magdy/Documents/figure/unnamed-chunk-3-1.png)

2. Maximum number of steps
--------------------------

    averages[which.max(averages$steps),]

    ##     interval    steps
    ## 104      835 206.1698

Imputing missing values
=======================

1. Calculating and reporting the total number of missing values in the dataset
------------------------------------------------------------------------------

    missing <- is.na(activityData$steps)
    table(missing)

    ## missing
    ## FALSE  TRUE 
    ## 15264  2304

2. Filling in all of the missing values in the dataset with the mean value of its corresponding 5-minute interval
-----------------------------------------------------------------------------------------------------------------

    fill.value <- function(steps, interval) {
        filled <- NA
        if (!is.na(steps))
            filled <- c(steps)
        else
            filled <- (averages[averages$interval==interval, "steps"])
        return(filled)
    }

3. Creating a new dataset with missing data filled in
-----------------------------------------------------

    filled.activityData <- activityData
    filled.activityData$steps <- mapply(fill.value, filled.activityData$steps, filled.activityData$interval)

4. Impact of imputing missing data
----------------------------------

### a. Plotting a histogram of the total number of steps taken each day

    total.steps <- tapply(filled.activityData$steps, filled.activityData$date, FUN=sum)
    hist(total.steps, xlab="total number of steps taken each day")

![](/Users/Magdy/Documents/figure/unnamed-chunk-8-1.png)

### b. Calculating and reporting mean and median

    mean(total.steps)

    ## [1] 10766.19

    median(total.steps)

    ## [1] 10766.19

### c. Mean and median values differ from the estimates when missing values were not accounted for. Hence, an impact of imputing missing data on the estimates of the total daily number of steps can be concluded.

Estimating the differences in activity patterns between weekdays and weekends
=============================================================================

1. Subset the dataset into 2 levels (weekday vs weekend)
--------------------------------------------------------

    weekday_vs_weekend <- function(date) {
        day <- weekdays(date)
        if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
            return("weekday")
        else if (day %in% c("Saturday", "Sunday"))
            return("weekend")
        else
            stop("invalid date")
    }
    filled.activityData$date <- as.Date(filled.activityData$date)
    filled.activityData$day <- sapply(filled.activityData$date, FUN=weekday_vs_weekend)

2. Creating time series plot
----------------------------

    averages <- aggregate(steps ~ interval + day, data=filled.activityData, mean)
    ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
        xlab("5-minute interval") + ylab("Number of steps")

![](/Users/Magdy/Documents/figure/unnamed-chunk-11-1.png)
