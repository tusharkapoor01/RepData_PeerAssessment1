Prepare the R environment
-------------------------

    library(knitr)

    ## Warning: package 'knitr' was built under R version 3.3.2

    opts_chunk$set(echo = TRUE, results = 'hold')

Load required libraries
-----------------------

    library(data.table)

    ## Warning: package 'data.table' was built under R version 3.3.2

    library(ggplot2) # we shall use ggplot2 for plotting figures

    ## Warning: package 'ggplot2' was built under R version 3.3.2

Loading and preprocessing the data
----------------------------------

    rdata <- read.csv('activity.csv', header = TRUE, sep = ",",
                      colClasses=c("numeric", "character", "numeric"))

tidy the data or preprocess the data
------------------------------------

    rdata$date <- as.Date(rdata$date, format = "%Y-%m-%d")
    rdata$interval <- as.factor(rdata$interval)
    str(rdata)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...

What is mean total number of steps taken per day?
-------------------------------------------------

    steps_per_day <- aggregate(steps ~ date, rdata, sum)
    colnames(steps_per_day) <- c("date","steps")
    head(steps_per_day)

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

1.  Now we make a histogram of the total number of steps taken per day,
    plotted with appropriate bin interval.

<!-- -->

    ggplot(steps_per_day, aes(x = steps)) + 
           geom_histogram(fill = "blue", binwidth = 1000) + 
            labs(title="Histogram of Steps Taken per Day", 
                 x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

2.Now we calculate the mean and median of the number of steps taken per
day.

    steps_mean   <- mean(steps_per_day$steps, na.rm=TRUE)
    steps_median <- median(steps_per_day$steps, na.rm=TRUE)

What is the average daily activity pattern?
-------------------------------------------

    steps_per_interval <- aggregate(rdata$steps, 
                                    by = list(interval = rdata$interval),
                                    FUN=mean, na.rm=TRUE)
    #convert to integers
    ##this helps in plotting
    steps_per_interval$interval <- 
            as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])
    colnames(steps_per_interval) <- c("interval", "steps")

1.  We make the plot with the time series of the average number of steps
    taken (averaged across all days) versus the 5-minute intervals:

<!-- -->

    ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
            geom_line(color="orange", size=1) +  
            labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
            theme_bw()

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

1.  ow, we find the 5-minute interval with the containing the maximum
    number of steps:

<!-- -->

    max_interval <- steps_per_interval[which.max(  
            steps_per_interval$steps),]
    max_interval

    ##     interval    steps
    ## 104      835 206.1698

Imputing missing values:
------------------------

1.  Total number of missing values:

<!-- -->

    missing_vals <- sum(is.na(rdata$steps))
    missing_vals

    ## [1] 2304

1.  Strategy for filling in all of the missing values in the dataset

<!-- -->

    na_fill <- function(data, pervalue) {
            na_index <- which(is.na(data$steps))
            na_replace <- unlist(lapply(na_index, FUN=function(idx){
                    interval = data[idx,]$interval
                    pervalue[pervalue$interval == interval,]$steps
            }))
            fill_steps <- data$steps
            fill_steps[na_index] <- na_replace
            fill_steps
    }

    rdata_fill <- data.frame(  
            steps = na_fill(rdata, steps_per_interval),  
            date = rdata$date,  
            interval = rdata$interval)
    str(rdata_fill)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...

    sum(is.na(rdata_fill$steps))

    ## [1] 0

1.  A histogram of the total number of steps taken each day

<!-- -->

    fill_steps_per_day <- aggregate(steps ~ date, rdata_fill, sum)
    colnames(fill_steps_per_day) <- c("date","steps")

    ##plotting the histogram
    ggplot(fill_steps_per_day, aes(x = steps)) + 
           geom_histogram(fill = "red", binwidth = 1000) + 
            labs(title="Histogram of Steps Taken per Day", 
                 x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-14-1.png)
\#\#\# Calculate and report the mean and median total number of steps
taken per day.

    steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)
    steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)

The Mean is

    steps_mean_fill

    ## [1] 10766.19

The Median is

    steps_median_fill

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    weekdays_steps <- function(data) {
        weekdays_steps <- aggregate(data$steps, by=list(interval = data$interval),
                              FUN=mean, na.rm=T)
        # convert to integers for plotting
        weekdays_steps$interval <- 
                as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
        colnames(weekdays_steps) <- c("interval", "steps")
        weekdays_steps
    }

    data_by_weekdays <- function(data) {
        data$weekday <- 
                as.factor(weekdays(data$date)) # weekdays
        weekend_data <- subset(data, weekday %in% c("Saturday","Sunday"))
        weekday_data <- subset(data, !weekday %in% c("Saturday","Sunday"))

        weekend_steps <- weekdays_steps(weekend_data)
        weekday_steps <- weekdays_steps(weekday_data)

        weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
        weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))

        data_by_weekdays <- rbind(weekend_steps, weekday_steps)
        data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
        data_by_weekdays
    }

    data_weekdays <- data_by_weekdays(rdata_fill)

    ggplot(data_weekdays, aes(x=interval, y=steps)) + 
            geom_line(color="green") + 
            facet_wrap(~ dayofweek, nrow=2, ncol=1) +
            labs(x="Interval", y="Number of steps") +
            theme_bw()

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-19-1.png)
