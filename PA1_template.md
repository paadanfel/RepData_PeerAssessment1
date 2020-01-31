---
title: "Reproducible Research: Peer Assessment 1"
author: "Daniel Felbah"
output: 
  html_document:
    keep_md: true
    self_contained: no
---

---------------------------------

#### Libraries used


```r
library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)
```

&nbsp;

## Loading and preprocessing the data


```r
# Unzip file
unzip("activity.zip")

# Load the data (i.e. read.csv())
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)

# Process/transform the data (if necessary) into a format suitable for your analysis
activity <- tbl_df(activity)
activity$date <- ymd(activity$date)

str(activity)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

&nbsp;

## What is mean total number of steps taken per day?


```r
# Calculate the total number of steps taken per day
total_steps_by_day <- summarise(group_by(activity, date), total_steps = sum(steps, na.rm = TRUE))
head(total_steps_by_day, 10)
```

```
## # A tibble: 10 x 2
##    date       total_steps
##    <date>           <int>
##  1 2012-10-01           0
##  2 2012-10-02         126
##  3 2012-10-03       11352
##  4 2012-10-04       12116
##  5 2012-10-05       13294
##  6 2012-10-06       15420
##  7 2012-10-07       11015
##  8 2012-10-08           0
##  9 2012-10-09       12811
## 10 2012-10-10        9900
```

```r
# Make a histogram of the total number of steps taken each day
qplot(total_steps, data = total_steps_by_day, main = "Histogram of the total number of steps taken each day", binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# Calculate and report the mean and median of the total number of steps taken per day
summary(total_steps_by_day$total_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

The mean and median of the total number of steps taken per day are `9354` and `10395` respectively.


&nbsp;

## What is the average daily activity pattern?


```r
# Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

average_steps_by_interval <- summarise(group_by(activity, interval), average_steps = mean(steps, na.rm = TRUE))

ggplot(average_steps_by_interval, aes(x=interval, y=average_steps)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

Looking at the plot, it should be between `830` and `835`


## Imputing missing values  


```r
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

table(is.na(activity$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```


There are `2304` NAs in the dataset.  

Since there are so many NAs in the dataset, we'll replace each NA with the mean number of steps for that interval across the days.  
We have `average_steps_by_interval` already created in the table so we'll map it back to the main dataset to replace all NA's. We will call
this new data `activity_NAs_replaced`.



```r
# Create a new dataset that is equal to the original dataset but with the missing data filled in.

activity_NAs_replaced <- activity # make a copy of original file

for (i in 1:nrow(activity)) {  # loop through activity
  
  if (is.na(activity$steps[i])){  # check if steps is NA
    
    activity_NAs_replaced$steps[i] <- average_steps_by_interval[which(average_steps_by_interval$interval == activity$interval[i]),]$average_steps  # replace NA
  
  }
  
}

table(is.na(activity_NAs_replaced$steps)) # check if there are still NAs
```

```
## 
## FALSE 
## 17568
```


Make a histogram of the total number of steps taken each day.


```r
# Calculate the total number of steps taken per day
total_steps_by_day_new <- summarise(group_by(activity_NAs_replaced, date), total_steps = sum(steps))


# Make a histogram of the total number of steps taken each day
new <- qplot(total_steps, data = total_steps_by_day_new, main = "Data without NAs", binwidth = 1000)
old <- qplot(total_steps, data = total_steps_by_day, main = "Data with NAs", binwidth = 1000)

plot_grid(new, old)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day.


```r
summary(total_steps_by_day_new$total_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

The mean and median of the total number of steps taken per day are `10766` and `10766` respectively.



## Are there differences in activity patterns between weekdays and weekends?
