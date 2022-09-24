# Reproducible Research: Peer Assessment 1
Alessandro Pietrelli  


## Loading and preprocessing the data

### Define chunk options and load R libraries


```r
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
opts_chunk$set(echo=TRUE, fig.width=8, fig.height=6)
options("scipen"=100, "digits"=4)
```

### Download data


```r
url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filename = "Ass1_data.zip"
download.file(url, filename)
outname = "activity.csv"
unzip(filename, outname)
```

### Read data


```r
dt = read.csv(outname,colClasses=c("double",
                                "Date",
                                "integer"))
dt=as.data.table(dt)
str(dt)
```

```
## Classes 'data.table' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

### Remove NA

Since the "steps" column contain NA values, I will remove them


```r
dtFilter = na.omit(dt)
```

## What is mean total number of steps taken per day?

### * Prepare the data


```r
dt_mean = 
        dtFilter %>% 
        group_by(date) %>% 
        summarise(total = sum(steps),
                  avg = mean(steps, na.rm=TRUE),
                  median = median(as.double(steps)))
```

### * Plot Total steps per day


```r
ggplot(dt_mean, aes(total)) +
        geom_histogram(fill = "steelblue", binwidth = 1000) +
        xlab("Total steps per day") +
        ylab("Frequency") +
        ggtitle("Histogram of the total number of steps per day")
```

![](PA1_template_files/figure-html/plot_total-1.png) 

The average number of steps per day is *10766.1887*

The median number of steps per day is *10765*

## What is the average daily activity pattern?

### * Prepare the data


```r
dt_mean2 = 
        dtFilter %>% 
        group_by(interval) %>% 
        summarise(total = sum(steps),
                  avg = mean(steps, na.rm=TRUE),
                  median = median(as.double(steps)))
```

### * Plot the average steps per interval across all days


```r
ggplot(dt_mean2, aes(interval,avg)) +
        geom_line(colour = "blue") +
        xlab("Interval") +
        ylab("Steps") +
        ggtitle("Histogram of the average number of steps per interval")
```

![](PA1_template_files/figure-html/plot_timeseries-1.png) 

### * What is the interval with the maximum average value of steps? 


```r
dt_mean2 %>% 
        filter(avg==max(avg)) %>% 
        select(interval,avg)
```

```
## Source: local data table [1 x 2]
## 
##   interval   avg
##      (int) (dbl)
## 1      835 206.2
```

## Imputing missing values

### How many NA values are present in the original dataset


```r
is.na(dt$steps) %>% sum()
```

```
## [1] 2304
```

### Strategy for NA imputation

NA values will be imputed using the MEAN for each INTERVAL


```r
setkey(dt,date)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

dtFilled = dt %>%
        group_by(interval) %>%
        mutate(
                steps = impute.mean(steps)
        )

sum(is.na(dtFilled))
```

```
## [1] 0
```

### Prepare filled data


```r
dtFilled_mean = 
        dtFilled %>% 
        group_by(date) %>% 
        summarise(total = sum(steps),
                  avg = mean(steps, na.rm=TRUE),
                  median = median(as.double(steps)))
```

### Plot the total steps per day of filled data


```r
ggplot(dtFilled_mean, aes(total)) +
        geom_histogram(fill = "steelblue", binwidth = 1000) +
        xlab("Total steps per day") +
        ylab("Frequency") +
        ggtitle("Histogram of the total number of steps per day - Filled dataset")
```

![](PA1_template_files/figure-html/plot_total_filled-1.png) 

The average number of steps per day is *10766.1887*

The median number of steps per day is *10766.1887*

## Are there differences in activity patterns between weekdays and weekends?

Add new column to filled dataset that indicate if is a weekday or a weekend


```r
dtFilledWeekday = 
        dtFilled %>% 
        mutate(
                weekday = ifelse(
                        weekdays(date, abbreviate=1) %in% c("Sab","Dom"),
                        "Weekend",
                        "Weekday")
        )
```

### * Prepare the data


```r
dtFilledWeekday_time = 
        dtFilledWeekday %>% 
        group_by(interval,weekday) %>% 
        summarise(total = sum(steps),
                  avg = mean(steps, na.rm=TRUE),
                  median = median(as.double(steps)))
```

### * Plot the number of steps facet by weekdays


```r
ggplot(dtFilledWeekday_time, aes(interval,avg)) +
        geom_line(aes(color=weekday)) +
        scale_color_brewer(palette = "Set1") +
        xlab("Interval") +
        ylab("Steps") +
        facet_grid(weekday ~ .) +
        ggtitle("Histogram of the average number of steps per interval facet by weekday - Filled dataset")
```

![](PA1_template_files/figure-html/plot_weekday-1.png) 
