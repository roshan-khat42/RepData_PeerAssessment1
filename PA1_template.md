---
title: "Project 1"
author: "Roshan"
date: "10/26/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data

### 1. Load the data
```{r}
activity <- read.csv("activity.csv")
```
##                   days steps
## 1  2012-10-02 00:00:00   126
## 2  2012-10-03 00:00:00 11352
## 3  2012-10-04 00:00:00 12116
## 4  2012-10-05 00:00:00 13294
## 5  2012-10-06 00:00:00 15420
## 6  2012-10-07 00:00:00 11015
## 7  2012-10-09 00:00:00 12811
## 8  2012-10-10 00:00:00  9900
## 9  2012-10-11 00:00:00 10304
## 10 2012-10-12 00:00:00 17382
## 11 2012-10-13 00:00:00 12426
## 12 2012-10-14 00:00:00 15098
## 13 2012-10-15 00:00:00 10139
## 14 2012-10-16 00:00:00 15084
## 15 2012-10-17 00:00:00 13452
## 16 2012-10-18 00:00:00 10056
## 17 2012-10-19 00:00:00 11829
## 18 2012-10-20 00:00:00 10395
## 19 2012-10-21 00:00:00  8821
## 20 2012-10-22 00:00:00 13460
## 21 2012-10-23 00:00:00  8918
## 22 2012-10-24 00:00:00  8355
## 23 2012-10-25 00:00:00  2492
## 24 2012-10-26 00:00:00  6778
## 25 2012-10-27 00:00:00 10119
## 26 2012-10-28 00:00:00 11458
## 27 2012-10-29 00:00:00  5018
## 28 2012-10-30 00:00:00  9819
## 29 2012-10-31 00:00:00 15414
## 30 2012-11-02 00:00:00 10600
## 31 2012-11-03 00:00:00 10571
## 32 2012-11-04 23:00:00 10439
## 33 2012-11-05 23:00:00  8334
## 34 2012-11-06 23:00:00 12883
## 35 2012-11-07 23:00:00  3219
## 36 2012-11-10 23:00:00 12608
## 37 2012-11-11 23:00:00 10765
## 38 2012-11-12 23:00:00  7336
## 39 2012-11-14 23:00:00    41
## 40 2012-11-15 23:00:00  5441
## 41 2012-11-16 23:00:00 14339
## 42 2012-11-17 23:00:00 15110
## 43 2012-11-18 23:00:00  8841
## 44 2012-11-19 23:00:00  4472
## 45 2012-11-20 23:00:00 12787
## 46 2012-11-21 23:00:00 20427
## 47 2012-11-22 23:00:00 21194
## 48 2012-11-23 23:00:00 14478
## 49 2012-11-24 23:00:00 11834
## 50 2012-11-25 23:00:00 11162
## 51 2012-11-26 23:00:00 13646
## 52 2012-11-27 23:00:00 10183
## 53 2012-11-28 23:00:00  7047

### 2. Process/transform the data
```{r}
activity$date <- as.POSIXct(activity$date, format="%Y-%m-%d")
```


## What is mean total number of steps taken per day?

### 1. 
```{r}
activity_clean <- activity[complete.cases(activity),]
total_steps_per_day <- aggregate(activity_clean["steps"], 
                   list(days=cut(activity_clean$date, "1 days")),
                   sum)
total_steps_per_day
```

### 2. 
```{r}
library(ggplot2)

g <- ggplot(total_steps_per_day, aes(x = steps))
g <- g + geom_histogram() + stat_bin(aes(y=..count.., label=..count..), 
                                geom="text", vjust=-.5)
g + ggtitle("Histogram of total steps per day")
```

### 3. 

```{r}
average_steps_per_day <- aggregate(activity_clean["steps"], 
                   list(days=cut(activity_clean$date, "1 days")),
                   mean)
average_steps_per_day
```

```{r}
median_steps_per_day <- aggregate(activity_clean["steps"], 
                   list(days=cut(activity_clean$date, "1 days")),
                   median)
median_steps_per_day
```

## What is the average daily activity pattern?

### 1. 

```{r}
avg_steps_five_mins <- aggregate(steps ~ interval, data = activity_clean, mean)

plot(avg_steps_five_mins$interval, avg_steps_five_mins$steps, type="l")
lines(avg_steps_five_mins$interval, avg_steps_five_mins$steps, col="red")
```

### 2. Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?

```{r}
avg_steps_five_mins$interval[which.max(avg_steps_five_mins$steps)]
```

835, 5-minute interval, on average across all the days in the 
data set, contains the maximum number of steps.

## Imputing missing values

### 1.

```{r}
sum(is.na(activity))
```

Total number of missing rows: 2304

### 2. 

Filling missing values with the average value of the day.

### 3.

```{r}
fill_missing <- function(df1, df2) {
  n <- nrow(df1) 
  new_df <- df1

  for(i in 1:ncol(df1)){
    new_df[is.na(df1[,"steps"]), "steps"] <- mean(df1[,"steps"], na.rm = TRUE)
  }
  
  return(new_df)
}

average_steps_per_day$days <- as.POSIXct(average_steps_per_day$days, format="%Y-%m-%d")
activity_new <- fill_missing(activity, average_steps_per_day)

sum(is.na(activity_new))
```

### 4. 

```{r}
total_steps_per_day <- aggregate(activity_new["steps"], 
                   list(days=cut(activity_new$date, "1 days")),
                   sum)
head(total_steps_per_day)
```

```{r}
g <- ggplot(total_steps_per_day, aes(x = steps))
g <- g + geom_histogram() + stat_bin(aes(y=..count.., label=..count..), 
                                geom="text", vjust=-.5)
g + ggtitle("Histogram of total steps per day")
```

```{r}
average_steps_per_day <- aggregate(activity_new["steps"], 
                   list(days=cut(activity_new$date, "1 days")),
                   mean)
head(average_steps_per_day)
```

```{r}
median_steps_per_day <- aggregate(activity_new["steps"], 
                   list(days=cut(activity_new$date, "1 days")),
                   median)
median_steps_per_day
```

The imputed dates now have different median values, also the mean
of each day has changed after imputing missing values.

## Are there differences in activity patterns between weekdays and weekends?


### 1.

```{r}
activity$weekday <- ifelse(weekdays(activity$date) == "Saturday" | weekdays(activity$date) == "Sunday", "Weekend", "Weekday")
```


### 2.

```{r}
g <- ggplot(activity, aes(x = interval, y = steps))
g <- g + geom_line() + facet_grid(vars(weekday))
g
```

