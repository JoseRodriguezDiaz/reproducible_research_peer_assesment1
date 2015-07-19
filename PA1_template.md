---
title: "PA1_template.Rmd"
output: html_document
---

#Load necessary packages


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.1.3
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.1.3
```

#Explore the data


```r
url <-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
activity <- read.csv(unz("activity.zip", "activity.csv"), header=T, quote="\"", sep=",")
```

```
## Warning in open.connection(file, "rt"): cannot open zip file 'activity.zip'
```

```
## Error in open.connection(file, "rt"): cannot open the connection
```

```r
dim(activity)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
str(activity)
```

```
## Error in str(activity): object 'activity' not found
```

```r
names(activity)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
summary(activity)
```

```
## Error in summary(activity): object 'activity' not found
```
#Processing and Transforming the data


```r
nonaactivity <-na.omit(activity)
```

```
## Error in na.omit(activity): object 'activity' not found
```

```r
nonaactivity$date <- as.Date(nonaactivity$date)
```

```
## Error in as.Date(nonaactivity$date): object 'nonaactivity' not found
```

```r
str(nonaactivity)
```

```
## Error in str(nonaactivity): object 'nonaactivity' not found
```

#Feature Engineering - adding the day column, month and year columns


```r
nonaactivity <- transform(nonaactivity, day = day(nonaactivity$date))
```

```
## Error in transform(nonaactivity, day = day(nonaactivity$date)): object 'nonaactivity' not found
```

```r
nonaactivity <- transform(nonaactivity, month = month(nonaactivity$date))                                        
```

```
## Error in transform(nonaactivity, month = month(nonaactivity$date)): object 'nonaactivity' not found
```

```r
nonaactivity <- transform(nonaactivity, year = year(nonaactivity$date))
```

```
## Error in transform(nonaactivity, year = year(nonaactivity$date)): object 'nonaactivity' not found
```

```r
View(nonaactivity[1:500,])
```

```
## Error in View : object 'nonaactivity' not found
```

#Provide the sum of total steps as grouped by date


```r
totals <- nonaactivity %>%
         group_by(date) %>%
         summarize(totalsteps = sum(steps))
```

```
## Error in eval(expr, envir, enclos): object 'nonaactivity' not found
```

#Create the histogram showing the total number of steps per day


```
## Error in ggplot(nonaactivity, aes(date, steps)): object 'nonaactivity' not found
```

#calculate the mean and the median


```r
mean(totals$totalsteps)
```

```
## Error in mean(totals$totalsteps): object 'totals' not found
```

```r
median(totals$totalsteps)
```

```
## Error in median(totals$totalsteps): object 'totals' not found
```

# What is the average daily activity pattern?
Calculate average steps for each of 5-minute interval during a 24-hour period.  
Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axi) and the average number of steps taken, averaged across all days (y-axis)  
Report which 5-minute interval, on average across all the days in the dataset contains the maximum number of steps?  
Observer and comment the average daily activity pattern


```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.2.0
```

```
## -------------------------------------------------------------------------
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
## -------------------------------------------------------------------------
## 
## Attaching package: 'plyr'
## 
## The following object is masked from 'package:lubridate':
## 
##     here
## 
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```r
intervalsteps <- ddply(nonaactivity, ~ interval, summarize,mean=mean(steps))
```

```
## Error in empty(.data): object 'nonaactivity' not found
```

```r
qplot(x=interval, y=mean, data = intervalsteps,  geom = "line",
      xlab="Interval - 5 Minute",
      ylab="Step Count",
      main="Average Steps"
)
```

```
## Error in ggplot(data, aesthetics, environment = env): object 'intervalsteps' not found
```

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
intervalsteps[which.max(intervalsteps$mean), ]
```

```
## Error in eval(expr, envir, enclos): object 'intervalsteps' not found
```

#Calculate and report the total number of missing values in the dataset


```r
activity_NA <- sum(is.na(activity))
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
activity_NA
```

```
## Error in eval(expr, envir, enclos): object 'activity_NA' not found
```

#Imputing NAs
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
newsteps <- aggregate(steps ~ interval, data = activity, FUN = mean)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
imputdata <- numeric()
for (i in 1:nrow(activity)) {
  obs <- activity[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(newsteps, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  imputdata <- c(imputdata, steps)
}
```

```
## Error in nrow(activity): object 'activity' not found
```

```r
imputactivity <- activity
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
imputactivity$steps <- imputdata
```

```
## Error in imputactivity$steps <- imputdata: object 'imputactivity' not found
```

```r
imputsteps <- aggregate(steps ~ date, data = imputactivity, sum, na.rm = TRUE)
```

```
## Error in eval(expr, envir, enclos): object 'imputactivity' not found
```


```
## Error in hist(imputsteps$steps, main = "Steps Per day", xlab = "Day"): object 'imputsteps' not found
```


```r
mean(imputsteps$steps)
```

```
## Error in mean(imputsteps$steps): object 'imputsteps' not found
```

```r
median(imputsteps$steps)
```

```
## Error in median(imputsteps$steps): object 'imputsteps' not found
```

#Are there differences in the weekdays vs weekends?


```r
weekenddata <- nonaactivity 
```

```
## Error in eval(expr, envir, enclos): object 'nonaactivity' not found
```

```r
weekenddata$weekdays <- factor(format(weekenddata$date, "%A"))
```

```
## Error in format(weekenddata$date, "%A"): object 'weekenddata' not found
```

```r
levels(weekenddata$weekdays)
```

```
## Error in levels(weekenddata$weekdays): object 'weekenddata' not found
```

```r
levels(weekenddata$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                                 "Wednesday", 
                                                 "Thursday", "Friday"),
                                     weekend = c("Saturday", "Sunday"))
```

```
## Error in levels(weekenddata$weekdays) <- list(weekday = c("Monday", "Tuesday", : object 'weekenddata' not found
```

```r
levels(weekenddata$weekdays)
```

```
## Error in levels(weekenddata$weekdays): object 'weekenddata' not found
```

```r
table(weekenddata$weekdays)
```

```
## Error in table(weekenddata$weekdays): object 'weekenddata' not found
```

```r
stepaverage <- aggregate(weekenddata$steps, 
                         list(interval = as.numeric(as.character(weekenddata$interval)), 
                              weekdays = weekenddata$weekdays),
                         FUN = "mean")
```

```
## Error in aggregate(weekenddata$steps, list(interval = as.numeric(as.character(weekenddata$interval)), : object 'weekenddata' not found
```

```r
names(stepaverage)[3] <- "meanOfSteps"
```

```
## Error in names(stepaverage)[3] <- "meanOfSteps": object 'stepaverage' not found
```

```r
library(lattice)
```


```
## Error in eval(expr, envir, enclos): object 'stepaverage' not found
```
