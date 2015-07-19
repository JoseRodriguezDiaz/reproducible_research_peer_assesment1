echo = TRUE
#Loading the processing the data
library(ggplot2)
library(dplyr)
library(lubridate)
url <-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
activity <- read.csv(unz("activity.zip", "activity.csv"), header=T, quote="\"", sep=",")
dim(activity)
str(activity)
names(activity)
summary(activity)

#Processing and Transforming the data
nonaactivity <-na.omit(activity)
nonaactivity$date <- as.Date(nonaactivity$date)
str(nonaactivity)

#Feature Engineering - adding the day column, month and year columns
nonaactivity <- transform(nonaactivity, day = day(nonaactivity$date))
nonaactivity <- transform(nonaactivity, month = month(nonaactivity$date))                                        
nonaactivity <- transform(nonaactivity, year = year(nonaactivity$date))
View(nonaactivity[1:500,])
For this part of the assignment, you can ignore the missing values in the dataset.
1.	Calculate the total number of steps taken per day
2.	Make a histogram of the total number of steps taken each day
3.	Calculate and report the mean and median of the total number of steps taken per day
totals <- nonaactivity %>%
         group_by(date) %>%
         summarize(totalsteps = sum(steps))

#Create the histogram showing the total number of steps per day
hist(totals$totalsteps, xlab = "Total Steps per day", main = "Steps per Day", breaks = 10)

#Calculate the mean and the media
mean(totals$totalsteps)
10766

median(totals$totalsteps)
10765
#What is the average daily activity pattern?
1.	Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2.	Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


intervalsteps <- ddply(nonaactivity, ~interval, summarize,mean=mean(steps))
qplot(x=interval, y=mean, data = intervalsteps,  geom = "line",
      xlab="Interval - 5 Minute",
      ylab="Step Count",
      main="Average Steps"
)


intervalsteps[which.max(intervalsteps$mean), ]
Interval: 104   835  206



#Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
1.	Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2.	Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3.	Create a new dataset that is equal to the original dataset but with the missing data filled in.
4.	Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
activity_NA <- sum(is.na(activity))
activity_NA

#Total number of missing values
2304


#Histogram with imputed data
newsteps <- aggregate(steps ~ interval, data = activity, FUN = mean)
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

imputactivity <- activity
imputactivity$steps <- imputdata

imputsteps <- aggregate(steps ~ date, data = imputactivity, sum, na.rm = TRUE)
hist(imputsteps$steps, main = "Steps Per day", xlab = "Day")


mean(imputsteps$steps)
10766

mean(imputsteps$steps)
10766

The  means are the same but the new median is slightly higher
#Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
1.	Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2.	Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

head(weekenddata)
weekenddata$weekdays <- factor(format(weekenddata$date, "%A"))
levels(weekenddata$weekdays)
levels(weekenddata$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(weekenddata$weekdays)
table(weekenddata$weekdays)

stepaverage <- aggregate(weekenddata$steps, 
                      list(interval = as.numeric(as.character(weekenddata$interval)), 
                           weekdays = weekenddata$weekdays),
                      FUN = "mean")
names(stepaverage)[3] <- "meanOfSteps"
library(lattice)
xyplot(stepaverage$meanOfSteps ~ stepaverage$interval | stepaverage$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")


