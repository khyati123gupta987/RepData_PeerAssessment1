# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1. Load the data (i.e. 𝚛𝚎𝚊𝚍.𝚌𝚜𝚟())
2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
dataMain <- read.table(unz("activity.zip", "activity.csv"), header=T, quote="\"", sep=",")
dataMain$date<-as.Date(dataMain$date)
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
library("ggplot2")
dayData=aggregate(steps~date,FUN=sum,data=dataMain)
```
2. Make a histogram of the total number of steps taken each day


```r
hist(dayData$steps,col="grey", xlab = "Total Steps per Day", ylab = "Frequency", 
    main = "Histogram of Total Steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(dayData$steps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median (dayData$steps,na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of  5-minute interval (x-axis) and the average number of   steps taken, averaged across all days (y-axis)


```r
dayDataMean= aggregate(steps~interval,FUN=mean,data=dataMain)
p <- ggplot(dayDataMean, aes(x=interval, y=steps))
p + geom_line()+labs(title="Average number of steps taken per interval")+labs(x="interval",y="steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
dayDataMean[which.max(dayDataMean$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```r
sum(!complete.cases(dataMain))
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
dataMean<-aggregate(steps ~interval,FUN=mean, data=dataMain)

data<-merge(dataMain,dataMean,by="interval")

data = transform(data, steps.x = ifelse(is.na(steps.x), steps.y, steps.x))
dayData=aggregate(steps.x ~ date,FUN=sum,data=data)
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
hist(dayData$steps.x,col="grey", xlab = "Total Steps per Day", ylab = "Frequency", 
    main = "Histogram of Total Steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(dayData$steps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median (dayData$steps,na.rm=TRUE)
```

```
## [1] 10766.19
```




## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
dataMain$day <-factor(weekdays(dataMain$date))
levels(dataMain$day)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(dataMain$day) <- list(weekday = c("Monday", 
                                         "Tuesday",
                                         "Wednesday", 
                                         "Thursday", 
                                         "Friday"),
                             weekend = c("Saturday", 
                                         "Sunday"))
levels(dataMain$day)
```

```
## [1] "weekday" "weekend"
```
2. Make a panel plot containing a time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(lattice)
meanStepsByDay <- aggregate(steps ~ interval + day, data = dataMain, mean)

xyplot(steps ~ interval | day, meanStepsByDay, type = "l", layout = c(1, 2), 
       col="blue",
       par.settings = list(strip.background=list(col="lightgreen")),
       xlab="Interval", ylab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
