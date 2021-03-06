# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv") #reading data
data$date<-as.Date(data$date)    #convert date as Date type

head(data,5)                     #Show first 5 rows 
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
```

```r
#Format interval to HH:MM format
data$interval<-sprintf("%04d",data$interval) #convert 4 character long and filling with 0
#data$interval<-strptime(data$interval,"%H%M")#formatting to HH:MM
head(data,5)
```

```
##   steps       date interval
## 1    NA 2012-10-01     0000
## 2    NA 2012-10-01     0005
## 3    NA 2012-10-01     0010
## 4    NA 2012-10-01     0015
## 5    NA 2012-10-01     0020
```

## What is mean total number of steps taken per day?
Steps:  
1. Calculate set of totals number of steps by day and stored in a variable for reuse it.  
2. Plot an histogram for the set of totals number of steps by day.    
3. Calculate mean and median for the set of totals number of steps by day.    

The histogram below shows a rigth skewed distribution. The mean and median are similar.


```r
#Calculating Sum
stepsbydate <-sapply(split(data$steps,data$date),sum)

#Plotting histogram 
hist(stepsbydate,breaks=10,col="lightblue",xlab="Steps by day",main="Histogram Steps by Day")

#mean
pmean<-mean(stepsbydate,na.rm=TRUE)
pmedian<-median(stepsbydate,na.rm=TRUE)
print(sprintf("media = %f and meadian = %f total number of steps taken per day",pmean,pmedian))
```

```
## [1] "media = 10766.188679 and meadian = 10765.000000 total number of steps taken per day"
```

```r
#plotting lines for mean and median in histogram
abline(v=pmean,col="red", lwd=2)
abline(v=pmedian,col="black",lty=4)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

## What is the average daily activity pattern?
The time seriebelow shows a peak around 8:00 interval, meaning the number of steps on that interval is higher than the average steps. Between 10:00 and 15:00 intervals the oscilation of number steps have a constant variation (down and up).


```r
time_mean<-aggregate(steps ~ interval, data = data, mean)
time_mean$interval<-as.POSIXct(strptime(time_mean$interval,"%H%M"))#formatting to HH:MM
plot(time_mean$interval, time_mean$steps, type="l",xlab="Interval",ylab="steps mean",main="Time Serie")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
int_max<-aggregate(steps ~ interval, data = data, max)
print(sprintf("5-minute interval with the maximum number of steps is %s",int_max[which.max(int_max$steps),1]))
```

```
## [1] "5-minute interval with the maximum number of steps is 0615"
```

## Imputing missing values
Missing data were replaced by median because the histogram above shows a rigth skewed distribution. The median is better suited, as cendtral tendency, for this kind of distribution.


```r
#count missing data (NA)
print(sprintf("The total number of missing data (NA) is %d",sum(!complete.cases(data))))
```

```
## [1] "The total number of missing data (NA) is 2304"
```

```r
#Replacing missing data (NA) with median
new_data <- data
new_data[is.na(new_data)] <- pmedian

#Histogram for new_data
stepsbydate <-sapply(split(new_data$steps,new_data$date),sum)
#stepsbydate<-aggregate(steps ~ date, data = new_data, sum)
hist(stepsbydate,breaks=5,col="lightblue",xlab="Steps by day",main="Histogram Steps by Day with NA replaced by median ")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
pmean<-mean(stepsbydate)
pmedian<-median(stepsbydate)
print(sprintf("media = %f and meadian = %f total number of steps taken per day",pmean,pmedian))
```

```
## [1] "media = 415953.573770 and meadian = 11458.000000 total number of steps taken per day"
```

```r
#What is the impact of imputing missing data on the estimates of the total
#daily number of steps?
```
The histogram above shows that replacing missing data by the median makes the variation  of data similar concentrating the range of steps by days on one class.

## Are there differences in activity patterns between weekdays and weekends?

The patterns between weekday and weekend, see plots below, are different. During weekends the behaviour of steps keeps down and up similarly during the all interval and the average of steps are between 1350 and 1450 steps. On the other hand, during weekdays the average of steps stats 1450 and there is peak around 8:00 interval. Between 10:00 and 15:00 intervals the oscilation of number of steps on weekdays is lower than on weekends.


```r
#Adding new column to the dataset
new_data$weekday <- apply(new_data,1,function(row) if (weekdays(as.Date(row[2]), abbreviate = TRUE) %in% c("Sat","Sun")) as.factor("weekend") else as.factor("weekday") )

#Groupping weekdad and interval
grouping_mean<-aggregate(steps ~ weekday+interval, data = new_data, mean)
grouping_mean$interval<-as.POSIXct(strptime(as.character(grouping_mean$interval), format="%H%M"),format="%H%M")#formatting to HH:MM

head(grouping_mean,10) #showing 10 rows
```

```
##    weekday            interval    steps
## 1  weekday 2015-08-07 00:00:00 1437.356
## 2  weekend 2015-08-07 00:00:00 1345.625
## 3  weekday 2015-08-07 00:05:00 1435.733
## 4  weekend 2015-08-07 00:05:00 1345.625
## 5  weekday 2015-08-07 00:10:00 1435.489
## 6  weekend 2015-08-07 00:10:00 1345.625
## 7  weekday 2015-08-07 00:15:00 1435.511
## 8  weekend 2015-08-07 00:15:00 1345.625
## 9  weekday 2015-08-07 00:20:00 1435.422
## 10 weekend 2015-08-07 00:20:00 1345.625
```

```r
#Plotting panel plots
library(lattice)
print(xyplot(steps ~ interval|weekday, data = grouping_mean, 
             layout = c(1, 2),type="l",xlab="Intervals"
      ))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
