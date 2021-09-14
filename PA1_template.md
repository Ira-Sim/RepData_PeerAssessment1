---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document: 
    keep_md: TRUE
---
Reproducible research Week 2: Assignment 
=========================================



Today is 2021-09-14.

Clear the workspace and load packages. 

```r
rm(list=ls())
library(dplyr)
library(tidyr)
```


Download and upload data. 


```r
wd <- "C:/Users/vi04wecu/Dropbox/Projects/ML/R/Reproducible_research/Week_2/RR_week_2"
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
myfile<- "data.zip"   

setwd(wd)

download.file(url, myfile, mode="wb")
unzip(myfile)
list.files(wd)
data<- read.csv("activity.csv")
```


Data manipulation and description


```r
data$date<- as.Date(data$date, "%Y-%m-%d")
data$steps<- as.numeric(data$steps)
data$interval<- as.numeric(data$interval)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
datanotmissing <- data[!is.na(data$steps), ]
```

##What is mean total number of steps taken per day?
Make a histogram of the total number of steps taken each day


```r
sumsteps<-aggregate(datanotmissing$steps, by=list(Category=datanotmissing$date), FUN=sum)
hist(sumsteps$x, xlab = "Total number of steps per day", main="Total Number of Steps per Day")
```

![](./figure/unnamed-chunk-5-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day


```r
mean<- mean(sumsteps$x,  na.rm=TRUE)
median <- median(sumsteps$x,  na.rm=TRUE)
```

Mean of total number of steps is **1.0766189\times 10^{4}**, median is **1.0765\times 10^{4}**.

##What is the average daily activity pattern? 


```r
meansteps<-aggregate(datanotmissing$steps, by=list(Category=datanotmissing$interval), FUN=mean)
```


```r
plot(x=meansteps$Category, y=meansteps$x, type = "l", main = "Average daily activity pattern", ylab = "Number of Steps", xlab = "5-minute interval")
```

![](./figure/unnamed-chunk-8-1.png)<!-- -->
 
 

```r
max <- max(meansteps$x)
maxint<- meansteps[meansteps$x == max , 1]
```
 
The maximum number of steps is in the **835** interval.

##Imputing missing values


```r
summissing<- sum(is.na(data))
```
There are **2304** missing values in the dataset.



```r
dataimputed <- data

dataimputed<- dataimputed %>% group_by(interval) %>% mutate(num_avg = mean(steps,na.rm=T))
dataimputed$steps[is.na(dataimputed$steps)] <- dataimputed$num_avg[is.na(dataimputed$steps)]
```



```r
sumsteps_imp<-aggregate(dataimputed$steps, by=list(Category=dataimputed$date), FUN=sum)
hist(sumsteps_imp$x, xlab = "Total number of steps per day", main="Total Number of Steps per Day")
```

![](./figure/unnamed-chunk-12-1.png)<!-- -->


```r
mean_imp<- mean(sumsteps_imp$x,  na.rm=TRUE)
median_imp <- median(sumsteps_imp$x,  na.rm=TRUE)
```

Mean of total number of steps is **1.0766189\times 10^{4}** , median is **1.0765\times 10^{4}**.

The calculations are the same.

##Are there differences in activity patterns between weekdays and weekends?


```r
dataimputed$weekdays<- weekdays(dataimputed$date)

 dataimputed$weekend<- as.numeric(rep(0, times=length(dataimputed$steps)))
 for (x in 1:length(dataimputed$steps)) {
   
   if (dataimputed[x, 5] %in% c("Samstag", "Sonntag")) {
     dataimputed[x, 6]<- 1
   }
   
 }
 
 weekdaydata<- dataimputed[dataimputed$weekend==0, ]
 meansteps_weekday<-aggregate(weekdaydata$steps, by=list(Category=weekdaydata$interval), FUN=mean)

 
 weekendata<- dataimputed[dataimputed$weekend==1, ]
 meansteps_weekend<-aggregate(weekendata$steps, by=list(Category=weekendata$interval), FUN=mean)
```


Plot by group


```r
par(mfrow=c(2,1),  mar=c(4, 4, 3, 2))
plot(x=meansteps_weekday$Category, y=meansteps_weekday$x, type = "l", main = "Average daily activity pattern (Weekday)", ylab = "Number of Steps", xlab = "5-minute interval")

plot(x=meansteps_weekend$Category, y=meansteps_weekend$x, type = "l", main = "Average daily activity pattern (Weekend)", ylab = "Number of Steps", xlab = "5-minute interval")
```

![](./figure/unnamed-chunk-15-1.png)<!-- -->

```r
dev.off()  
```

```
## null device 
##           1
```

