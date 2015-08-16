---
title: "Peerass1 - activity"
output: word_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.


```r
setwd("~/R/repdata/peerass1")
activity <- read.csv("activity.csv", header=TRUE)
library(ggplot2)
library(dplyr)
library(lubridate)
activity$date <- ymd(activity$date)
activity$steps <- as.numeric(activity$steps)
```

## What is mean total number of steps taken per day?

1) The plot down here shows the total number of steps per day in a histogram

```r
stepsday <- activity %>%
  select(steps, date) %>%
  group_by(date) %>%
  summarise(steps=sum(steps))

ggplot(stepsday, aes(x = date, y = steps)) + geom_histogram(stat="identity")
```

```
## Warning in loop_apply(n, do.ply): Removed 8 rows containing missing values
## (position_stack).
```

![plot of chunk steps per day](figure/steps per day-1.png) 

```r
meanday <- as.integer(round(mean(stepsday$steps, na.rm=TRUE),digits=0))
medianday <- as.integer(round(median(stepsday$steps,na.rm=TRUE),digits=0))
```
2)   
The **mean** total number of steps taken per day is 10766  
The **median** total number of steps taken per day is 10765


## What is the average daily activity pattern?


1) Below the plot shows the average number of steps per 5 minute interval, averaged across all days.

```r
fivemin <- activity %>%
  select(steps, interval) %>%
  group_by(interval)%>%
  summarise(steps=mean(steps, na.rm=TRUE))

ggplot(fivemin, aes(x=interval, y=steps), type = "l") + geom_line() + xlab("Interval number") + ylab("Average number of steps taken")
```

![plot of chunk 5 minutes interval](figure/5 minutes interval-1.png) 

```r
max <- max(fivemin$steps)
max <- fivemin[which(fivemin$steps == max),]
max <- max$interval
```

2) The 5 minute interval which contains the highest average number of steps is interval number 835.


## Imputing missing values


```r
missings <- activity[which(is.na(activity$steps)),]
missingno <- as.integer(nrow(missings))
```

1) The total number of missing values in the dataset is 2304



```r
library(plyr)
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
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following object is masked from 'package:lubridate':
## 
##     here
```

```r
# in order to fill the missings the mean of every 5 minute interval is used
#1 fill missingstable with mean of every 5 minute interval
missings <- missings[,2:3]
missingsimput <- join(x = missings, y = fivemin, by="interval", type = "left")
#2 reorder colums in order to make things ready to rbind
missingsimput <- missingsimput[,c(3,1,2)]
#3 select right activity rows and rbind with imputed missings
act_nw <- activity[which(!is.na(activity$steps)),]
act_nw <- rbind(act_nw, missingsimput)

print(summary(act_nw), type = 'html')
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

2) and   
3) in the chunk above a new dataset containing imputed missings is made. 


```r
detach("package:plyr", unload=TRUE)
```

```
## Warning: 'plyr' namespace cannot be unloaded:
##   namespace 'plyr' is imported by 'lubridate', 'scales', 'ggplot2', 'reshape2' so cannot be unloaded
```

```r
library(dplyr)
stepsday_nw <- act_nw %>%
  select(steps, date) %>%
  group_by(date) %>%
  summarise(steps = sum(steps))

ggplot(stepsday_nw, aes(x = date, y = steps)) + geom_histogram(stat="identity")
```

![plot of chunk histogram incl imputed](figure/histogram incl imputed-1.png) 

```r
meanday_nw <- as.integer(round(mean(stepsday_nw$steps, na.rm=TRUE),digits=0))
medianday_nw <- as.integer(round(median(stepsday_nw$steps,na.rm=TRUE),digits=0))
```
4) Above the new plot with total steps per day  
The **mean** total number of steps taken per day is 10766  
The **median** total number of steps taken per day is 10766

The numbers do not differ significantly from the ones before imputation. The impact of imputation on these calculations is null.


## Are there differences in activity patterns between weekdays and weekends?


```r
library(timeDate)
act_nw$weekdays <- isWeekday(act_nw$date)
act_nw$weekdays <- ifelse(act_nw$weekdays == "TRUE", c("weekday"), c("weekend"))

fivemin_nw <- act_nw %>%
  select(steps, interval, weekdays) %>%
  group_by(weekdays, interval) %>%
  summarise(steps=mean(steps))

qplot(interval,steps, data = fivemin_nw, facets= weekdays ~ ., geom = "line")
```

![plot of chunk weekdays/ends](figure/weekdays/ends-1.png) 

1) The new variabel weekdays is created by using the 'timeDate' package
2) The plot shows a higher activity rate during weekdays 

