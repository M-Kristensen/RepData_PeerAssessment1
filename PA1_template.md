# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
### Loading the data
Let's start off by getting the activity data into R. Here I create a folder for the data and download the .zip file. A URL for the data is provided on the course homepage.



```r
if (!file.exists("data")) dir.create("data")

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "./data/activity.zip", method = "curl")
```


Now, since there is no sense in extracting the data multiple times, we check if this step has already been done. If not, we simply extract the data.



```r
if (!file.exists("./data/activity.csv")) unzip("./data/activity.zip", exdir = "./data")
```


Finally, we load the data into a variable that we shall call... data.



```r
data <- read.table("./data/activity.csv", sep = ",", header = TRUE)
```





### Processing the data
To allow for easier processing, we turn the date data into a valid date class. Dates are in YYYY-MM-DD format...


```r
dates <- strptime(data$date, "%Y-%m-%d")
data$date <- dates
```

... and keep a list of all possible days and intervals in the data.

```r
uniqueDates <- unique(dates)
uniqueIntervals <- unique(data$interval)
```















## What is the mean total number of steps taken per day?
In this step, we would like to calculate the total number of steps taken each day (and to make a matching histogram), as well as the mean and median number of steps taken each day.


### Calculating the total number of steps taken per day
First, we need to split up the data frame according to the date:



```r
stepsSplit <- split(data$steps, dates$yday)
```

Now we can calculate the total number of steps per day, as long as we remember to remove any missing values. Here follows a list of the number of steps taken each day.


```r
totalStepsPerDay <- sapply(stepsSplit, sum, na.rm=TRUE)
totalStepsPerDay
```

```
##   274   275   276   277   278   279   280   281   282   283   284   285 
##     0   126 11352 12116 13294 15420 11015     0 12811  9900 10304 17382 
##   286   287   288   289   290   291   292   293   294   295   296   297 
## 12426 15098 10139 15084 13452 10056 11829 10395  8821 13460  8918  8355 
##   298   299   300   301   302   303   304   305   306   307   308   309 
##  2492  6778 10119 11458  5018  9819 15414     0 10600 10571     0 10439 
##   310   311   312   313   314   315   316   317   318   319   320   321 
##  8334 12883  3219     0     0 12608 10765  7336     0    41  5441 14339 
##   322   323   324   325   326   327   328   329   330   331   332   333 
## 15110  8841  4472 12787 20427 21194 14478 11834 11162 13646 10183  7047 
##   334 
##     0
```


### Create a histogram of the total number of steps taken each day

```r
plot(uniqueDates, totalStepsPerDay,
     main = "Histogram of the total number of steps taken each day",
     xlab = "Dates (October to November 2012",
     ylab = "Number of Steps",
     type = "h",
     lwd = 4,
     col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


### Calculate and report the mean and median of the total number of steps taken per day
Now that we have created a vector containing the number of steps per day (*totalStepsPerDay*), we simply use *mean* and *median* to calculate these values in R.


```r
## Mean:
mean(totalStepsPerDay)
```

```
## [1] 9354.23
```

```r
## Median:
median(totalStepsPerDay)
```

```
## [1] 10395
```
















## What is the average daily activity pattern?
Here we make a time-series plot (type = "l") of the average number of steps taken each day, as a function of the 5 minute time interval of the day.

To do this, we will need to split the data up according to the interval of the day and to calculate the average number of steps for each interval across all possible intervals. Again we remove the missing values.


```r
## Split up the data according to the interval
intervalSplit <- split(data$steps, data$interval)

## Find the average amount of steps per time interval - ignore NA values
averageStepsPerInterval <- sapply(intervalSplit, mean, na.rm=TRUE)

## Plot the time-series graph
plot(uniqueIntervals, averageStepsPerInterval, type="l",
     main="Average number of steps per interval across all days", 
     xlab="Interval", ylab="Average # of steps across all days", 
     lwd=2, col="blue")

## Find the location of where the maximum is
maxIntervalDays <- max(averageStepsPerInterval, na.rm=TRUE)
maxIndex <- as.numeric(which(averageStepsPerInterval == maxIntervalDays))

## Plot a vertical line where the max is
maxInterval <- uniqueIntervals[maxIndex]
abline(v=maxInterval, col="red", lwd=3)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

From this, we can conclude, that the 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps, is:


```r
## Interval of the greatest number of steps:
maxInterval
```

```
## [1] 835
```
















## Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA's)


```r
isna<- is.na(data$steps)
sum(isna)
```

```
## [1] 2304
```


### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
meanStepsPerDay <- sapply(stepsSplit, mean, na.rm=TRUE)

## First modify the meanStepsPerDay vector that contains the mean number of steps taken for the 5-minute intervals. Each day consists of 288 intervals and there are 61 days in total. First, remove NA values and replace with 0. NaN values are produced when the entire day was filled with NA values. This shouldn't make any difference to the data, since the mean and median will be zero.
meanStepsPerDay[is.nan(meanStepsPerDay)] <- 0

## Now we create a daily data vector and replicate a vector 288 times. The reason why we're doing this is that the slots in the vector naturally line up with the interval for any particular day.  Now, all we have to do is find where in the data set there are missing values, and simply do a copy from one vector to the other.
meanColumn <- rep(meanStepsPerDay, 288)

## The steps before replacement
rawSteps <- data$steps

## Find any values that are NA in the rawSteps data
stepsNA <- is.na(rawSteps)

## Now replace these missing values with their corresponding mean values.
rawSteps[stepsNA] <- meanColumn[stepsNA]
```


### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
## newData is initially set equal to the old data set.
newData <- data

## However, the steps column is replaced by rawSteps.
newData$steps <- rawSteps
head(newData)
```

```
##      steps       date interval
## 1  0.00000 2012-10-01        0
## 2  0.43750 2012-10-01        5
## 3 39.41667 2012-10-01       10
## 4 42.06944 2012-10-01       15
## 5 46.15972 2012-10-01       20
## 6 53.54167 2012-10-01       25
```


### Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
## First we split up the newData frame for steps by day
stepsSplitNew <- split(newData$steps, dates$yday)

## Next find the total number of steps over each day. This time there shouldn't be any NA values, and so we don't need to set a flag.
totalStepsPerDayNew <- sapply(stepsSplitNew, sum)

## The mean and the median of the newData set is:
mean(totalStepsPerDayNew)
```

```
## [1] 10579.21
```

```r
median(totalStepsPerDayNew)
```

```
## [1] 10395
```

We see that, as expected, the mean has increased somewhat (1225 steps per day), while the median hasn't changed at all since only the lowest measurements were affected by NA's.

Next, we proceed to plot a histogram of the number of steps. To ease the comparison, we will both make the newData histogram, as well as remake the original.


```r
## Plot a histogram where the x-axis denotes the day and the y-axis denotes the total number of steps taken for each day.
par(mfcol=c(2,1))

## Plot the original histogram first
plot(uniqueDates, totalStepsPerDay, main="Histogram of steps taken each day before imputing", 
     xlab="Date (October to November 2012)", ylab="Frequency", type="h", lwd=4, col="blue")
## Plot the modified histogram after
plot(uniqueDates, totalStepsPerDayNew, main="Histogram of steps taken each day after imputing", 
     xlab="Date (October to November 2012)", ylab="Frequency", type="h", lwd=4, col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->















## Are there differences in activity patterns between weekdays and weekends?

Now we need to split up the data so that it's sorted by weekday or weekend. Since we started out by using the *strptime*, we have transformed the dates to a POSIXlt class, in which the weekday is stored in *wday*. wday is an integeger (0–6 day of the week, starting on Sunday). We will use this to assign a weekday to the activity data.


```r
wdays <- dates$wday

## Create a new factor variable that classifies the day as either a weekday or weekend. First, create a numeric vector with 2 levels (1 is for a weekday, 2 for a weekend).
classifywday <- rep(0, length(wdays)-1)

## For weekdays, set the numeric vector in these positions as 1
classifywday[wdays >= 1 & wdays <= 5] <- 1

## For weekend days, set the numeric vector in these positions as 2
classifywday[wdays == 6 | wdays == 0] <- 2

## Create a new factor variable that has labels Weekdays and Weekends.
daysFactor <- factor(classifywday, levels=c(1,2), labels=c("Weekdays", "Weekends"))

## Create a new column in the newData file that contains this factor for each day.
newData$typeOfDay <- daysFactor
```


With the factoring by weekday done, we move on to making a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

Since we have now factored our data according to weekday, we start of by calculating new mean values for the number of steps per interval. We will use the *aggregate* function to calculate the means while factoring for both the interval and the type of weekday. Then we make the actual plot, by using the *lattice* package. As a last comment, we see that the activity patterns are somewhat alike, though generally a little higher on weekend day.



```r
aggregatedData <- aggregate(steps ~ interval + typeOfDay, data = newData, FUN = "mean")

library(lattice)
xyplot(steps ~ interval | factor(typeOfDay),
       data = aggregatedData,
       type = "l",
       aspect = .2,
       xlab = "Time Interval of the day",
       ylab = "Number of steps in interval",
       main = "Average number of steps per interval across all weekends and week days")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->







