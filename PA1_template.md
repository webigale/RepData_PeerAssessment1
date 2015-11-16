# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv", 
                     colClasses = c("numeric", "Date", "numeric"))
library(reshape2)
int_date <- dcast(activity, interval ~ date, value.var = "steps")
date_int <- dcast(activity, date ~ interval, value.var = "steps")
```


## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day  

```r
date_sum <- as.data.frame(colSums(int_date[2:62], na.rm = TRUE))
colnames(date_sum) <- "steps"
date_sum$date <- rownames(date_sum)
```

2. Make a histogram of the total number of steps taken each day   

```r
hist(date_sum$steps, xlab = "Total steps per day", main = "")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day  

```r
mean(date_sum$steps)
```

```
## [1] 9354.23
```

```r
median(date_sum$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

```r
int_avg <- as.data.frame(colMeans(date_int[2:289], na.rm = TRUE))
colnames(int_avg) <- "steps"
int_avg$interval <- rownames(int_avg)
with(int_avg, plot(interval, steps, type = "l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
int_avg$interval[which.max(int_avg$steps)]
```

```
## [1] "835"
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. 
   Use the mean for the 5-minute interval

```r
for (i in which(sapply(date_int, is.numeric))) {
    date_int[is.na(date_int[, i]), i] <- round(mean(date_int[, i],                                                    na.rm = TRUE))
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.  

```r
new_df <- melt(date_int, id = "date", variable.name = "interval", 
               value.name = "steps")
new_df <-new_df[c("steps", "date", "interval")]
new_df <- new_df[order(new_df$date, new_df$interval), ]
rownames(new_df) <- c(1:17568)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  
   Because I used the mean to impute the missing data, the mean and median are the same and the histogram shows a higher peak as the mean.

```r
date_sum_new <- as.data.frame(colSums(int_date[2:62], na.rm = TRUE))
colnames(date_sum_new) <- "steps"
date_sum_new$date <- rownames(date_sum_new)
hist(date_sum_new$steps, xlab = "Total steps per day", main = "With imputed data")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
mean(date_sum_new$steps)
```

```
## [1] 9354.23
```

```r
median(date_sum_new$steps)
```

```
## [1] 10395
```

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.  

```r
new_df$weekday <- as.factor(ifelse(weekdays(new_df$date) %in% 
                            c("Saturday", "Sunday"), "weekend", "weekday"))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.  

```r
date_int_we <- dcast(new_df[which(new_df$weekday == "weekend"), ], 
                     date ~ interval, value.var = "steps")
int_avg_we <- as.data.frame(colMeans(date_int_we[2:289], na.rm = TRUE))
colnames(int_avg_we) <- "steps"
int_avg_we$interval <- rownames(int_avg_we)
int_avg_we$weekday <- "weekend"
date_int_wd <- dcast(new_df[which(new_df$weekday == "weekday"), ], 
                     date ~ interval, value.var = "steps")
int_avg_wd <- as.data.frame(colMeans(date_int_wd[2:289], na.rm = TRUE))
colnames(int_avg_wd) <- "steps"
int_avg_wd$interval <- rownames(int_avg_wd)
int_avg_wd$weekday <- "weekday"
int_avg_new <- rbind(int_avg_we, int_avg_wd)
library(lattice)
with(int_avg_new, xyplot(steps ~ interval | weekday, layout = c(1, 2), 
                         type = "l", xlab = "Interval", 
                         ylab = "Number of steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
