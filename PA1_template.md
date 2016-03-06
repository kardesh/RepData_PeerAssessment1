# PA1_template
Kartik Deshmukh  
March 6, 2016  
### Reproducible Research Assignment 1

#### Mean and Median of Total number of steps taken per day


```r
# Reading activity data
activity <- read.csv("activity.csv", 
                      header = TRUE, sep = ",", na.strings = "NA")

# Converting into date and time format
act_date <- as.Date(activity$date)

daily_steps <- sapply(split(activity,activity$date), 
                    function(x) x <- {return (sum(x$steps, na.rm = TRUE))})

#Plotting Histogram
hist(daily_steps, col = "red", 
     ylab = "Frequency", xlab = "Daily Steps", breaks = 25, main = "Total Steps Distribution")
```

![](PA1_template_files/figure-html/Daily mean and median-1.png)

```r
daily_mean <- as.integer(round(mean(daily_steps), 2))
daily_median <- median(daily_steps)
```
The mean of total number of steps taken per day is **9354**.  
The median of total number of steps taken per day is **10395**.

#### Average Daily Activity Pattern

```r
avg_step_int <- sapply(split(activity,activity$interval), 
                    function(x) x <- {return (mean(x$steps, na.rm = TRUE))})

intervals <- unique(activity$interval)
plot(intervals, avg_step_int, type = "l",
     ylab = "Avg. Steps", 
     xlab = "Intervals")
```

![](PA1_template_files/figure-html/Average Daily Activity pattern-1.png)

```r
max_interval <- intervals[match(max(avg_step_int), avg_step_int)]
```
The 5 minute interval containing maximum number of steps is **835**.

#### Inputting missing values and the effect on dataset.

```r
no_values <- sum((is.na(activity$steps) == TRUE))

for (i in which(is.na(activity$steps))) {
  activity[i,1] <- avg_step_int[match(activity[i,3], intervals)]
}

# Revised total daily steps
daily_steps <- sapply(split(activity,activity$date), 
                    function(x) x <- {return (sum(x$steps, na.rm = TRUE))})

#Plotting Histogram
hist(daily_steps, col = "red", 
     ylab = "Frequency", xlab = "Daily Steps", breaks = 25, main = "Total Steps Distribution")
```

![](PA1_template_files/figure-html/Missing Values-1.png)

```r
daily_mean <- as.integer(round(mean(daily_steps), digits = 3))
daily_median <- as.integer(round(median(daily_steps), digits = 3))
```
Total rows of missing values are **2304**.  

The new mean of total number of steps taken per day is **10766**.    

The new median of total number of steps taken per day is **10766**.  

These new values differ from their earlier estimates after filling missing data values in data set with the means for their particular intervals.  

Also, the revised histogram has smoothed out due to filling the missing values.

#### Weekdays and Weekend Activity difference

```r
library(lattice)
fac_days <- factor ((weekdays(act_date, abbreviate = FALSE) %in% c('Monday',   'Tuesday', 'Wednesday', 'Thursday', 'Friday')), 
                    levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

activity_new <- cbind(activity, fac_days)

# Splitting data by weekdays and weekends
act_weeks <- split(activity_new, activity_new$fac_days)

#Calculating mean average for intervals in weekdays
avg_step_int_wday <- sapply(split(act_weeks$weekday, act_weeks$weekday$interval), 
                       function(x) x <- {return (list( mean = mean(x$steps, na.rm = TRUE),
                                                       interval = x[1,3], week = x[1,4]))})

#Calculating mean average for intervals in weekends
avg_step_int_wend <- sapply(split(act_weeks$weekend, act_weeks$weekend$interval), 
                       function(x) x <- {return (list(mean = mean(x$steps, na.rm = TRUE),
                                                      interval = x[1,3], week = x[1,4]))})
# Inverting columns and rows of data frame
avg_step_int_wday <- as.data.frame(t(avg_step_int_wday))
avg_step_int_wend <- as.data.frame(t(avg_step_int_wend))

act_final <- rbind(avg_step_int_wend, avg_step_int_wday)

week_type <- factor(act_final$week, levels = c(1,2), labels = c("Weekdays", "Weekends"))

xyplot(act_final$mean~act_final$interval|week_type,xlab = "interval", ylab = "No. of Steps",
       layout = c(1,2), panel = function(x, y) { panel.loess(x, y) })
```

![](PA1_template_files/figure-html/weekday and weekend activity-1.png)

