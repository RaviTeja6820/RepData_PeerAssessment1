
## Course Project 1

Reading CSV data in R


```r
activityDT <- data.table::fread(input = "activity.csv")
library(ggplot2)
library(data.table)
library(knitr)
```

**Mean total number of steps taken per day (with missing values)**


```r
total <- tapply(activityDT$steps, activityDT$date, sum)
date <- names(total)
steps <- data.frame(total)[[1]]
res <- data.frame(date, steps)
ggplot(res, aes(x = steps)) + geom_histogram(fill = "red", binwidth = 1000) + labs(title = "Steps per Day", x = "steps", y = "frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![plot of chunk mmi1](figure/mmi1-1.png)

```r
mmi <- summary(res$steps, na.rm = TRUE)[c(3,4)]
mmi <- data.table(mmi[[1]], mmi[[2]])
colnames(mmi) <- c("Mean", "Median")
kable(mmi, caption = "Mean and Median with missing values")
```



|  Mean|   Median|
|-----:|--------:|
| 10765| 10766.19|


```r
intervalDT <- activityDT[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c("steps") ,by = .(interval)]
ggplot(intervalDT, aes(x = interval , y = steps)) + geom_line(color = "red", size = 0.2) + labs(title = "Average daily activity pattern", y = "Average Steps per Day")
```

![plot of chunk mmi2](figure/mmi2-1.png)

```r
kable(intervalDT[steps == max(steps)], caption = "Interval with maximum steps")
```



| interval|    steps|
|--------:|--------:|
|      835| 206.1698|



```r
activityDT[is.na(steps), .N]
```

```
## [1] 2304
```

```r
med <- data.table(median(activityDT$steps))
colnames(med) <-c("steps")
activityDT[is.na(steps), "steps"] <- med
data.table::fwrite(activityDT, file = "cleanData.csv", quote = FALSE)
```

**Mean total number of steps taken per day (without missing values)**


```r
totalSteps <- activityDT[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("steps"), by = .(date)]
ggplot(totalSteps, aes(x = steps)) + geom_histogram(fill = "red", binwidth = 1000) + labs(title = "Steps per Day", x = "steps", y = "frequency")
```

![plot of chunk mmf1](figure/mmf1-1.png)

```r
mmf <- data.table(mean(totalSteps$steps), median(totalSteps$steps))
colnames(mmf) <- c("Mean", "Median")
```

**Comparing mean and medians after and before adding the missing values**


|     Mean|   Median|
|--------:|--------:|
|  9354.23| 10395.00|
| 10765.00| 10766.19|


```r
activityDT$day <- weekdays(as.Date(activityDT$date, format = "%Y-%m-%d"))
activityDT$weekendorday <- "weekday"
activityDT[grep("Saturday|Sunday", activityDT$day)]$weekendorday <- "weekend"
activityDT$weekendorday <- as.factor(activityDT$weekendorday)
```


```r
intervalDT <- activityDT[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c("steps") ,by = .(interval, weekendorday)]
ggplot(intervalDT, aes(x = interval , y = steps, col = weekendorday)) + geom_line(size = 0.05) + labs(title = "Average daily activity pattern by weekend or weekday", y = "Average Steps per Day") + facet_wrap(facets = .~weekendorday, ncol = 1)
```

![plot of chunk wdoegraph](figure/wdoegraph-1.png)
