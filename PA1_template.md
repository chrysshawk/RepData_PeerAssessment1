# Reproducible Research: Peer Assessment 1
Christoffer Haukvik  


## Loading and preprocessing the data
First, the data is extracted and loaded to a dataframe from the given "activity.zip" file.

```r
act <- read.csv(unzip("activity.zip"))
act$date <- as.Date(as.character(act$date), "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

1. Calculating the total steps per day

```r
dailyAct <- aggregate(act$steps, by = list(act$date), FUN = sum)
names(dailyAct) <- c("Date", "Steps")
```

2. Plotting the total number of steps per day

```r
library(ggplot2)
ggplot(data = dailyAct, aes(x=Date, y=Steps)) +
     geom_col(col="black", fill = "blue", 
              width = 1, alpha=.5) +
     theme_light() +
     labs(title = "Total steps per day")
```

![](PA1_template_files/figure-html/stepHistogram-1.png)<!-- -->

3. Calculating the mean and median steps per day

```r
dailyMean <- mean(dailyAct$Steps, na.rm = TRUE)
dailyMedian <- median(dailyAct$Steps, na.rm = TRUE)
```
Mean steps per day are 1.0766189\times 10^{4}.  
Median steps per day are 10765.

## What is the average daily activity pattern?

1. Time series plot of the 5-minute intervals and mean steps taken

```r
intervalMean <- aggregate(act$steps, by = list(act$interval), 
                         FUN = mean, na.rm = TRUE)
names(intervalMean) <- c("Interval", "Steps")
ggplot(data = intervalMean, aes(x=Interval, y=Steps)) +
     geom_line(colour="blue", size = 1) +
     theme_light() + 
     labs(title = "Mean steps per 5-min interval")
```

![](PA1_template_files/figure-html/activityPattern-1.png)<!-- -->

2. Which 5-minute interval on average contains the max number of steps?

```r
intervalMax <- intervalMean[which.max(intervalMean$Steps), ]
```
The 5-minute interval 835 has the most steps (206 steps).

## Imputing missing values
1. Calculating and reporting the total number of missing values (NAs) in the dataset.

```r
length(is.na(act))
```

```
## [1] 52704
```

2 & 3. Filling in the missing values in the dataset using mean of the given 5-minute interval

```r
# Creating a new dataset for complete interval measurements
cAct <- act

# Determining NAs and replacing with means for the given interval
for (i in 1:nrow(cAct)) {
     if (is.na(cAct$steps[i])) {
          # Rounding the steps with 0 decimals
          cAct$steps[i] <- 
               round(intervalMean$Steps[intervalMean$Interval == 
                                       cAct$interval[i]], 0)
     }
}
```

4a. Making a histogram of the total number of steps each day


```r
# Calculating total steps with complete dataset
cDailyAct <- aggregate(cAct$steps, by = list(cAct$date), FUN = sum)
names(cDailyAct) <- c("Date", "Steps")

# Calculating and reporting the mean and median steps per day
cDailyMean <- mean(cDailyAct$Steps)
cDailyMedian <- median(cDailyAct$Steps)

# Plotting total steps with complete dataset, incl daily mean & median
ggplot(data = cDailyAct, aes(x=Date, y=Steps)) +
     geom_col(col="black", fill = "blue", 
              width = 1, alpha=.5) +
     geom_hline(aes(yintercept = cDailyMean, linetype = "Daily mean"), 
                    col="red", size = 1) +
     theme_light() +
     scale_linetype_manual(name = "", values = c(3,1), guide =
                                guide_legend(override.aes =
                                                  list(color="red"))) +
     labs(title = "Total steps per day")
```

![](PA1_template_files/figure-html/histTotals-1.png)<!-- -->

4b. The impact of imputing the NAs in the dataset is that the mean has changed to 1.0765639\times 10^{4} (from 1.0766189\times 10^{4}) and the median has changed to 1.0762\times 10^{4} (from 10765).

## Are there differences in activity patterns between weekdays and weekends?
