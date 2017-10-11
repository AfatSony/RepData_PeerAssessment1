---
output:
  html_document: default
  pdf_document: default
---
  ## Loading and preprocessing the data
  ```{r}
library(dplyr)
library(ggplot2)
activity.data <- read.csv("activity.csv")
str(activity.data)
summary(activity.data)
head(activity.data,3)
act.data.completecase <- na.omit(activity.data)
head(act.data.completecase,3)
```


##What is mean total number of steps taken per day?

```{r}
activity.day <- group_by(act.data.completecase, date)
activity.day <- summarize(activity.day, steps=sum(steps))
summary(activity.day)
qplot(steps, data=activity.day)
mean(activity.day$steps)
median(activity.day$steps)
```

## What is the average daily activity pattern?
```{r}
activity.interval <- group_by(act.data.completecase, interval)
activity.interval <- summarize(activity.interval, steps=mean(steps))
ggplot(activity.interval, aes(interval, steps)) + geom_line()
activity.interval[activity.interval$steps==max(activity.interval$steps),]

```


## Imputing missing values
```{r}
nrow(activity.data)-nrow(act.data.completecase)
names(activity.interval)[2] <- "mean.steps"
activity.impute <- merge(activity.data, activity.interval)
activity.impute$steps[is.na(activity.impute$steps)] <- activity.impute$mean.steps[is.na(activity.impute$steps)]
```


## Are there differences in activity patterns between weekdays and weekends?
```{r}
activity.impute$dayofweek <- weekdays(as.Date(activity.impute$date))
activity.impute$weekend <-as.factor(activity.impute$dayofweek=="Saturday"|activity.impute$dayofweek=="Sunday")
levels(activity.impute$weekend) <- c("Weekday", "Weekend")

activity.weekday <- activity.impute[activity.impute$weekend=="Weekday",]
activity.weekend <- activity.impute[activity.impute$weekend=="Weekend",]

act.int.weekday <- group_by(activity.weekday, interval)
act.int.weekday <- summarize(act.int.weekday, steps=mean(steps))
act.int.weekday$weekend <- "Weekday"
act.int.weekend <- group_by(activity.weekend, interval)
act.int.weekend <- summarize(act.int.weekend, steps=mean(steps))
act.int.weekend$weekend <- "Weekend"

activity.interval <- rbind(act.int.weekday, act.int.weekend)
activity.interval$weekend <- as.factor(activity.interval$weekend)
ggplot(activity.interval, aes(interval, steps)) + geom_line() + facet_grid(weekend ~ .)
```