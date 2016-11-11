##Load data ( working directory set accordingly )

data <- read.csv("activity.csv")


##===========================================
##Steps each day 

library(ggplot2)
tot.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(tot.steps, binwidth=500, xlab="Tot no of steps taken each day")
mean(tot.steps, na.rm=TRUE)
median(tot.steps, na.rm=TRUE)



##===========================================
##5 min interval / avg across days 

library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-min interval") +
  ylab("average number of steps taken( Avg across days") +
  ggtitle("Average daily activity pattern")
averages[which.max(averages$steps),]

##=============================================

## Missing 

missing <- is.na(data$steps)
summary(missing)

##============================================

## Adding missing values 

fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

##===histogram

total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=500, xlab="total number of steps taken each day") +
  ggtitle("Report of mean and median for tot no of steps taken per day")
mean(total.steps)
median(total.steps)

##===========================================


##activity patterns between weekdays and weekends

weekday.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.weekend)


averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps") + 
  ggtitle("Activity patterns between weekdays and weekends") 
