## Loading and preprocessing the data

library(lubridate)
steps <- read.csv("./data/activity.csv")
steps$date <- ymd(steps$date)
good <- complete.cases(steps)
goodsteps <- steps[good,]
dailySteps <- aggregate(goodsteps$steps,by=list(goodsteps$date), FUN=sum)

## What is mean total number of steps taken per day?
hist(goodsteps$steps,main="Histograms of Steps per Day",xlab = "Steps",ylab = "Days")
origMean <- mean(goodsteps$steps)
origMedian <- median(goodsteps$steps)
paste("Mean steps per day:",round(origMean,digits = 4))
paste("Median steps per day:",round(origMedian,digits=4))



## What is the average daily activity pattern?
activity <- aggregate(goodsteps$steps,by = list(goodsteps$interval), FUN = mean)
names(activity) <- c("interval","steps")
plot(activity$steps~activity$interval,type="l", 
     main="Average Steps by Time Interval", xlab="Interval",ylab="Avg. Steps")
paste("Interval with Max Avg Steps Per Day:", activity[which.max(activity$steps),"interval"])


## Imputing missing values
bad <- is.na(steps$steps)
paste("Missing Data in DataSet:",sum(bad),"Records")
imputedsteps <- steps
imputedsteps$goodsteps <- imputedsteps$steps
for(j in 1:nrow(imputedsteps)) {
    if (is.na(imputedsteps[j,"steps"])) {
        interval <- imputedsteps[j,"interval"]
        fixedsteps <- activity[activity$interval == interval,"steps"]
        imputedsteps[j,"goodsteps"] <- fixedsteps
    } 
}
hist(imputedsteps$goodsteps,main="Adjusted Histograms of Steps per Day",xlab = "Steps",ylab = "Days")
newMean <- mean(imputedsteps$goodsteps)
newMedian <- median(imputedsteps$goodsteps)
meanDiff <- origMean - newMean
medianDiff <- origMedian - newMedian
paste("Adjusted Mean steps per day:",round(newMean,digits = 4))
paste("Adjusted Median steps per day:",round(newMedian,digits=4))
paste("Difference in Means:",meanDiff)
paste("Difference in Medians:",medianDiff)

origSD <- sd(steps$steps,na.rm = TRUE)
newSD <- sd(imputedsteps$goodsteps)
paste("SD of Original Steps:",round(origSD,digits = 4))
paste("SD of Adjusted Steps:",round(newSD,digits = 4))

# WIP
library(lattice)
imputedsteps$daytype <- factor(ifelse(weekdays(imputedsteps$date,abbreviate = TRUE) %in% 
                                          c("Sat","Sun"),"weekend","weekday"))
xyplot(imputedsteps$goodsteps~imputedsteps$interval|imputedsteps$daytype,type="l")
