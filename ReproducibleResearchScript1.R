library(dplyr)
setwd("E:/Coursera/Reproducible Research/repdata%2Fdata%2Factivity/")
dat <- read.csv("activity.csv", header=T)
bydate <- group_by(dat,date)

sumCount <- summarize(bydate, s=sum(steps))
hist(sumCount$s, breaks = c(seq(0, 25000, 2500)), col = "blue", main = "Step Count Frequency", xlab = "Steps Taken")

sumMeanMed <- summarize(bydate, mn=mean(steps), med=median(steps))

byInterval <- group_by(dat, interval)
sumMean_interval <- summarize(byInterval, steps = mean(steps, na.rm = T))
with(sumMean_interval, plot(steps ~ interval, type = "l", main = "Mean Steps per Interval", xlab = "Daily Interval", ylab = "Steps Taken"))

sumMean_interval$interval[which(sumMean_interval$steps == max(sumMean_interval$steps))]

length(which(is.na(dat) == TRUE))

dat2 <- dat
for(i in 1:dim(dat2)[1]){
  if(is.na(dat2$steps[i]) == TRUE){
    dat2$steps[i] <- sumMean_interval$steps[which(sumMean_interval$interval == dat2$interval[i])]
  }
}

bydate2 <- group_by(dat2,date)

sumCount2 <- summarize(bydate2, s=sum(steps))
hist(sumCount2$s, breaks = c(seq(0, 25000, 2500)), col = "blue", main = "Step Count Frequency", xlab = "Steps Taken")

sumMeanMed2 <- summarize(bydate2, mn=mean(steps), med=median(steps))

dat2$day <- weekdays(ymd(as.character(dat2$date))); dat2$weekday <- NA
for(i in 1:dim(dat2)[1]){
  if(dat2$day[i] == "Saturday" || dat2$day[i] == "Sunday"){
    dat2$weekday[i] <- "weekend"
  } else {dat2$weekday[i] <- "weekday"}
}
dat2$weekday <- as.factor(dat2$weekday)
by_weekday <- group_by(dat2, interval, weekday)
sumMean_interval_weekday <- summarize(by_weekday, steps = mean(steps))

par(mfrow=c(2,1))

with(sumMean_interval_weekday[sumMean_interval_weekday$weekday == "weekday",],  plot(steps ~ interval, main = "Steps Taken (Weekday)", ylim = c(0, 240), type = "l"))
with(sumMean_interval_weekday[sumMean_interval_weekday$weekday == "weekend",],  plot(steps ~ interval, main = "Steps Taken (Weekend)", ylim = c(0, 240), type = "l"))
