library(plyr)
dfwna=read.csv('activity.csv')
df=subset(dfwna,steps!='NA')
any(is.na(df))

dailytotals=ddply(df, c('date'), summarise, totals=sum(steps))
png('plot1.png')
  hist(dailytotals$totals, col='green', xlab='daily-totals steps (ingoring NAs)', ylab='count of days with the given daily-total')
dev.off()

#dailyaves=ddply(df, c('date'), summarise, mean=mean(steps))
dailyaves=ddply(df, c('date'), summarise, mean=mean(steps), median=median(steps))
png('plot2.png')
  plot(dailyaves$date, dailyaves$mean, xlab='date', ylab='daily averages of steps for 5-minute intervals (omitting NAs)')
  lines(dailyaves$date, dailyaves$mean)
dev.off()

intervalaves=ddply(df, c('interval'), summarise, mean=mean(steps))
png('plot3.png')
  plot(intervalaves$interval, intervalaves$mean, type='l', col='red', xlab='interval', ylab='mean steps')
dev.off()

maxi=subset(intervalaves, mean==max(intervalaves$mean))
maxi

dfim=dfwna
dfim$steps[is.na(dfim$steps)] = with(dfim, ave(steps, interval, FUN = function(x) mean(x, na.rm=T)))[is.na(dfim$steps)]
any(is.na(dfim))

dailytotalsim=ddply(dfim, c('date'), summarise, totals=sum(steps))
png('plot4.png')
  hist(dailytotalsim$totals, col='blue', xlab='daily-total steps (NAs replaced by interval-means)', ylab='count of days with the given daily-total')
dev.off()

dfwd=mutate(df, wd=timeDate::isWeekday(date))
dfwk=subset(dfwd,wd==T)
dfwe=subset(dfwd,wd==F)

png(filename='plot5.png')
  par(mfrow=c(2,1))
  intervalaves=ddply(dfwk, c('interval'), summarise, mean=mean(steps))
  plot(intervalaves$interval, intervalaves$mean, 'l', col='blue', xlab='interval', ylab='Mon-Fri mean steps',lwd=3)
  intervalaves=ddply(dfwe, c('interval'), summarise, mean=mean(steps))
  plot(intervalaves$interval, intervalaves$mean, 'l', col='green', xlab='interval', ylab='Sat-Sun mean steps',lwd=3)
dev.off()

