library(chron)
library(lattice)
activity = read.csv("./activity.csv")

#remove NA observations
complete = activity[complete.cases(activity),]

#sum of steps by date
sumStepsDay = aggregate(activity$steps, 
                        by=list(date=activity$date), 
                        FUN = sum)
#histogram of steps per day frequency
hist(sumStepsDay$x, 
     breaks = 10, 
     main = "Frequency of sum of steps in a day", 
     xlab = "Steps in day")

# mean and median of sum of steps per day
mean(sumStepsDay$x, na.rm = T)
median(sumStepsDay$x, na.rm = T)

# average steps taken across all days by interval 
avgStepsInterval = aggregate(activity$steps, 
                             by=list(interval=activity$interval), 
                             FUN = mean, 
                             na.rm = T)
# plot average steps by interval
plot(avgStepsInterval$interval, avgStepsInterval$x, type = "l")

# sort by avg steps descending, view interval with highest avg steps
sorted = avgStepsInterval[with(avgStepsInterval, order(-x)),]
sorted[1,1]

# return the number of NA values in dataset
sum(is.na(activity))

# function to replace NA steps values in activity with avg steps 
# in time interval
replaceNa = function(df1, df2) {
    for(i in 1:nrow(df1)) {
        if(is.na(df1[i,1])) {
            interval = df1[i, 3]
            row = which(df2$interval == interval)
            df1[i, 1] = as.integer(df2[row, 2])
        }
    }
    df1
}

naReplaced = replaceNa(activity, avgStepsInterval)

#sum of steps by date with replaced NA values
sumStepsDay2 = aggregate(naReplaced$steps, 
                        by=list(date=naReplaced$date), 
                        FUN = sum)

hist(sumStepsDay2$x, 
     breaks = 10, 
     main = "Frequency of sum of steps in a day (NA replaced)", 
     xlab = "Steps in day")

# mean and median of sum steps by day with replaced NA values
mean(sumStepsDay2$x)
median(sumStepsDay2$x)

# add date format column and logical factor for weekend
naReplaced$asDate = as.Date(naReplaced$date)
naReplaced$weekend = chron::is.weekend(naReplaced$asDate)
naReplaced$weekend[naReplaced$weekend == "TRUE"] = "Weekend"
naReplaced$weekend[naReplaced$weekend == "FALSE"] = "Weekday"
naReplaced$weekend = factor(naReplaced$weekend)

naReplacedInterval = aggregate(naReplaced$steps, 
                             by=list(interval=naReplaced$interval, 
                                     weekend=naReplaced$weekend), 
                             FUN = mean)

#create lattice plot of average steps per interval by weekend
xyplot(x ~ interval | weekend, 
       data = naReplacedInterval, 
       layout = c(1,2), 
       type = "l")