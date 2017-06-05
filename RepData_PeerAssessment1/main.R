library(lubridate)
library(lattice)

##Question 1
##loading and processing the data
unzip("activity.zip", exdir = ".")
activity <- read.csv("activity.csv", header=TRUE, stringsAsFactors = FALSE, na.strings = "NA")
activity$date <- ymd(activity$date) 



#aggregating the data set by Day
totalsteps <- aggregate(activity$steps, by = list(date = activity$date), FUN=sum, na.rm=TRUE)

## Drawing the plot
hist(totalsteps$x, 
     breaks = 15, 
     xlab = "Total Number of Steps per Day", 
     main = "", 
     xlim = c(0,23000),
     xaxt = 'n')
axis(side =1, at =seq(0,23000,2000), labels = seq(0,23000,2000))

##Adding the mean and median to the plot
summary(totalsteps$x, mean, median)
abline(v = mean(totalsteps$x), col="red")
abline(v=median(totalsteps$x), col="blue")
legend("topright", inset =.02, legend=c("Mean", "Median"), col=c("red","blue"), lty=1, box.lty=0)

##Question 2
## What is the Average Daily Activity Pattern
##Average steps per 5 min interval
avesteps<-aggregate(activity$steps, by = list(interval=activity$interval), FUN=mean, na.rm=TRUE)
plot(avesteps$interval, avesteps$x, type ="l", ylab = "Average Steps", xlab ="Interval")
abline(v= with(avesteps, interval[x == max(x)]), col="red")

##Question 3
##Imputing missing values
sum(is.na(activity$steps))

activity2 <- activity
i <- 1

while (i <= length(activity2$steps)) {
        if (is.na(activity2$steps[i]))
            {
                activity2$steps[i] <- avesteps$x[avesteps$interval == activity2$interval[i]]
                i = i + 1
        }
        else {i = i + 1}       
        
}

sum(is.na(activity2$steps))


##Plotting Total Steps 2 
totalsteps2 <- aggregate(activity2$steps, by = list(date = activity$date), FUN=sum)
hist(totalsteps2$x, 
     breaks = 15, 
     xlab = "Total Number of Steps per Day", 
     main = "", 
     xlim = c(0,23000),
     xaxt = 'n')
axis(side =1, at =seq(0,23000,2000), labels = seq(0,23000,2000))

##Adding the mean and median to the plot
summary(totalsteps2$x, mean, median)
abline(v = mean(totalsteps2$x), col="red")
abline(v=median(totalsteps2$x), col="blue")
legend("topright", inset =.02, legend=c("Mean", "Median"), col=c("red","blue"), lty=1, box.lty=0)

##Question 4
##Differences in activity between weekends and weeekdays

activity2$daytype <- NULL
i <- 1

while (i <= length(activity2$date)) {
        if (wday(activity2$date[i]) == 1  || wday(activity2$date[i]) == 7)
        {
                
                activity2$daytype[i] <- "weekend"
                i = i + 1
        }
        else {
                activity2$daytype[i] <- "weekday"
                i = i +1
        }       
        
}

##plotting 2 panel plots

par(mfrow=c(2,1))
avesteps2<-aggregate(activity2$steps, by = list(interval=activity2$interval, daytype = activity2$daytype), FUN=mean)
xyplot(x~interval | daytype, data = avesteps2 , type="l", layout=c(1,2))




