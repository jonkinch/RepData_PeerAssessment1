#clear the environment
rm(list=ls(all=TRUE)) 

#Create a temp file for the zip file
temp <- tempfile()

#Load zip into temp
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)

#Read the activity.csv file in the downloaded zip file
actraw <- read.csv(unz(temp,"activity.csv"))

#Remove the temp file
rm("temp")

#Create the histogram
hist(tapply(actraw$steps, actraw$date, sum), breaks=20, main="Frequecy of the Total Number of Steps by Day", xlab="Total number of steps", xlim=c(0,25000), col="yellow")

#Save Plot File
dev.copy(png, file="Histogram_v1.png", width=480, height=480)
graphics.off()

#Sum by day and convert to numeric
daily_steps <- as.numeric(tapply(actraw$steps, actraw$date, sum))

#Calculate the Mean and Median and round to nearest whole number
daily_mean <- round(mean(daily_steps, na.rm=TRUE))
daily_median <- round(median(daily_steps, na.rm=TRUE))
daily_mean
daily_median

#Create a factor on interval so we can use it in a data frame shortly
actraw$interval <- as.factor(as.character(actraw$interval))

#Calculate the mean and remove all NA values
interval_mean <- as.numeric(tapply(actraw$steps, actraw$interval, mean, na.rm=TRUE))

#Create a dataframe based on interval_mean using the interval factor
interval_data <- data.frame(intervals = as.numeric(levels(actraw$interval)), interval_mean)

#Make sure the data is in the correct order
interval_data <- interval_data[order(interval_data$intervals),]

#Create the plot and add an axis label for clarity
plot(interval_data$intervals, interval_data$interval_mean, type = "l", main = "Average steps 5-minute interval", ylab = "Average steps", xlab = "Time of day", xaxt = "n")
axis(side = 1, at = seq(0, 2400,400), labels = c("00:00", "04:00", "08:00", "12:00", "16:00","20:00","24:00"))

#Save Plot File
dev.copy(png, file="IntervalAvgSteps.png", width=480, height=480)
graphics.off()

#Get the top values
head(interval_desc, 1)

#Dim to determine NA count
dim(actraw[is.na(actraw$steps),])[1]

#Create a loop to go through and update all the NA values 
steps <- vector()
for (i in 1:dim(actraw)[1]) {
    if (is.na(actraw$steps[i])) {
        steps <- c(steps, interval_data$interval_mean[interval_data$intervals == actraw$interval[i]])
    } else {
        steps <- c(steps, actraw$steps[i])
    }
}


act_nona <- data.frame(steps = steps, date = actraw$date, interval = actraw$interval)

#create the histogram
hist(tapply(act_nona$steps, act_nona$date, sum), xlab = "Total Daily Steps", breaks = 20, main = "Frequency of Total Steps Taken per Day no NA", xlim = c(0,25000), ylim = c(0,20), col = "cyan")

#Save Plot File
dev.copy(png, file="Histogram-NoNA.png", width=480, height=480)
graphics.off()


#Sum by day and convert to numeric
daily_steps_v2 <- as.numeric(tapply(act_nona$steps, act_nona$date, sum))

#Calculate the Mean and Median and round to nearest whole number
daily_mean <- round(mean(daily_steps_v2, na.rm=TRUE))
daily_median <- round(median(daily_steps_v2, na.rm=TRUE))
daily_mean
daily_median

#Adding the day_type and making it a factor
act_nona$day_type <- c("weekend", "weekday", "weekday", "weekday", "weekday", "weekday", "weekend")[as.POSIXlt(act_nona$date)$wday + 1]
act_nona$day_type <- as.factor(act_nona$day_type)

weekday <- act_nona[act_nona$day_type == "weekday", ]
weekend <- act_nona[act_nona$day_type == "weekend", ]
weekday_means <- as.numeric(tapply(weekday$steps, weekday$interval, mean))
weekend_means <- as.numeric(tapply(weekend$steps, weekend$interval, mean))
intervals_dt <- data.frame(intervals = as.numeric(levels(actraw$interval)), weekday_means, weekend_means)
intervals_dt <- intervals_dt[order(intervals_dt$intervals),  ]

#Creating a plot for the Weekday and Weekend comparison
plot(intervals_dt$intervals, intervals_dt$weekday_means, type = "l", col = "red", ylab = "Average steps", xlab = "Time of day", main = "Step Comparison Between Weekdays and Weekends", xaxt = "n")
axis(side = 1, at = seq(0, 2400,400), labels = c("00:00", "04:00", "08:00", "12:00", "16:00","20:00","24:00"))
lines(intervals_dt$intervals, intervals_dt$weekend_means, type = "l", col = "blue")
legend(1500, 230, c("Weekend", "Weekday "), lty = c(1, 1), lwd = c(1, 1), col = c("blue", "red"))

#Save Plot File
dev.copy(png, file="WeekendvsWeekday.png", width=480, height=480)
graphics.off()
	