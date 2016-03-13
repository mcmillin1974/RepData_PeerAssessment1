#unzip the "activity.zip" file and read the contents into a variable "actdat" with read.csv
actdat <- read.csv(unz("activity.zip", "activity.csv"))

#loaded dplyr library
library(dplyr)
library(ggplot2)

#NEW QUESTION: What is mean total number of steps taken per day?

#calculate the total number of steps per day
totstepsd <- actdat %>% na.omit() %>% group_by(date) %>% summarise(total = sum(steps))

#Make a histogram of the total number of steps taken each day
ggplot(totstepsd, aes(x = total)) + geom_histogram(binwidth = 600, aes(fill = ..count..)) + scale_x_continuous(breaks = seq(0,25000, by = 2500))

#Output the histogram to a png file called "hist1.png"
ggsave(filename = "hist1.png")

#Calculate the mean and median number of steps taken per day
meansteps_day <- mean(totstepsd$total)
medsteps_day <- median(totstepsd$total)

#the median number of steps is 10765 and the mean is 10766.19

#NEW QUESION: What is the average daily activity pattern?

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

avg5int <- actdat %>% na.omit() %>% group_by(interval) %>% summarise(avg = mean(steps))

#Plot the line graph
ggplot(data = avg5int, mapping = aes(x = interval, y = avg)) + geom_line() + scale_x_continuous(breaks = seq(0, 2400, by = 100)) + xlab("5 minute intervals") + ylab("Average steps per day") + ggtitle("Average Steps Per Day Per 5 Minute Interval")

#Save the graph as "time1.png"
ggsave(filename = "time1.png")

#Determine which 5 minute interval contains the maximum number of steps
maxsteps <- avg5int %>% filter(avg == max(avg))

#The interval with the maximun number of steps on average across all days is: 835

#NEW QUESTION:  Imputing missing values

#Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

#Calculate the total number of NA fields
countna <- actdat %>% group_by(steps) %>% summarise(num = n()) %>% filter(is.na(steps))

#The number of NA fields is 2304

#Impute the NA fields with the mean of steps for each grouped interval
impactdat <- actdat %>% group_by(interval) %>% mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))

#calculate the total number of steps per day
totstepsd_imp <- impactdat %>% na.omit() %>% group_by(date) %>% summarise(total = sum(steps))

impplot2 <- ggplot(totstepsd_na, aes(x = total)) + geom_histogram(binwidth = 600, aes(fill = ..count..)) + scale_x_continuous(breaks = seq(0,25000, by = 2500)) + ylim(0,15) + ggtitle("Total number of steps per day without data imputation")

impplot1 <- ggplot(totstepsd_imp, aes(x = total)) + geom_histogram(binwidth = 600, aes(fill = ..count..)) + scale_x_continuous(breaks = seq(0,25000, by = 2500)) + ylim(0,15) + ggtitle("Total number of steps per day with data imputation")
#load library Rmisc for multiplot capability


#save the 2 histograms to the "hist2.png"
png(filename = "hist2.png")
multiplot(impplot1,impplot2)
dev.off()

#Calculate and report the mean and median total number of steps taken per day

meansteps_day_imp <- mean(totstepsd_imp$total)
medsteps_day_imp <- median(totstepsd_imp$total)

#Mean steps were 10766.19 and median steps were 10765

#NEW QUESTION:  Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

actdat1$weekday <- weekdays(as.Date(actdat$date))

#load library "timeDate"
library(timeDate)

#determine if a date is weekday or weekend and save it to a wkday column

actdat1$wkday <- isWeekday(actdat1$date)

#rename True to "weekday" and False to "weekend"

actdat1 <- actdat1 %>% mutate(wkdayend = replace(wkday, wkday == FALSE, "weekend"))
actdat1 <- actdat1 %>% mutate(wkdayend = replace(wkdayend, wkdayend == TRUE, "weekday"))

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
#despite using dplyr for this entire exercise, it would not group this data.  After 45 minutes, switching to aggregate

wkdayavg_steps <- avg5intwkday %>% ungroup() %>% na.omit() %>% filter(wkdayend == "weekday") %>% select(c(steps, interval, wkdayend)) %>% group_by(interval) %>% summarize(avg = mean(steps))
> View(wkdayavg_steps)

wkendavg_steps <- avg5intwkday %>% ungroup() %>% na.omit() %>% filter(wkdayend == "weekend") %>% select(c(steps, interval, wkdayend)) %>% group_by(interval) %>% summarize(avg = mean(steps))

#Plot the average number of steps across the 5 minute intervals for weekend and weekday

weekdayplot <- ggplot(wkdayavg_steps,aes(interval, avg)) + geom_line() + scale_x_continuous(breaks = seq(0, 2400, by = 100)) + xlab("5 minute intervals") + ylab("Average steps per day") + ggtitle("Average Steps Per Day Per 5 Minute Interval (Weekday)")

weekendplot <- ggplot(wkendavg_steps,aes(interval, avg)) + geom_line() + scale_x_continuous(breaks = seq(0, 2400, by = 100)) + xlab("5 minute intervals") + ylab("Average steps per day") + ggtitle("Average Steps Per Day Per 5 Minute Interval (Weekend)")

#print it to "timeplot3.png"
png(filename = "timeplot3.png")
multiplot(weekdayplot,weekendplot)
dev.off()

