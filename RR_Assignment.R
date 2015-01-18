source("./FileUpload.R") 

# call function that loads data

data <- FileUpload()

#histogram of the total number of steps taken each day

hist(data$steps)

#2.Calculate and report the mean and median total number of steps taken per day

library(dplyr)
summary1 <- summarise(group_by(data, date), mean(steps, na.rm = TRUE),median(steps, na.rm = TRUE))
print(summary1)

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the 
#average number of steps taken, averaged across all days (y-axis)

library(lattice)
summary2 <- summarise(group_by(data,interval), mean(steps, na.rm = TRUE))
names(summary2) <- c("interval", "mean")
xyplot(interval~mean, data= summary2,type="l")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxmeaninterval <- filter(summary2, mean >= 200)
print(maxmeaninterval)


# Calculate and report the total number of missing values in the dataset (i.e. 
#the total number of rows with NAs)

missing_rows <- mutate(data, is.na(steps))
names(missing_rows)<- c("steps","date","interval","step_NA")
missing_count <- summarise(missing_rows, sum(step_NA))
print(missing_count)

# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use 
# the mean/median for that day, or the mean for that 5-minute interval, etc
# In this case, the overall mean is used to populate each ealement of the vector 


missing_rows <-  mutate(missing_rows, mean(steps, na.rm = TRUE))
names(missing_rows)<- c("steps","date","interval","steps_NA","mean_all")
missing_rows$steps[missing_rows$steps_NA == TRUE ] <- missing_rows$mean_all

#histogram of the total number of steps taken each day after missing values 
#are updated 

hist(missing_rows$steps)

#Calculate and report the mean and median total number of steps taken per day
# after the missing values have been populated 
# Based on this approach there is no impact on mean or mdeian of the data

library(dplyr)
summary3 <- summarise(group_by(missing_rows, date), mean(steps, na.rm = TRUE),median(steps, na.rm = TRUE))
print(summary3)
summary4 <-summarise(missing_rows,mean(steps),median(steps))
print(summary4)


#Create a new factor variable in the dataset with two levels - "weekday" and 
#"weekend" indicating whether a given date is a weekday or weekend day.

library(dplyr)

missing_rows<- mutate(missing_rows, weekdays(as.Date(date),abbreviate=TRUE))
names(missing_rows)<-c("steps", "date", "interval","steps_NA","mean_all","weekday")

#5-minute interval (x-axis) and the average number of steps taken, averaged 
#across all weekday days or weekend days (y-axis). 

library(ggplot2)
averages <- aggregate(steps ~ interval + weekday, data=missing_rows, mean) 
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(weekday ~ .) + 
xlab("5-minute interval") + ylab("Number of steps") 

