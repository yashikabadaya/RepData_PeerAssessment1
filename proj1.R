library(ggplot2)
library(lattice)
library(plyr)
library(reshape2)
data<- read.csv("activity.csv")
data$date<- as.POSIXct(data$date, format="%Y-%m-%d")
na_removed_data <- data[!is.na(data$steps),]
total_steps_in_a_day <- aggregate(na_removed_data$steps, by = list(na_removed_data$date), FUN = sum)
hist(total_steps_in_a_day$x, col= "red", xlab = "Total no of steps in a day", ylab = "Frequency")
mean(total_steps_in_a_day$x)
median(total_steps_in_a_day$x)

avg_no_steps_by_interal <- aggregate(na_removed_data$steps , by = list(na_removed_data$interval), FUN = mean)
ggplot(avg_no_steps_by_interal, aes(avg_no_steps_by_interal$Group.1, avg_no_steps_by_interal$x)) + geom_line()


colnames(mean_table) <- c("Date", "avg_no_of_steps_in_a_day")


StepsPerInterval <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
# split activity data by interval
data_split <- split(data, data$interval)
# fill in missing data for each interval
for(i in 1:length(data_split)){
  data_split[[i]]$steps[is.na(data_split[[i]]$steps)] <- StepsPerInterval[i]
}
changed_data <- do.call("rbind", data_split)
changed_data <- changed_data[order(changed_data$date) ,]

changed_total_steps_in_a_day <- tapply(changed_data$steps, changed_data$date, sum)
hist(changed_total_steps_in_a_day, xlab = "Number of Steps", main = "Histogram: Steps per Day (After replacing NAs)")

changed_data$week <- ifelse(weekdays(changed_data$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

intervalTable2 <- ddply(changed_data, .(interval, week), summarize, Avg = mean(steps))

##Plot data in a panel plot
xyplot(Avg~interval|week, data=intervalTable2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")

