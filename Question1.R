##This file is used in the course projuct in week two for the reproducibile reasearch course. 
##This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals
##through out the day. The data consists of two months of data from an anonymous individual collected during the months 
##of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##The data for this assignment was downloaded from the 
##course web site: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
    
##read in data
activity<-read.csv("C:/Users/betty/OneDrive/Desktop/RepData_PeerAssessment1/activity/activity.csv", header=TRUE)

##make dplyr available
library(dplyr)

##Create daily steps using group by
dailySteps<-activity%>%group_by(date)%>%summarize(TotalSteps = sum(steps))

##create historgram for the daily steps
hist(dailySteps$TotalSteps)

##Determine mean steps per day
meanDailySteps<-mean(dailySteps$TotalSteps, na.rm=TRUE)

#Determine median steps per day
medianDailySteps<-median(dailySteps$TotalSteps, na.rm=TRUE)

#Determine steps by interval, removing NA values in calculations
dailyInterval<-activity%>%group_by(interval, na.rm=TRUE)%>%summarize(aveSteps = mean(steps, na.rm=TRUE))

#plot average steps by interval
plot(dailyInterval$interval, dailyInterval$aveSteps, type="l")

##Determine the interval with the maxSteps
maxInterval<-dailyInterval$interval[which.max(dailyInterval$aveSteps)]

##Determine how many NA's are in the activity data
noData<-sum(is.na(activity))

##load the library zoo to fill NA values
library(zoo)

##fill all NA values with the last observed value in the data set
activity1<-na.locf(activity)

#Create daily steps for imputted data
dailySteps1<-activity1%>%group_by(date)%>%summarize(TotalSteps1 = sum(steps))

#create historgram for the daily steps
hist(dailySteps1$TotalSteps1)

#Determine mean steps per day
meanDailySteps1<-mean(dailySteps1$TotalSteps1, na.rm=TRUE)

#Determine median steps per day
medianDailySteps1<-median(dailySteps1$TotalSteps1, na.rm=TRUE)

#Convert activity date column to a date data type
activity1$date<-as.Date(activity1$date)

#Change activity1 date column to show the day of the week instead of a date
activity1$date <-weekdays(activity1$date)

#create a new column to show weekend or weekday and label of values weekday to start
activity1$day<-"Weekday"

#change all weekend values to weekend
activity1$day[activity1$date==c("Saturday", "Sunday")]<-"Weekend"

#Determine the average steps per interval
averageStepInterval<-activity1%>%group_by(day,interval)%>%summarize(aveSteps = mean(steps))

#plot the average steps using the lattice xy plot
xyplot(aveSteps~interval|day, data=averageStepInterval, type="l", layout=c(1,2))
