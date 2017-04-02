dir <- "C:/Users/danielsw/OneDrive/Data Science Specialization/ReproducibleResearchWeek2/RepData_PeerAssessment1"
setwd(dir)
source("ReadData.R")
Data <- ReadData(dir)
library(lattice)
StepsPerDay <- aggregate(steps ~ date,Data, sum, na.rm=TRUE)
StepsMean <- mean(StepsPerDay$steps)
StepsMedian <- median(StepsPerDay$steps)
histogram(StepsPerDay$steps)

#Average daily activity
StepsByIntervalMean <- aggregate(steps ~ interval, Data, mean)
xyplot(steps ~ interval, StepsByIntervalMean, type="l")

#Missing values
#Calculate total missing values
TotalMissing <- sum(is.na(Data))
#Create new dataset
CleanData <- Data
MaxRows <- nrow(Data)
index <- 1
While(index <= MaxRows){
  if(is.na(Data$steps[index])){
    CleanData$steps[iRow] <- StepsByIntervalMean$steps[which(StepsByIntervalMean$interval==Data$interval[index])]
  }
  index <- index + 1
}


StepsPerDay <- aggregate(steps ~ date,CleanData, sum, na.rm=TRUE)
histogram(StepsPerDay$steps)
print(mean(StepsPerDay$steps))
print(median(StepsPerDay$steps))

#Differences in activity by weekdays and weekends
CleanData$date <- as.Date.POSIXct(NonNAData$date)
DayType <- function(v_date){
  d <- as.POSIXlt(as.Date(v_date))$wday
  return( ifelse(d==0||d==6,"Weekend","Weekday"))
}
CleanData$WeekTypeDay <- as.factor(sapply(Data$date, DayType))
StepsByIntervalWeekType <- aggregate(steps ~ interval + WeekTypeDay, CleanData, mean, na.rm=TRUE)
xyplot(steps ~ interval | tpDay, data=StepsByIntervalWeekType, type="l")


