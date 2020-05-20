rm(list = ls())
graphics.off()
setwd("C:/Users/leech/Documents/MSA/Spring2020/MSA 8150/Final Project/SP - Gotham Cabs/Test Predictions")
Data <- read.csv("Train.csv")

####################################################################################################

#Number of Passengers
NumOfPass <- Data$NumberOfPassengers

#Seperateing the Date and Creating DayOfWeek Variable
library(lubridate)
Date <- format((strptime(Data$pickup_datetime,'%Y-%m-%d %H:%M:%S')), format = '%Y-%m-%d')
Data$Date <- Date
Year <- format((strptime(Data$Date,'%Y-%m-%d')), format = '%Y')
Data$Year <- Year
Month <- format((strptime(Data$Date,'%Y-%m-%d')), format = '%m')
Data$Month <- Month
Day <- format((strptime(Data$Date,'%Y-%m-%d')), format = '%d')
Data$Day <- Day
DayOfWeek <- wday(as.Date(Data$Date,'%Y-%m-%d'))
Data$DayOfWeek <- DayOfWeek


#Seperating the Time
Time <- format((strptime(Data$pickup_datetime,'%Y-%m-%d %H:%M:%S')), format = '%H:%M:%S')
Data$Time <- Time
Hour <- format((strptime(Data$Time,'%H:%M:%S')), format = '%H')
Data$Hour <- Hour
Minute <- format((strptime(Data$Time,'%H:%M:%S')), format = '%M')
Data$Minute <- Minute
Second <- format((strptime(Data$Time,'%H:%M:%S')), format = '%S')
Data$Second <- Second



#Selecting specific Columns #Dropping Year and Seconds after split
Data <- cbind(Data[,2:7],Data[10],Data[12],Data[,14:15])

#Creating new Variable - Distance, Assuming it's a straight path
x <- (Data$pickup_x)-(Data$dropoff_x)
y <- (Data$pickup_y)-(Data$dropoff_y)
x2 <- (x)^2
y2 <- (y)^2
Distance <- sqrt(x2+y2)
Data$Distance <- round(Distance,2)
  
Data$MinuteLevel <- NA
Data$MinuteLevel[(0 <= Data$Minute)&(Data$Minute <= 30)] <- 1
Data$MinuteLevel[(31 <= Data$Minute)&(Data$Minute <= 59)] <- 2

#Output Data with some Feature Selection
Data <- cbind(Data[0:9],Data[11:12])
write.table(Data, file = 'Train_Cleaned.csv', sep = ',', row.names = FALSE)

##############################################################################################3