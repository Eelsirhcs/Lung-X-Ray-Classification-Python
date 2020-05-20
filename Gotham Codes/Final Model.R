rm(list = ls())
graphics.off()
setwd("C:/Users/leech/Documents/MSA/Spring2020/MSA 8150/Final Project/SP - Gotham Cabs/Test Predictions")

###############################################################################################################
#Best Model Trained with no splitting

Data <- read.csv("Train_Cleaned.csv")

Data$NumberOfPassengers <- as.factor(Data$NumberOfPassengers)
Data$Month <- as.factor(Data$Month)
Data$DayOfWeek <- as.factor(Data$DayOfWeek)
Data$Hour <- as.factor(Data$Hour)
Data$MinuteLevel <- as.factor(Data$MinuteLevel)

nData <- na.omit(Data) #Check for NA fro cleaning


library(h2o)

#Start H20
h2o.init()

#"Train_Cleaned_Distance.csv"
library(tidyverse)


#Import file to H20
#training
data_h2o <- as.h2o(nData,
  destination_frame = "train.hex"
)

#Check if the files has imported
h2o.ls()



#Tuning
gbm <- h2o.gbm(
  training_frame = data_h2o,      ##
  x=colnames(data_h2o)[c(1,3:11)],##
  y=2,                            ## 
  ntrees = 1000,                  ## increase the trees (default is 50)
  learn_rate = 0.5,               ## increase the learning rate (from 0.1)
  max_depth = 10,                 ## increase the depth (from default 5)
  stopping_rounds = 2,            ## 
  stopping_tolerance = 0.01,      ##
  score_each_iteration = T,       ##
  model_id = "gbm_covType",       ##
  seed = 2000000)                 ##

summary(gbm)   

##import test data file
DataT <- read.csv("Test_Cleaned.csv")
DataT$NumberOfPassengers <- as.factor(DataT$NumberOfPassengers)
DataT$Month <- as.factor(DataT$Month)
DataT$DayOfWeek <- as.factor(DataT$DayOfWeek)
DataT$Hour <- as.factor(DataT$Hour)
DataT$MinuteLevel <- as.factor(DataT$MinuteLevel)

nDataT <- na.omit(DataT) #check for NA from cleaning

#Import file to H20
#training
dataT_h2o <- as.h2o(nDataT,
                   destination_frame = "test.hex"
)

#Check if the files has imported
h2o.ls()


#Test GBM model
predictions <- h2o.predict(
  object = gbm
 ,newdata = dataT_h2o)

predictions


h2o.exportFile(
  predictions,
  path = "C:/Users/leech/Documents/MSA/Spring2020/MSA 8150/Final Project/SP - Gotham Cabs/Test Predictions/G.csv"
)





