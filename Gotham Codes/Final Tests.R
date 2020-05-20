rm(list = ls())
graphics.off()
setwd("C:/Users/leech/Documents/MSA/Spring2020/MSA 8150/Final Project/SP - Gotham Cabs")

###############################################################################################################
Data1 <- read.csv("Train_Cleaned_Original_Select.csv")

#Model Selection (Dropped Year and Seconds - Irrelevant, explained in report)
Data1$NumberOfPassengers <- as.factor(Data1$NumberOfPassengers)
Data1$Month <- as.factor(Data1$Month)
Data1$DayOfWeek <- as.factor(Data1$DayOfWeek)
Data1$Hour <- as.factor(Data1$Hour)

#Remove rows with NA
nData1 <- na.omit(Data1)

#Split Training and Testing
n1 <- floor(0.80*nrow(nData1))
train_ind1 <- sample(seq_len(nrow(nData1)), size = n1)
train1 <- nData1[train_ind1,]
test1 <- nData1[-train_ind1,]

#Linear Regression
fit1 <- lm(duration ~ ., data = train1)
pred.fit1 <- predict(fit1, test1)
lr1 <- mean((pred.fit1 - test1$duration)^2)  #229752.8

#Decision Tree
library(rpart)
tree.fit.1 <- rpart(duration ~ ., data = train1, method = 'anova')
tree.fit.prune.1 <- prune(tree.fit.1, cp = 0.01)
prune.pred.1 <- predict(tree.fit.prune.1, test1)
dt1 <- mean((prune.pred.1 - test1$duration)^2)  #180053.4

#Boosting
library(gbm)
shrink.boost.fit.1 <- gbm(duration ~ ., data = train1, distribution = 'gaussian', n.trees = 2000, interaction.depth = 11, shrinkage = 0.01, verbose = FALSE)
shrink.boost.pred.1 <- predict(shrink.boost.fit.1, test1, n.trees = 2000)
b1 <- mean((shrink.boost.pred.1-test1$duration)^2) #88953.97


###############################################################################################################

Data2 <- read.csv("Train_Cleaned_Distance.csv")

Data2$NumberOfPassengers <- as.factor(Data2$NumberOfPassengers)
Data2$Month <- as.factor(Data2$Month)
Data2$DayOfWeek <- as.factor(Data2$DayOfWeek)
Data2$Hour <- as.factor(Data2$Hour)
Data2$MinuteLevel <- as.factor(Data2$MinuteLevel)

nData2 <- na.omit(Data2)

n2 <- floor(0.80*nrow(nData2))
train_ind2 <- sample(seq_len(nrow(nData2)), size = n2)
train2 <- nData2[train_ind2,]
test2 <- nData2[-train_ind2,]

#Linear Regression
fit2 <- lm(duration ~ ., data = train2)
pred.fit2 <- predict(fit2, test2)
lr2 <- mean((pred.fit2 - test2$duration)^2)  #109518.20

#Decision Tree
library(rpart)
tree.fit.2 <- rpart(duration ~ ., data = train2, method = 'anova')
tree.fit.prune.2 <- prune(tree.fit.2, cp = 0.01)
prune.pred.2 <- predict(tree.fit.prune.2, test2)
dt2 <- mean((prune.pred.2 - test2$duration)^2) #114665.11

#Boosting
library(gbm)
shrink.boost.fit.2.1 <- gbm(duration ~ ., data = train2, distribution = 'gaussian', n.trees = 1000, interaction.depth = 11, shrinkage = 0.01, verbose = FALSE)
shrink.boost.pred.2.1 <- predict(shrink.boost.fit.2.1, test2, n.trees = 1000)
b2.1 <- mean((shrink.boost.pred.2.1 - test2$duration)^2) #70281.29

shrink.boost.fit.2 <- gbm(duration ~ ., data = train2, distribution = 'gaussian', n.trees = 2000, interaction.depth = 11, shrinkage = 0.01, verbose = FALSE)
shrink.boost.pred.2 <- predict(shrink.boost.fit.2, test2, n.trees = 2000)
b2 <- mean((shrink.boost.pred.2 - test2$duration)^2) #66250.34

shrink.boost.fit.2.2 <- gbm(duration ~ ., data = train2, distribution = 'gaussian', n.trees = 2000, interaction.depth = 11, shrinkage = 0.001, verbose = FALSE)
shrink.boost.pred.2.2 <- predict(shrink.boost.fit.2.2, test2, n.trees = 2000)
b2.2 <- mean((shrink.boost.pred.2.2 - test2$duration)^2) #91566.31

shrink.boost.fit.2.3 <- gbm(duration ~ ., data = train2, distribution = 'gaussian', n.trees = 2000, interaction.depth = 11, shrinkage = 0.02, verbose = FALSE)
shrink.boost.pred.2.3 <- predict(shrink.boost.fit.2.3, test2, n.trees = 2000)
b2.3 <- mean((shrink.boost.pred.2.3 - test2$duration)^2) #62582.73

#Normalize the Distance
train2$Distance <- (train2$Distance - mean(train2$Distance)) / (sd(train2$Distance))
test2$Distance <- (test2$Distance - mean(test2$Distance)) / (sd(test2$Distance))

#Linear Regression
fit3 <- lm(duration ~ ., data = train2)
pred.fit3 <- predict(fit3, test2)
lr3 <- mean((pred.fit3 - test2$duration)^2) #109516.39

#Decision Tree
library(rpart)
tree.fit.3 <- rpart(duration ~ ., data = train2, method = 'anova')
tree.fit.prune.3 <- prune(tree.fit.3, cp = 0.01)
prune.pred.3 <- predict(tree.fit.prune.3, test2)
dt3 <- mean((prune.pred.3 - test2$duration)^2) #114696.09

#Boosting
library(gbm)
shrink.boost.fit.3 <- gbm(duration ~ ., data = train2, distribution = 'gaussian', n.trees = 2000, interaction.depth = 11, shrinkage = 0.01, verbose = FALSE)
shrink.boost.pred.3 <- predict(shrink.boost.fit.3, test2, n.trees = 2000)
b3 <- mean((shrink.boost.pred.3 - test2$duration)^2)


###############################################################################################################


Data3 <- read.csv("Train_Cleaned_CDistance.csv")

Data3$NumberOfPassengers <- as.factor(Data3$NumberOfPassengers)
Data3$Month <- as.factor(Data3$Month)
Data3$DayOfWeek <- as.factor(Data3$DayOfWeek)
Data3$Hour <- as.factor(Data3$Hour)
Data3$MinuteLevel <- as.factor(Data3$MinuteLevel)

nData3 <- na.omit(Data3)

n3 <- floor(0.80*nrow(nData3))
train_ind3 <- sample(seq_len(nrow(nData3)), size = n3)
train3 <- nData3[train_ind3,]
test3 <- nData3[-train_ind3,]

#Linear Regression
fit4 <- lm(duration ~ ., data = train3)
pred.fit4 <- predict(fit4, test3)
lr4 <- mean((pred.fit4 - test3$duration)^2) #115048.97

#Decision Tree
library(rpart)
tree.fit.4 <- rpart(duration ~ ., data = train3, method = 'anova')
tree.fit.prune.4 <- prune(tree.fit.4, cp = 0.01)
prune.pred.4 <- predict(tree.fit.prune.4, test3)
dt4 <- mean((prune.pred.4 - test3$duration)^2) #122433.05

#Boosting
library(gbm)
shrink.boost.fit.4.1 <- gbm(duration ~ ., data = train3, distribution = 'gaussian', n.trees = 1000, interaction.depth = 11, shrinkage = 0.01, verbose = FALSE)
shrink.boost.pred.4.1 <- predict(shrink.boost.fit.4.1, test3, n.trees = 1000)
b4.1 <- mean((shrink.boost.pred.4.1 - test3$duration)^2) #69840.49

shrink.boost.fit.4 <- gbm(duration ~ ., data = train3, distribution = 'gaussian', n.trees = 2000, interaction.depth = 11, shrinkage = 0.01, verbose = FALSE)
shrink.boost.pred.4 <- predict(shrink.boost.fit.4, test3, n.trees = 2000)
b4 <- mean((shrink.boost.pred.4 - test3$duration)^2) #65520.93

shrink.boost.fit.4.2 <- gbm(duration ~ ., data = train3, distribution = 'gaussian', n.trees = 2000, interaction.depth = 11, shrinkage = 0.001, verbose = FALSE)
shrink.boost.pred.4.2 <- predict(shrink.boost.fit.4.2, test3, n.trees = 2000)
b4.2 <- mean((shrink.boost.pred.4.2 - test3$duration)^2) #95087.83

shrink.boost.fit.4.3 <- gbm(duration ~ ., data = train3, distribution = 'gaussian', n.trees = 2000, interaction.depth = 11, shrinkage = 0.02, verbose = FALSE)
shrink.boost.pred.4.3 <- predict(shrink.boost.fit.4.3, test3, n.trees = 2000)
b4.3 <- mean((shrink.boost.pred.4.3 - test3$duration)^2) #62476.18


#Normalize the CDistance
train3$CDistance <- (train3$CDistance - mean(train3$CDistance)) / (sd(train3$CDistance))
test3$CDistance <- (test3$CDistance - mean(test3$CDistance)) / (sd(test3$CDistance))

#Linear Regression
fit5 <- lm(duration ~ ., data = train3)
pred.fit5 <- predict(fit5, test3)
lr5 <- mean((pred.fit5 - test3$duration)^2) #115052.96

#Decision Tree
library(rpart)
tree.fit.5 <- rpart(duration ~ ., data = train3, method = 'anova')
tree.fit.prune.5 <- prune(tree.fit.5, cp = 0.01)
prune.pred.5 <- predict(tree.fit.prune.5, test3)
dt5 <- mean((prune.pred.5 - test3$duration)^2) #122455.46

#Boosting
library(gbm)
shrink.boost.fit.5 <- gbm(duration ~ ., data = train3, distribution = 'gaussian', n.trees = 2000, interaction.depth = 11, shrinkage = 0.01, verbose = FALSE)
shrink.boost.pred.5 <- predict(shrink.boost.fit.5, test3, n.trees = 2000)
b5 <- mean((shrink.boost.pred.5 - test3$duration)^2)




###############################################################################################################
#H2O

rm(list = ls())
graphics.off()
setwd("C:/Users/leech/Documents/MSA/Spring2020/MSA 8150/Final Project/SP - Gotham Cabs")

###############################################################################################################
Data1 <- read.csv("Train_Cleaned_Original_Select.csv")

#Model Selection (Dropped Year and Seconds - Irrelevant, explained in report)
Data1$NumberOfPassengers <- as.factor(Data1$NumberOfPassengers)
Data1$Month <- as.factor(Data1$Month)
Data1$DayOfWeek <- as.factor(Data1$DayOfWeek)
Data1$Hour <- as.factor(Data1$Hour)

#Remove rows with NA# Check for NA
nData1 <- na.omit(Data1)

#Split Training and Testing
n1 <- floor(0.80*nrow(nData1))
train_ind1 <- sample(seq_len(nrow(nData1)), size = n1)
train1 <- nData1[train_ind1,]
test1 <- nData1[-train_ind1,]

#Boosting
library(gbm)
shrink.boost.fit.1 <- gbm(duration ~ ., data = train, distribution = 'gaussian', n.trees = 1000, interaction.depth = 11, shrinkage = 0.01, verbose = FALSE)
shrink.boost.pred.1 <- predict(shrink.boost.fit.1, test, n.trees = 1000)
b1 <- mean((shrink.boost.pred.1-test$duration)^2)


###############################################################################################################

Data2 <- read.csv("Train_Cleaned_Distance.csv")

Data2$NumberOfPassengers <- as.factor(Data2$NumberOfPassengers)
Data2$Month <- as.factor(Data2$Month)
Data2$DayOfWeek <- as.factor(Data2$DayOfWeek)
Data2$Hour <- as.factor(Data2$Hour)
Data2$MinuteLevel <- as.factor(Data2$MinuteLevel)

nData2 <- na.omit(Data2)

n2 <- floor(0.80*nrow(nData2))
train_ind2 <- sample(seq_len(nrow(nData2)), size = n2)
train2 <- nData2[train_ind2,]
test2 <- nData2[-train_ind2,]

#Boosting
library(gbm)
shrink.boost.fit.2 <- gbm(duration ~ ., data = train, distribution = 'gaussian', n.trees = 1000, interaction.depth = 11, shrinkage = 0.01, verbose = FALSE)
shrink.boost.pred.2 <- predict(shrink.boost.fit.2, test, n.trees = 1000)
b2 <- mean((shrink.boost.pred.2 - test$duration)^2)


#Normalize the Distance
train$Distance <- (train$Distance - mean(train$Distance)) / (sd(train$Distance))
test$Distance <- (test$Distance - mean(test$Distance)) / (sd(test$Distance))


#Boosting
library(gbm)
shrink.boost.fit.3 <- gbm(duration ~ ., data = train, distribution = 'gaussian', n.trees = 1000, interaction.depth = 11, shrinkage = 0.01, verbose = FALSE)
shrink.boost.pred.3 <- predict(shrink.boost.fit.3, test, n.trees = 1000)
b3 <- mean((shrink.boost.pred.3 - test$duration)^2)


###############################################################################################################


Data3 <- read.csv("Train_Cleaned_CDistance.csv")

Data3$NumberOfPassengers <- as.factor(Data3$NumberOfPassengers)
Data3$Month <- as.factor(Data3$Month)
Data3$DayOfWeek <- as.factor(Data3$DayOfWeek)
Data3$Hour <- as.factor(Data3$Hour)
Data3$MinuteLevel <- as.factor(Data3$MinuteLevel)

nData3 <- na.omit(Data3)

n3 <- floor(0.80*nrow(nData3))
train_ind3 <- sample(seq_len(nrow(nData3)), size = n3)
train3 <- nData3[train_ind3,]
test3 <- nData3[-train_ind3,]

#Boosting
library(gbm)
shrink.boost.fit.4 <- gbm(duration ~ ., data = train, distribution = 'gaussian', n.trees = 1000, interaction.depth = 11, shrinkage = 0.01, verbose = FALSE)
shrink.boost.pred.4 <- predict(shrink.boost.fit.4, test, n.trees = 1000)
b4 <- mean((shrink.boost.pred.4 - test$duration)^2)


#Normalize the CDistance
train$CDistance <- (train$CDistance - mean(train$CDistance)) / (sd(train$CDistance))
test$CDistance <- (test$CDistance - mean(test$CDistance)) / (sd(test$CDistance))


#Boosting
library(gbm)
shrink.boost.fit.5 <- gbm(duration ~ ., data = train, distribution = 'gaussian', n.trees = 1000, interaction.depth = 11, shrinkage = 0.01, verbose = FALSE)
shrink.boost.pred.5 <- predict(shrink.boost.fit.5, test, n.trees = 1000)
b5 <- mean((shrink.boost.pred.5 - test$duration)^2)




###############################################################################################################
#"Train_Cleaned_Original_Select.csv"
library(tidyverse)

x_train <- train1 %>% select(-duration)
y_train <- train1 %>% select(duration)
x_test <- test1 %>% select(-duration)
y_test <- test1 %>% select(duration)

write_rds(x_train, "x_train.rds") #Simplify memory usage
write_rds(x_test, "x_test.rds")
write_rds(y_train, "y_train.rds")
write_rds(y_test, "y_test.rds")

x_train_rds <- read_rds("x_train.rds")
x_test_rds <- read_rds("x_test.rds")
y_train_rds <- read_rds("y_train.rds")
y_test_rds <- read_rds("y_test.rds")

#Clean memory space
rm(x_train)
rm(x_test)
rm(y_train)
rm(y_test)

#Install h2o
#if(!"h2o" %in% rownames(installed.packages()) | packageVersion("h2o") != "3.28.0.2") {  
#  install.packages("remotes")  
#  remotes::install_version("h2o", "3.28.0.2", upgrade=F)
#}
library(h2o)

#Start H20
h2o.init()

#Import file to H20
#training
data_h2o <- as.h2o(  
  bind_cols(y_train_rds, x_train_rds),  #duration first, then features [lavel, feature]
  destination_frame= "train.hex"
)

#testing
new_data_h2o <- as.h2o(  
  bind_cols(y_test_rds, x_test_rds),  
  destination_frame= "test.hex"
)

#Check if the files has imported
h2o.ls()

#parrtition the data into training, validation and test sets
splits <- h2o.splitFrame(data = data_h2o,                          
                         ratios = c(0.7, 0.15), # 70/15/15 split                         
                         seed = 1234
)
train_h2o <- splits[[1]] # from training data
valid_h2o <- splits[[2]] # from training data
test_h2o  <- splits[[3]] # from training data

#GBM
gbm1 <- h2o.gbm(
  training_frame = train_h2o,        ## the H2O frame for training
  validation_frame = valid_h2o,      ## the H2O frame for validation (not required)
  x=2:10,                            ## the predictor columns, by column index
  y=1,                               ## the target index (what we are predicting)
  model_id = "gbm_covType1",         ## name the model in H2O
  seed = 1234)                       ## Set the random seed for reproducability

summary(gbm1)                        ## View information about the model.

#Tuning
gbm2 <- h2o.gbm(
  training_frame = train_h2o,     ##
  validation_frame = valid_h2o,   ##
  x=2:10,                         ##
  y=1,                            ## 
  ntrees = 100,                   ## increase the trees (default is 50)
  learn_rate = 0.2,               ## increase the learning rate (from 0.1)
  max_depth = 10,                 ## increase the depth (from default 5)
  stopping_rounds = 2,            ## 
  stopping_tolerance = 0.01,      ##
  score_each_iteration = T,       ##
  model_id = "gbm_covType2",      ##
  seed = 2000000)                 ##

summary(gbm2)   


###############################################################################################################
#"Train_Cleaned_Distance.csv"
library(tidyverse)


x_train <- train2 %>% select(-duration)
y_train <- train2 %>% select(duration)
x_test <- test2 %>% select(-duration)
y_test <- test2 %>% select(duration)

write_rds(x_train, "x_train.rds") #Simplify memory usage
write_rds(x_test, "x_test.rds")
write_rds(y_train, "y_train.rds")
write_rds(y_test, "y_test.rds")

x_train_rds <- read_rds("x_train.rds")
x_test_rds <- read_rds("x_test.rds")
y_train_rds <- read_rds("y_train.rds")
y_test_rds <- read_rds("y_test.rds")

#Clean memory space
rm(x_train)
rm(x_test)
rm(y_train)
rm(y_test)

#Import file to H20
#training
data_h2o <- as.h2o(  
  bind_cols(y_train_rds, x_train_rds),  #duration first, then features [label, feature]
  destination_frame= "train.hex"
)

#testing
new_data_h2o <- as.h2o(  
  bind_cols(y_test_rds, x_test_rds),  
  destination_frame= "test.hex"
)

#Check if the files has imported
h2o.ls()

#parrtition the data into training, validation and test sets
splits <- h2o.splitFrame(data = data_h2o,                          
                         ratios = c(0.7, 0.15), # 70/15/15 split                         
                         seed = 1234
)
train_h2o <- splits[[1]] # from training data
valid_h2o <- splits[[2]] # from training data
test_h2o  <- splits[[3]] # from training data

#GBM
gbm3 <- h2o.gbm(
  training_frame = train_h2o,        ## the H2O frame for training
  validation_frame = valid_h2o,      ## the H2O frame for validation (not required)
  x=2:11,                            ## the predictor columns, by column index
  y=1,                               ## the target index (what we are predicting)
  model_id = "gbm_covType3",         ## name the model in H2O
  seed = 1234)                       ## Set the random seed for reproducability

summary(gbm3)                        ## View information about the model.

#Tuning
gbm4 <- h2o.gbm(
  training_frame = train_h2o,     ##
  validation_frame = valid_h2o,   ##
  x=2:11,                         ##
  y=1,                            ## 
  ntrees = 1000,                  ## increase the trees (default is 50)
  learn_rate = 0.5,               ## increase the learning rate (from 0.1)
  max_depth = 10,                 ## increase the depth (from default 5)
  stopping_rounds = 2,            ## 
  stopping_tolerance = 0.01,      ##
  score_each_iteration = T,       ##
  model_id = "gbm_covType4",      ##
  seed = 2000000)                 ##

summary(gbm4)   




###############################################################################################################
#"Train_Cleaned_CDistance.csv"
library(tidyverse)


x_train <- train3 %>% select(-duration)
y_train <- train3 %>% select(duration)
x_test <- test3 %>% select(-duration)
y_test <- test3 %>% select(duration)

write_rds(x_train, "x_train.rds") #Simplify memory usage
write_rds(x_test, "x_test.rds")
write_rds(y_train, "y_train.rds")
write_rds(y_test, "y_test.rds")

x_train_rds <- read_rds("x_train.rds")
x_test_rds <- read_rds("x_test.rds")
y_train_rds <- read_rds("y_train.rds")
y_test_rds <- read_rds("y_test.rds")

#Clean memory space
rm(x_train)
rm(x_test)
rm(y_train)
rm(y_test)

#Import file to H20
#training
data_h2o <- as.h2o(  
  bind_cols(y_train_rds, x_train_rds),  #duration first, then features [label, feature]
  destination_frame= "train.hex"
)

#testing
new_data_h2o <- as.h2o(  
  bind_cols(y_test_rds, x_test_rds),  
  destination_frame= "test.hex"
)

#Check if the files has imported
h2o.ls()

#parrtition the data into training, validation and test sets
splits <- h2o.splitFrame(data = data_h2o,                          
                         ratios = c(0.7, 0.15), # 70/15/15 split                         
                         seed = 1234
)
train_h2o <- splits[[1]] # from training data
valid_h2o <- splits[[2]] # from training data
test_h2o  <- splits[[3]] # from training data

#GBM
gbm5 <- h2o.gbm(
  training_frame = train_h2o,        ## the H2O frame for training
  validation_frame = valid_h2o,      ## the H2O frame for validation (not required)
  x=2:11,                            ## the predictor columns, by column index
  y=1,                               ## the target index (what we are predicting)
  model_id = "gbm_covType3",         ## name the model in H2O
  seed = 1234)                       ## Set the random seed for reproducability

summary(gbm5)                        ## View information about the model.

#Tuning
gbm6 <- h2o.gbm(
  training_frame = train_h2o,     ##
  validation_frame = valid_h2o,   ##
  x=2:11,                         ##
  y=1,                            ## 
  ntrees = 100,                   ## increase the trees (default is 50)
  learn_rate = 0.2,               ## increase the learning rate (from 0.1)
  max_depth = 10,                 ## increase the depth (from default 5)
  stopping_rounds = 2,            ## 
  stopping_tolerance = 0.01,      ##
  score_each_iteration = T,       ##
  model_id = "gbm_covType4",      ##
  seed = 2000000)                 ##

summary(gbm6)   


###########################################################################################
#Best Model Trained with no splitting
rm(list = ls())
graphics.off()
setwd("C:/Users/leech/Documents/MSA/Spring2020/MSA 8150/Final Project/SP - Gotham Cabs")

Data <- read.csv("Train_Cleaned_Distance.csv")

Data$NumberOfPassengers <- as.factor(Data$NumberOfPassengers)
Data$Month <- as.factor(Data$Month)
Data$DayOfWeek <- as.factor(Data$DayOfWeek)
Data$Hour <- as.factor(Data$Hour)
Data$MinuteLevel <- as.factor(Data$MinuteLevel)

nData <- na.omit(Data)


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
DataT <- read.csv("Test_Cleaned_Distance.csv")
DataT$NumberOfPassengers <- as.factor(DataT$NumberOfPassengers)
DataT$Month <- as.factor(DataT$Month)
DataT$DayOfWeek <- as.factor(DataT$DayOfWeek)
DataT$Hour <- as.factor(DataT$Hour)
DataT$MinuteLevel <- as.factor(DataT$MinuteLevel)

nDataT <- na.omit(DataT)

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
  path = "C:/Users/leech/Documents/MSA/Spring2020/MSA 8150/Final Project/SP - Gotham Cabs/Predicted.csv"
)
