# Course      : 2022F CS 513-C
# First Name  : Abhishek 
# Last Name   : Panda
# CWID        : 10478684
# Purpose     : Assignment - HW_06_RF_C50

# 6.1 Solution: C5.0 
# Deleting all the objects from R- environment 
rm(list=ls())

# Reading data from file to R and setting missing values to 'NA' 
dataSet = read.csv('breast-cancer-wisconsin.csv',
                   header=TRUE,
                   sep=",",
                   colClasses = c('Sample'='numeric',
                                  'F1'='factor',
                                  'F2'='factor',
                                  'F3'='factor',
                                  'F4'='factor',
                                  'F5'='factor',
                                  'F6'='factor',
                                  'F7'='factor',
                                  'F8'='factor',
                                  'F9'='factor',
                                  'Class'='factor'))

# Representing categories as factors 
dataSet$Class <- factor(dataSet$Class, levels = c('2', '4'), labels = c("Benign", "Malignant"))

# Generating training and test set with a ratio of 70%:30% 
#Removing IDs from input variables
dataSetCopy <- dataSet[2:11]

index<-sort(sample(nrow(dataSetCopy),round(.30*nrow(dataSetCopy))))
training<-dataSetCopy[-index,]
test<-dataSetCopy[index,]

# Import required libraries 
library('C50')

# Defining accuracy function 
accuracy <- function(x) {
  sum(diag(x)/(sum(rowSums(x))))*100
}

# Implementing C5.0 Classification Tree 
model_C50 <- C5.0(Class~., data = training)
predict_C50 <- predict(model_C50, test, type = "class")
table_cart <- table(Actual = test$Class, predict_C50)
View(table_cart)
accuracy(table_cart)
plot(model_C50)
summary(model_C50)


# --------------------------------------------------------------------------#


# 6.2 Solution: Random Forest 
# Deleting all the objects from R- environment 
rm(list=ls())

# Reading data from file to R and setting missing values to 'NA' 
dataSet = read.csv('breast-cancer-wisconsin.csv',
                   header=TRUE,
                   sep=",",
                   colClasses = c('Sample'='numeric',
                                  'F1'='factor',
                                  'F2'='factor',
                                  'F3'='factor',
                                  'F4'='factor',
                                  'F5'='factor',
                                  'F6'='factor',
                                  'F7'='factor',
                                  'F8'='factor',
                                  'F9'='factor',
                                  'Class'='factor'))

# Representing categories as factors 
dataSet$Class <- factor(dataSet$Class, levels = c('2', '4'), labels = c("Benign", "Malignant"))

# Generating training and test set with a ratio of 70%:30% 
#Removing IDs from input variables
dataSetCopy <- dataSet[2:11]

index<-sort(sample(nrow(dataSetCopy),round(.30*nrow(dataSetCopy))))
training<-dataSetCopy[-index,]
test<-dataSetCopy[index,]

# Import required libraries 
library(randomForest)

# Defining accuracy function 
accuracy <- function(x) {
  sum(diag(x)/(sum(rowSums(x))))*100
}

#Implementing C5.0 Classification Tree 
model_rf <- randomForest(Class~., data = training, importance=TRUE, ntree=1000)
importance(model_rf)
varImpPlot(model_rf)
predict_rf <- predict(model_rf, test)
table_cart <- table(Actual = test$Class, predict_rf)
View(table_cart)
accuracy(table_cart)
