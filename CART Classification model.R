# Course      : 2022F CS 513-C
# First Name  : Abhishek 
# Last Name   : Panda
# CWID        : 10478684
# Purpose     : Assignment - HW_05_Dtree

# 5.2 Solution 
# Deleting all the objects from R- environment 
rm(list=ls())

# Reading data from file to R and setting missing values to 'NA' 
dataSet = read.csv('breast-cancer-wisconsin.csv', header=TRUE, sep=",", na.strings = "?")

# Deleting Rows with missing values 
dataSet <- na.omit(dataSet)

# Representing categories as factors 
dataSet$Class <- factor(dataSet$Class, levels = c('2', '4'), labels = c("Benign", "Malignant"))

# Generating training and test set with a ratio of 70%:30% 
dataSetCopy <- dataSet[2:11]

index<-sort(sample(nrow(dataSetCopy),round(.30*nrow(dataSetCopy))))
training<-dataSetCopy[-index,]
test<-dataSetCopy[index,]

# Import standard CART package and class package 
library(rpart)
library(class)

# Defining accuracy function 
accuracy <- function(x) {
  sum(diag(x)/(sum(rowSums(x))))*100
}

# Implementing CART 
model_cart <- rpart(Class~., data = training, method = "class")
predict_cart <- predict(model_cart, test, type = "class")
table_cart <- table(Actual = test$Class, predict_cart)
accuracy(table_cart)
