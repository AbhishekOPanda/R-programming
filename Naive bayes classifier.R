# Course      : 2022F CS 513-C
# First Name  : Abhishek 
# Last Name   : Panda
# CWID        : 10478684
# Purpose     : Assignment - HW_04_NB

#Deleting all the objects from R- environment
rm(list=ls())

#Reading data from file to R and setting missing values to 'NA'
dataSet = read.csv('breast-cancer-wisconsin.csv', header=TRUE, sep=",", na.strings = "?")

#Deleting Rows with missing values
dataSet <- na.omit(dataSet)

#Import package 'e1071' for Naive Bayes Classifier and class package
library(e1071)
library(class)

#Representing categories as factors 
dataSet$Class <- factor(dataSet$Class, levels = c('2', '4'), labels = c("Benign", "Malignant"))

#Generating training and test set with a ratio of 70%:30% 
dataSetCopy <- dataSet[2:11]

index<-sort(sample(nrow(dataSetCopy),round(.30*nrow(dataSetCopy))))
training<-dataSetCopy[-index,]
test<-dataSetCopy[index,]

#Defining accuracy function 
accuracy <- function(x) {
  sum(diag(x)/(sum(rowSums(x))))*100
}

#Implementing Naive Bayes 
model_nb <- naiveBayes(Class~., data = training)
predict_nb <- predict(model_nb, test)
table_nb <- table(Actual = test$Class, Predicted = predict_nb)
accuracy(table_nb)
