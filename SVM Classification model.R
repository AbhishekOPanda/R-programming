# Course      : 2021F CS 513-C
# First Name  : Abhishek 
# Last Name   : Panda
# CWID        : 10478684
# Purpose     : Assignment - HW_09_SVM

#Deleting all the objects from R- environment
rm(list=ls())

#Reading data from file to R and setting missing values to 'NA'
dataSet = read.csv('wisc_bc_ContinuousVar.csv',
                   header=TRUE,
                   sep=",",
                   colClasses = c("diagnosis"="factor")
)

dataSet <- dataSet[,-1]

#Factorizing the diagnosis column 
dataSet$diagnosis <- factor(dataSet$diagnosis, levels = c('M','B'),labels = c(1,2))

#Generating training and test set with a ratio of 70%:30%
index<-sort(sample(nrow(dataSet),round(.70*nrow(dataSet))))
train<-dataSet[index,]
test<-dataSet[-index,]

#### Import required libraries ####
library(e1071)

#### Performing SVM ####
model_svm <- svm(diagnosis~., data= train)
svm = predict(model_svm, test)

####Accuracy function and confusion matrix
#Accuracy function
accuracy <- function(x) {
  sum(diag(x)/(sum(rowSums(x))))*100
}

#Confusion Matrix
confusion_matrix <- table(Actual = test$diagnosis, Predicted = svm)

print(confusion_matrix)
accuracy(confusion_matrix)