# Course      : 2021F CS 513-C
# First Name  : Abhishek 
# Last Name   : Panda
# CWID        : 10478684
# Purpose     : Assignment - HW_07_ANN

#Deleting all the objects from R- environment
rm(list=ls())

#Reading data from file to R and setting missing values to 'NA'
dataSet = read.csv('wisc_bc_ContinuousVar.csv',
                   header=TRUE,
                   sep=",",
                   na.strings = "?",
                   )

#Deleting the 1st row 
dataSet = subset(dataSet, select = -c(id) )

#Deleting Rows with missing values
dataSet <- na.omit(dataSet)

#Converting diagnosis variable to numeric levels to make it easier to compare
dataSet$diagnosis <- factor(dataSet$diagnosis, levels = c('M','B'),labels = c(1,2))


#Generating training and test set with a ratio of 70%:30%
index<-sort(sample(nrow(dataSet),round(.70*nrow(dataSet))))
train<-dataSet[index,]
test<-dataSet[-index,]

#Import required libraries
library("neuralnet")


#Implementing ANN
model_nn <- neuralnet(diagnosis~., train, hidden=5, threshold=0.1)
plot(model_nn)
ann <- compute(model_nn, test)  
ann$net.result

ann_cat<-ifelse(ann$net.result <1.5,1,2)
length(ann_cat)


wrong <- (test$diagnosis!=ann_cat)
error_rate<-sum(wrong)/length(wrong)
error_rate

