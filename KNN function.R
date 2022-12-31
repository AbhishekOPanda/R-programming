# Course      : 2022F CS 513-C
# First Name  : Abhishek
# Last Name   : Panda
# CWID        : 10478684
# Purpose     : HW03_knn

#### Deleting all the objects from R- environment ####
rm(list=ls())

#### Reading data from a file into R and assigning 'NA' for any missing values ####
data_set = read.csv('D:/stevens/3_sem/cs513/breast-cancer-wisconsin.csv', header=TRUE, sep=",", na.strings = "?")

#### Deleting Rows with missing values ####
data_set <- na.omit(data_set)

#### Representing categories as factors ####
data_set$Class <- factor(data_set$Class, levels = c('2', '4'), labels = c("Benign", "Malignant"))

#### Performing KNN ####

#### Define max-min normalization function ####
mm_norm <- function(x) {
  z <- ((x-min(x))/(max(x)-min(x))) 
  return(z)
}

#### Normalizing F1 to F9 since they are the predicting facrtors ####
normalized_data <- as.data.frame(lapply(data_set[ , c(2,3,4,5,6,7,8,9,10)], mm_norm))

#### Generating training and test set with a ratio of 70%:30% ####
dataSet_copy <- as.data.frame(cbind(normalized_data, Class = data_set$Class))

i <-sort(sample(nrow(dataSet_copy),round(.30*nrow(dataSet_copy))))
training<-dataSet_copy[-i,]
test<-dataSet_copy[i,]

# Loading the package and knn class #
library(kknn)
library(class)

# Defining accuracy function #
acc <- function(x) {
  sum(diag(x)/(sum(rowSums(x))))*100
  }

#### Running knn function for k = 3 ####
predict_knn1 <- kknn(formula=Class~., training, test, k=3)
fit_knn1 <- fitted(predict_knn1)
table_knn1 <- table(Actual=test$Class,Fitted=fit_knn1)
acc(table_knn1)

#### Running knn function for k = 5 ####
predict_knn2 <- kknn(formula=Class~., training, test, k=5)
fit_knn2 <- fitted(predict_knn2)
table_knn2 <- table(Actual=test$Class,Fitted=fit_knn2)
acc(table_knn2)

#### Running knn function for k = 10 ####
predict_knn3 <- kknn(formula=Class~., training, test, k=10)
fit_knn3 <- fitted(predict_knn3)
table_knn3 <- table(Actual=test$Class,Fitted=fit_knn3)
acc(table_knn3)

