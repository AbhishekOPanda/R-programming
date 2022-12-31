# Course      : 2021F CS 513-C
# First Name  : Abhishek 
# Last Name   : Panda
# CWID        : 10478684
# Purpose     : Assignment - HW_08_Cluster


#Deleting all the objects from R- environment
rm(list=ls())

#Reading data from file to R and setting missing values to 'NA'
dataSet = read.csv('wisc_bc_ContinuousVar.csv',
                   header=TRUE,
                   sep=",",
                   na.strings = "?",
                   )

dataSet <- na.omit(dataSet)

#Factorizing the Dataset
dataSet$diagnosis <- factor(dataSet$diagnosis, levels = c('M','B'),labels = c(1,2))

#### 8.1 Performing Hierarchical Clustering
distance_matrix <- dist(dataSet[,-1])
hclust_result <- hclust(distance_matrix) 
plot(hclust_result)
hclust_2<-cutree(hclust_result,2)
table(hclust_2,dataSet[,1])

#### 8.2 Performing k-Means Clustering ####
kmeans_cluster<- kmeans(dataSet[,-1],2,nstart = 10)
kmeans_cluster$cluster
table(kmeans_cluster$cluster,dataSet[,1])
