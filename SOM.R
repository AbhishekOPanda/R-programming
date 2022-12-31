# Course      : 2021F CS 513-C
# First Name  : Abhishek 
# Last Name   : Panda
# CWID        : 10478684
# Purpose     : Assignment - HW_10_SOM

#Deleting all the objects from R- environment
rm(list=ls())

#Reading data from file to R and setting missing values to 'NA'
dataSet = read.csv('wisc_bc_ContinuousVar.csv',
                   header=TRUE,
                   sep=",",
                   colClasses = c("diagnosis"="factor")
)

dataSet <- na.omit(dataSet)

dataSet <- dataSet[-1]

#Factorizing the diagnosis column 
dataSet$diagnosis <- factor(dataSet$diagnosis, levels = c('M','B'),labels = c(1,2))

lData <- dataSet[,2:13]
som_grid <- class::somgrid(xdim = 20, ydim=20, topo="hexagonal")
dataSet.som <- class::SOM(lData, som_grid)
plot(dataSet.som)

## 2-phase training
dataSet.som2 <- class::SOM(lData, som_grid,
                           alpha = list(seq(0.05, 0, len = 1e4), seq(0.02, 0, len = 1e5)),
                           radii = list(seq(8, 1, len = 1e4), seq(4, 1, len = 1e5)))
plot(dataSet.som2)

