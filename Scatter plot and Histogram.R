#Load the “breast-cancer-wisconsin.data.csv” into R.
data_Set <- read.csv("D:/stevens/3_sem/cs513/breast-cancer-wisconsin.csv", na.string = "?")

#Summarizing each column (e.g. min, max, mean )
summary(data_Set)

#Identifying missing values
is.na(data_Set)
View(data_Set)

#Replacing the missing values with the “mean” of the column.
for(i in 1:ncol(data_Set)){
  data_Set[is.na(data_Set[,i]), i] <- mean(data_Set[,i], na.rm = TRUE)
}

#Rounding Values to first 2 decimal for simplicity
data_Set[,-1] <-round(data_Set[,-1],2)

#Displaying the frequency table of “Class” vs. F6
new_Table <- table(data_Set$Class, data_Set$F6)
ftable(new_Table)

#Displaying the scatter plot of F1 to F6, one pair at a time
plot(data_Set[2:7], main = "Scatter Plot of F1 to F6", ph = 10, col = 2)

#Show histogram box plot for columns F7 to F9
boxplot(data_Set[8:10], main = "Histogram Box Plot for Columns F7 to F9")

#Delete all the objects from your R- environment. 
rm(list=ls())

#Reload the “breast-cancer-wisconsin.data.csv” from canvas into R. 
data_Set <- read.csv("D:/stevens/3_sem/cs513/breast-cancer-wisconsin.csv", na.string = "?")

#Remove any row with a missing value in any of the columns.
data_Set <- na.omit(data_Set)
data_Set

