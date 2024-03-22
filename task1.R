

data=read.csv("Task1.csv")
#read file in R
data #print file
dim(data)
# number of rows and cols


#cleaning data
#duplicate
sum(duplicated(data)) #number of duplicate in all data set
library(dplyr)
new_data=distinct(data)#remove duplicate
dim(new_data)
#null
sum(is.na(data))#number of null in all data set
new_data=na.omit(data)# remove null
dim(new_data)

# visualize of Age 

new_data$Population.age.composition = as.numeric(new_data$Population.age.composition)
#convert Age type from any data type to numeric data

barplot(table(new_data$Population.age.composition), main ="Frequence Of Age ", xlab ="Age" , ylab = "Frequency" , col = "yellow"  )
#table : generates a frequency table of the "Age" column, Shows how many times a value is repeated in the table



