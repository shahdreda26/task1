

data=read.csv("Task1.csv")
#read file in R
data
dim(data)
# number of rows and cols

# visualize of Age 

new_data$Population.age.composition = as.numeric(new_data$Population.age.composition)
#convert Age type from any data type to numeric data

barplot(table(new_data$Population.age.composition), main ="Frequence Of Age ", xlab ="Age" , ylab = "Frequency" , col = "yellow"  )
#table : generates a frequency table of the "Age" column, Shows how many times a value is repeated in the table



