#read file 
data=read.csv("Task3.csv")
data
dim(data)

#Desicion Tree (classification)

library(rpart)
library(rpart.plot)#structure
# built tree
tree <- rpart( deposit ~housing+contact +previous+duration+poutcome+month +day + age , data = data, minsplit = 5)

# Make predictions 
predictions <- predict(tree, newdata = data, type = "class")

# Calculate accuracy
accuracy <- sum(predictions == data$deposit) / nrow(data) * 100
cat("Accuracy:", accuracy, "%\n")

# Display the tree if accuracy is higher than or equal to 75%
if (accuracy >= 75) {
  rpart.plot(tree)
} else {
  cat("Accuracy is below 75%.")
}

