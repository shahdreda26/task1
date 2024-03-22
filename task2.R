#read file 
data=read.csv("Task2.csv")
data
dim(data)
#************************************
#cleaning of data 
##duplicate
sum(duplicated(data)) #number of duplicate in all data set
library(dplyr)
task2=distinct(data)#remove duplicate
dim(data)

##null 
sum(is.na(data))#number of null in all data set
new_data=na.omit(data)# remove null
dim(new_data)
#*************************************
#structure data
#numeric
is.numeric(new_data$PassengerId)# check data
is.integer(new_data$Survived)
is.integer(new_data$Pclass)
is.integer(new_data$Age)
newAge=as.integer(new_data$Age)
is.integer(newAge)
is.integer(new_data$SibSp)
is.integer(new_data$Parch)
is.numeric(new_data$Fare)

#character
is.character(new_data$Name)
is.character(new_data$Sex)
is.character(new_data$Cabin)
is.character(new_data$Embarked)
is.character(new_data$Ticket)
#****************************************
head(data) # the firsrt 6 row
tail(data) #the last 6 row
sample(data) # some random row from data 
summary(data) # give me descripe of data
#***************************************

#relation between variable.
library(shiny)
# Define UI
ui <- fluidPage(
  titlePanel("Visualiztion"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", "Choose a plot type:", choices = c("Pie", "Histogram", "Boxplot", "Scatterplot", "Cluster")),
      
      conditionalPanel(
        condition = "input.plotType == 'Pie'",
        selectInput("pieVariable", "Choose a variable:", choices = c("Sex","Pclass", "Survived"))  ),
      
      conditionalPanel(
        condition = "input.plotType == 'Histogram'",
        selectInput("histogramVariable", "Choose a variable:", choices = c("Age"))  ),
      
      conditionalPanel(
        condition = "input.plotType == 'Boxplot'",
        selectInput("boxplotVariable", "Choose a variable:", choices = c("PassengerId"))  ),
      
      conditionalPanel(
        condition = "input.plotType == 'Scatterplot'",
        selectInput("scatterplotVariableX", "Choose X variable:", choices = c("Ticket", "Pclass")),
        selectInput("scatterplotVariableY", "Choose Y variable:", choices = c("Fare", "Age"))),
      
      conditionalPanel(
        condition = "input.plotType == 'Cluster'",
        numericInput("numClusters", "Number of Clusters:", value = 3, min = 1))  ),
    
    mainPanel(
      plotOutput("plot"))))

# Define server
server <- function(input, output) {
  output$plot <- renderPlot({
    
    # Similar from pie chart types
    if (input$plotType == "Pie") { 
      if (input$pieVariable == "Sex") {
        ui <- table(new_data$Sex)
        percentage <- paste0(round(100 * ui / sum(ui)), "%")
        pie(ui, labels = percentage, main = "Sex", col = c("skyblue", "pink"))
        legend("bottomright", legend = c("Male", "Female"), fill = c("skyblue", "pink"))}
      
      else if (input$pieVariable == "Pclass") {
        ui <- table(new_data$Pclass)
        percentage <- paste0(round(100 * ui / sum(ui)), "%")
        pie(ui, labels = percentage, main = "Pclass", col = c("skyblue","seagreen", "yellow"))
        legend("bottomright", legend = c("1", "2","3"), fill = c("skyblue","seagreen", "yellow"))}
      
      else if (input$pieVariable == "Survived") {
        ui <- table(new_data$Survived)
        percentage <- paste0(round(100 * ui / sum(ui)), "%")
        pie(ui, labels = percentage, main = "Survived", col = c("gold", "gray"))
        legend("bottomright", legend = c("0", "1"), fill = c("gold", "gray"))}}
    
    else if (input$plotType == "Histogram") {
      hist(new_data$Age, col = "violet", border = "black", main = "Frequency Of Age", xlab = "Age", ylab = "Frequency")}
    
    else if (input$plotType == "Boxplot") {
      boxplot(x = new_data$PassengerId, main = "PassengerId", xlab = "PassengerId")}
    
    else if (input$plotType == "Scatterplot") {
      plot(x = new_data[[input$scatterplotVariableX]], y = new_data[[input$scatterplotVariableY]],
           main = paste(input$scatterplotVariableX, "vs", input$scatterplotVariableY),
           xlab = input$scatterplotVariableX, ylab = input$scatterplotVariableY, col = "blue") }
    
    # Add clustering code for the selected variables (Pclass and Fare)
    else if (input$plotType == "Cluster") {
      clusters <- kmeans(new_data[, c("Pclass", "Fare")], centers = input$numClusters)
      plot(new_data$Pclass, new_data$Fare, col = clusters$cluster, pch = 19,xlab = "Pclass", ylab = "Fare")
      points(clusters$centers[, 1], clusters$centers[, 2], col = 1:input$numClusters, pch = 3, cex = 2)
      legend("topright", legend = paste0("Cluster ", 1:input$numClusters), col = 1:input$numClusters, pch = 19)}} )}

# Run the application
shinyApp(ui = ui, server = server)



