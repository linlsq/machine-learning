# install.packages("caret"); install.packages("randomForest"); install.packages("rpart"); 
library(lattice); 
library(ggplot2); 
library(caret); 
library(randomForest); 
library(rpart); 
library(rpart.plot);

set.seed(1234)

# data load and clean up
trainingset <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!", ""))
testingset <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))

# Perform exploratory analysis - 
# dim(trainingset); dim(testingset); summary(trainingset); summary(testingset); str(trainingset); str(testingset); head(trainingset); head(testingset);               

# Delete columns with all missing values
trainingset<-trainingset[,colSums(is.na(trainingset)) == 0]
testingset <-testingset[,colSums(is.na(testingset)) == 0]

# Delete variables are irrelevant to our current project: user_name, raw_timestamp_part_1, raw_timestamp_part_,2 cvtd_timestamp, new_window, and  num_window (columns 1 to 7). 
trainingset   <-trainingset[,-c(1:7)]
testingset <-testingset[,-c(1:7)]

# partition the data so that 75% of the training dataset into training and the remaining 25% to testing
traintrainset <- createDataPartition(y=trainingset$classe, p=0.75, list=FALSE)
TrainTrainingSet <- trainingset[traintrainset, ] 
TestTrainingSet <- trainingset[-traintrainset, ]

# The variable "classe" contains 5 levels: A, B, C, D and E. A plot of the outcome variable will allow us to see the frequency of each levels in the TrainTrainingSet data set and # compare one another.

plot(TrainTrainingSet$classe, col="yellow", main="Plot of levels of variable classe within the TrainTrainingSet data set", xlab="classe", ylab="Frequency")

#Prediction model 1: Decision Tree
model1 <- rpart(classe ~ ., data=TrainTrainingSet, method="class")

prediction1 <- predict(model1, TestTrainingSet, type = "class")

# Plot the Decision Tree
rpart.plot(model1, main="Classification Tree", extra=102, under=TRUE, faclen=0)

# Test results on our TestTrainingSet data set:
confusionMatrix(prediction1, TestTrainingSet$classe)

Prediction model 2: Random Forest
model2 <- randomForest(classe ~. , data=TrainTrainingSet, method="class")

# Predicting:
prediction2 <- predict(model2, TestTrainingSet, type = "class")

# Test results on TestTrainingSet data set:
confusionMatrix(prediction2, TestTrainingSet$classe)

# predict outcome levels on the original Testing data set using Random Forest algorithm
predictfinal <- predict(model2, testingset, type="class")
predictfinal