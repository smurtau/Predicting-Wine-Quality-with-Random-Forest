#Author: Samuel Murtaugh
#Date: 12/1/2020
#Task: 6300 Solo Project
#Purpose: Wine Quality Prediction with Random Forest

#Read Libraries, set up seed
library(ggplot2)
library(randomForest)
install.packages("caret")
library(caret)
set.seed(102)

#Read in data for red and white wine
redWine <- read.table("winequality-red.csv", sep=";", header=T)
whiteWine <- read.table("winequality-white.csv", sep=";",header=T)
dim(redWine)
dim(whiteWine)
#Combine data for both types of wine
allWine <- rbind(redWine,whiteWine)
dim(allWine)
summary(allWine)

#Quality is a rating from 1-10, not a variable, so factorize it
allWine$quality <- as.factor(allWine$quality)

#Explore the distribution of wine quality data with bar chart
ggplot(allWine, aes(x=quality)) + geom_bar()

#create test set and training set
N <- nrow(allWine)
test <- sample(1:N, (N/6))
train <- seq(1:N)[-test]

#train random forest models to "classify" a wine as a quality from 1-10
#based on its features
P <- ncol(allWine)-1

#We use mtry=sqrt(P) since this is a classification problem
tree.quality <- randomForest(quality~., data=allWine, subset=train,mtry=sqrt(P),importance=TRUE)

#Find Error rates
allWine.test <- allWine[test, "quality"]
yhat.tree <- predict(tree.quality,allWine[test,])


#Create confusion matrix and view model
conf <- table(yhat.tree,allWine[test,]$quality)
print(conf)
confusionMatrix(conf)
tree.quality
varImpPlot(tree.quality)
importance(tree.quality)s