wine<-read.csv("Wine Quality.csv")
set.seed(123)
library(caret)
library(randomForest)
library(dplyr)
library(e1071)
library(GGally)

str(wine)


wine<-wine[ ,-1]
q<-ggcorr(wine, label = F)
print(q)
trainIndex <- createDataPartition(wine$quality, p = 0.8, list = FALSE)
trainData <- wine[trainIndex, ]
testData <- wine[-trainIndex, ]
test.x <- testData[,-12]

mlr<-lm(quality~.,trainData)
s1<-summary(mlr)
print(s1)

svm_model <- svm(quality ~ ., data=trainData, kernel="linear", cost=10)
predictio <- predict(svm_model, newdata=test.x)
predictions <- round(predictio,digits = 0)

confusion_mat <- table(testData$quality, predictions)
accuracy <- sum(diag(confusion_mat))/sum(confusion_mat)
precision <- confusion_mat[2,2]/sum(confusion_mat[,2])
recall <- confusion_mat[2,2]/sum(confusion_mat[2,])
f1_score <- 2*(precision*recall)/(precision+recall)


cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-score:", f1_score, "\n")

