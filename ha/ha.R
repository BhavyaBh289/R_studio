wine<-read.csv("Wine Quality.csv")
set.seed(123)
library(caret)
library(randomForest)
library(dplyr)

q<-ggcorr(wine, label = T)
print(q)
str(wine)
wine<-wine[ ,-c(1,2,7)]
wine$quality<-as.factor(wine$quality)

set.seed(123)


train <- sample(nrow(wine), 0.8 * nrow(wine))
train_data <- wine[train, ]
test_data <- wine[-train, ]

#Random Forest 
model <- randomForest(as.factor(quality) ~ ., data = train_data, ntree = 1000)

predictions <- predict(model, test_data)

conf_matrix <- table(test_data$quality, predictions)
print(conf_matrix)


accuracy_rf <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("\nAccuracy of Random Forest:", accuracy_rf, "\n")

# Naive Bayes
model_nb <- naiveBayes(quality ~ ., data = train_data)

nb_predictions <- predict(model_nb, newdata = test_data)
confusion_matrix <- table(test_data$quality, nb_predictions)
print(confusion_matrix)

accuracy_nb <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("\nAccuracy of Naive Bayes:", accuracy_nb, "\n")

#SVM
svm_model <- svm(quality ~ ., data = train_data, kernel = "linear", cost = 10)

svm_pred <- predict(svm_model, newdata = test_data)

svm_cm <- table(svm_pred, test_data$quality)
print(svm_cm)
accuracy_svm <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("\nAccuracy of SVM: ",accuracy_svm)

rf_metrics <- caret::confusionMatrix(predictions, test_data$quality)
print(rf_metrics)

nb_metrics <- caret::confusionMatrix(nb_predictions, test_data$quality)
print(nb_metrics)

svm_metrics <- caret::confusionMatrix(svm_pred, test_data$quality)
print(svm_metrics)
