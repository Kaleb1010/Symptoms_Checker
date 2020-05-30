install.packages('mice')
library(mice)
install.packages('ggplot2')
library(ggplot2)
install.packages('Amelia')
library(Amelia)
install.packages('mlbench')
library(mlbench)
remove.packages("ggplot2")
install.packages("e1071")
library(e1071)
install.packages("caret")
library(caret)
install.packages("caretEnsemble")
library(caretEnsemble)
install.packages("iterators")
library(iterators)

#checking for empty colums or rows
is.na(heart)
sum(is.na(heart))

#splitting into training and testing
ind <- sample(2, nrow(heart), replace = T, prob = c(0.8,0.2))
train <- heart[ind==1, ]
test <- heart[ind==2, ]

#Setting outcome variables as categorical
heart$target <- factor(heart$target, levels = c(0,1), labels = c("False", "True"))
View(heart$target)


#visualizing data
head(heart)
describe(heart)
missmap(heart)
ggplot(heart, aes(age, colour = target)) + geom_freqpoly(binwidth = 1) +labs(title="age Distribution by target")
ggplot()
plot(heart)
hist(heart$trestbps)
ggpairs(heart)

#create objects x which holds the predictor variables and y which holds the response variables
x = train[,-14]
y = train$target
y <- as.factor(y)

#building model
model <- train(x,y,'nb',trControl=trainControl(method='cv',number=10))

warning()
#Model Evaluation
#Predict testing set
Predict <- predict(model, newdata = test)
for (i in 14) {
  print(test[i],Predict[i])
  
}
y <- unlist(y)


#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(Predict, as.factor(test$target))
