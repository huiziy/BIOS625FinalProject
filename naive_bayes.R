#Loading required packages
library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(rpart)
library(randomForest)
library(dplyr)
library(e1071)

df = read.csv("imputed1.csv", stringsAsFactors = T)
df["X"] = NULL
#df$anemia <- as.factor(df$anemia)
#df <- df[-1]
#data <- df
#data <- df %>% sample_frac(0.05)

df["X"] = NULL
df$anemia <- as.factor(df$anemia)
levels(df$anemia) <- c("NO", "YES")
## Down-sample: Train on 5% of data and evaluate on the rest
set.seed(12345)
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.05,0.95))
df = df[sample, ]
set.seed(12345)
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]











#Building a model
#split data into training and test data sets
#set.seed(123)

#indxTrain <- createDataPartition(y = data$anemia,p = 0.70,list = FALSE)
#training <- data[indxTrain,]
#testing <- data[-indxTrain,] 

prop.table(table(train$anemia)) * 100
prop.table(table(test$anemia)) * 100

#create objects x which holds the predictor variables and y which holds the response variables
x = train[,-19]
y = train$anemia
y <-as.factor(y)

# create naive bayes model 
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

#Model Evaluation
#Predict testing set
Predict <- predict(model,newdata = test ) 
#Get the confusion matrix to see accuracy value and other parameter values
test$anemia <- as.factor(test$anemia )
confusionMatrix(Predict, test$anemia)