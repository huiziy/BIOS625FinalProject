load("/Volumes/GoogleDrive/My Drive/BIOS625/GLM_FinalProject/Data_clean/Imputed_5.RData")
## Data Cleaning
imputed1["v000"] <- NULL
library(randomForest)
library(caret)
write.csv(imputed1, file = "/Volumes/GoogleDrive/My Drive/BIOS625/GLM_FinalProject/Data_clean/data_full.csv")
df = read.csv("/Volumes/GoogleDrive/My Drive/BIOS625/GLM_FinalProject/Data_clean/data_full.csv", stringsAsFactors = T)
df["X"] = NULL
df$anemia <- as.factor(df$anemia)
levels(df$anemia) <- c("NO", "YES")
data = df
## Down-sample: Train on 5% of data and evaluate on the rest
set.seed(12345)
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.05,0.95))
df = df[sample, ]
set.seed(12345)
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]
### Build a random forest model
control <- trainControl(method='cv', 
                        number=3, 
                        classProbs = TRUE,
                        verboseIter = TRUE)
#Metric compare model is Accuracy
set.seed(123)
#Number randomly variable selected is mtry
tunegrid <- expand.grid(k = seq(1,200,20))
### Constructing KNN
knn_default <- train(anemia~., 
                    data=train, 
                    method="knn", 
                    metric='Accuracy',
                    preProcess = c("center","scale"),
                    tuneGrid=tunegrid,
                    trControl=control
                    )
## Predicting with KNN
test_pred <- predict(knn_default, newdata = df)
confusionMatrix(test_pred, df$anemia)
result = rf_default$results
ggplot(result, aes(x=k))  + 
  geom_line(aes(y = Accuracy), color="steelblue", linetype="twodash") + 
  geom_line(aes(y = Kappa)) + theme_bw() 
## Plotting Result
coeff <- 1/10
ggplot(result, aes(x=k)) +
  geom_line( aes(y=Accuracy), color="steelblue", linetype="twodash") + 
  geom_line( aes(y=Kappa / coeff), color="red", linetype="twodash") + # Divide by 10 to get the same range than the temperature
  scale_y_continuous(
    # Features of the first axis
    name = "Accuracy",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Kappa")
  ) + theme_bw()
ggsave("/Volumes/GoogleDrive/My Drive/BIOS625/GLM_FinalProject/Data_clean/KNN.png")

### Constructing Random Forest
rf_default <- train(anemia~., 
                     data=train, 
                     method="rf", 
                     metric='Accuracy'
)
test_pred <- predict(rf_default, newdata = df)
confusionMatrix(test_pred, df$anemia)
