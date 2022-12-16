## Creating full dataset
rm(list = ls())
library(rdhs)
library(data.table)
library(ggplot2)
library(survey)
library(haven)
library(gdata)
library(labelled)
library(dplyr)
library(plyr)
library(Kmisc)
library(vtable)
library(mice)
library(VIM)
library(regclass)
library(car)
library(miceFast)


### Percentage of anemia by group
load("/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Data_clean/data_cleaned_v2.RData")
df_final <- df_final[!is.na(df_final$anemia),]

### remove unused variables 
df_final <- df_final[, !names(df_final) %in% c("country_names", "abbre","v024", "v005", "v007")]
df_final$v000 <- as.factor(df_final$v000)
df_final$anemia <- as.factor(df_final$anemia)

### check multicollinearity
model <- glm(anemia ~. , data = df_final, family = binomial)
summary(model)
vif(model)

### Check missingness
tempData <- mice(df_final, m=5, maxit = 5, method = "pmm")

summary(tempData)
imputed1 <- complete(tempData,1)
imputed2 <- complete(tempData,2)
imputed3 <- complete(tempData,3)
imputed4 <- complete(tempData,4)
imputed5 <- complete(tempData,5)

colSums(is.na(imputed1))

save.image(file = "/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Data_clean/Imputed_5.RData")
