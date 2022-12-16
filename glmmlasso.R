#install.packages("glmmLasso")
load("/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Data_clean/fit_use_data.RData")
rm(tempData,df_final,df.list,imputed1, imputed2, imputed4, imputed5)
library(glmmLasso)
library(haven)
library(tidyverse)
library(broom)
## generalized linear mixed model
## with starting values
imputed3$anemia <- as.integer(imputed3$anemia) - 1
#################### ########## ########## ########## ########## 
data <- imputed3 %>% 
  select(sort(current_vars())) %>% relocate(v024, .after = last_col())


### Sample a dataset of 50000 obs for feature selection
nsample = 10000
samp_idx <- sample(seq_len(nrow(data)), nsample, prob=as.numeric(data$v005/1e6))
train_data <- data[samp_idx, ]
train_data = rbind(data[1,],train_data)
train_data = train_data[-1,]

nsample = 5000
dat <- data[-samp_idx,]
samp_idx <- sample(seq_len(nrow(dat)), nsample, prob=as.numeric(dat$v005/1e6))
test_data <- dat[samp_idx, ]
test_data = rbind(data[1,],test_data)
test_data = test_data[-1,]

train_data <- train_data[!names(train_data) %in% c("v000","v005","v007","literacy_rate", "poverty_rate")]
test_data <- test_data[!names(test_data) %in% c("v000","v005","v007","literacy_rate", "poverty_rate")]



# imputed3 <- imputed3[1:50000,]
glm1 <- glmmLasso(anemia ~  age_group + breastfeeding + contraceptive + edu + facility_dist +
                    head + housesize + job + marriage + media + pregn_now + residence + term_preg + 
                    toilet + water + wealth, rnd = list(v024=~1),
                  data = train_data, family = binomial(link="logit"), lambda=50, 
                  control = list(print.iter=TRUE, maxIter = 10, steps = 1000)) 
coefficient <- c(glm1$coefficients, glm1$ranef[-1])


y <- predict(glm1, newdata = data.frame(test_data$age_group,test_data$edu, test_data$anemia), type ='class')

roc(test_data$anemia, as.vector(y), percent=F,   boot.n=1000, ci.alpha=0.9, stratified=FALSE, plot=TRUE, grid=TRUE, show.thres=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    # print.thres = c(0.30,0.35, 0.40, 0.45,0.48, 0.50,0.55, 0.60),#
    print.auc = TRUE, print.thres.col = "blue", ci=TRUE, ci.type="bars", print.thres.cex = 0.7, main = paste("ROC curve using","(N = ",nrow(aSAH),")") )


## exponentiatte the coefficients
exp(glm1$coefficients)

anemia ~  age_group + edu + marriage + job + wealth
+ head + media + toilet + water + term_preg + housesize
+ facility_dist + contraceptive  + pregn_now + breastfeeding
+ residence


anemia ~  age_group + edu + marriage + job + wealth
+ head + media + toilet + water + term_preg + housesize
+ facility_dist + contraceptive  + pregn_now + breastfeeding
+ residence

library(glmmLasso)
library(haven)
library(tidyverse)
library(broom)

data("soccer")
soccer[,c(4,5,9:16)]<-scale(soccer[,c(4,5,9:16)],center=TRUE,scale=TRUE)
soccer<-data.frame(soccer)
set.seed(123)
N<-dim(soccer)[1]
nk = 100
indi <- ind[(i-1)*nk+(1:nk)]
soccer.train<-soccer[-indi,]
soccer.test<-soccer[indi,]


lm1 <- glmmLasso(points ~ transfer.spendings + ave.unfair.score 
                 + ball.possession + tackles 
                 + ave.attend + sold.out, rnd = list(team=~1), 
                 lambda=10, data = soccer.train)

y.hat<-predict(lm1,soccer.test)    



