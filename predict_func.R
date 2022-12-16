library(AUC)


load("/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Data_clean/fit_use_data.RData")
rm(tempData,df_final,df.list,imputed1, imputed2, imputed4, imputed5)

library(glmmLasso)
library(haven)
library(tidyverse)
library(broom)
## generalized linear mixed model
## with starting values
imputed3$anemia <- as.integer(imputed3$anemia) - 1
######################################## ########## ########## 

self_predict <- function(test = data, obj) {
  wide_data <- model.matrix(anemia~., data = test)
  coefficient <- c(obj$coefficients, obj$ranef[-1])
  p <- inv.logit(wide_data %*% coefficient)
  return(p)
}

self_predict(test = test_data, obj = glm1)

data <- imputed3
data$v024 <- as.factor(data$v024)
df_final <- data[,!names(data) %in% c("country_names","abbre","v000","v005","v007","literacy_rate","poverty_rate")]
df_final <- df_final %>% 
  select(sort(current_vars())) %>% relocate(v024, .after = last_col())

wide_data <- model.matrix(anemia~., data = df_final)
colnames(wide_data)
p <- inv.logit(wide_data %*% coefficient)



lambda <- seq(500,0,by=-5)
BIC_vec<-rep(Inf,length(lambda))
family <- binomial(link = logit)
AUC_vec<-rep(Inf, length(lambda))


# specify starting values for the very first fit
Delta.start<-as.matrix(t(rep(0,41))) #length of fix and random effect (i.e., dim(glm1$Deltamatrix)[2])
Q.start<-0.1 

for(j in 1:length(lambda))
{
  print(paste("Iteration ", j,sep=""))
  glm3 <- glmmLasso(anemia ~  age_group + edu + marriage + job + wealth
                    + head + media + toilet + water + term_preg + housesize
                    + facility_dist + contraceptive  + pregn_now + breastfeeding
                    + residence, rnd = list(v024=~1),
                    data = train_data, family = binomial(link="logit"), lambda=lambda[j], 
                    control = list(print.iter=TRUE, maxIter = 10, steps = 1000))
  y.hat<-predict (glm3, new.data=test_data)   
  AUC_vec[j]<- auc(test_data$anemia, y.hat)
  BIC_vec[j]<-glm3$bic
  Delta.start<-rbind(Delta.start,glm3$Deltamatrix[glm3$conv.step,]) #number of iterations until the main algorithm has converged
  Q.start<-c(Q.start,glm3$Q_long[[glm3$conv.step+1]])
}



opt3<-which.min(BIC_vec)

glm3_final <- glmmLasso(anemia ~  age_group + edu + marriage + job + wealth
                        + head + media + toilet + water + term_preg + housesize
                        + facility_dist + contraceptive  + pregn_now + breastfeeding
                        + residence, rnd = list(v024=~1),   
                        family = family, 
                        data = imputed3, 
                        lambda=lambda[opt3],
                        switch.NR=F,final.re=TRUE,
                        control = list(start=Delta.start[opt3,],q_start=Q.start[opt3]))  
summary(glm3_final)
warnings()