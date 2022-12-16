rm(list = ls())
library(rdhs)
library(data.table)
library(ggplot2)
library(survey)
library(haven)
library(lme4)
library(performance)
library(kableExtra)
library(dplyr)
options(mc.cores = parallel::detectCores())
setwd("H:/.shortcut-targets-by-id/1ZATlcMsSk7V-s_381TnmG_KnIDDxgK2w/GLM_FinalProject")
load("H:/.shortcut-targets-by-id/1ZATlcMsSk7V-s_381TnmG_KnIDDxgK2w/GLM_FinalProject/Data_clean/data_cleaned.RData")


df_regre <- df_final
#group age
df_regre["age_group"] = cut(df_regre$age, c(15,19,24,29,34,39,44,49, Inf), c("15-19", "20-24",
                                                                                "25-29","30-34","35-39",
                                                                                "40-44","45-49",">50"), include.lowest=TRUE)
df_regre <- df_regre[,-5] #delete age ,use group age
# omit sample anemia with na.
df_regre <- df_regre[!is.na(df_regre$anemia),]


fit_null <- glmer(anemia ~ (1|df_regre$v024) + (1|df_regre$v000)
                  +(1|df_regre$v007),
                  data = df_regre,family = binomial(),weights = as.numeric(df_regre$v005/1e6),
                  nAGQ= 0 )



fit_individual <- glmer(anemia ~ df_regre$age_group + edu + marriage + job + wealth 
                        + head + media + toilet + water + term_preg + housesize
                        + facility_dist + contraceptive  + pregn_now + breastfeeding
                        + residence + (1|df_regre$v024) + (1|df_regre$v000)
                        +(1|df_regre$v007),
                        data = df_regre, family = binomial(),
                        weights = as.numeric(df_regre$v005/1e6), nAGQ= 0) 

fit_community <- glmer(anemia ~ (1|df_regre$v024) + (1|df_regre$v000)
                       +(1|df_regre$v007)
                       + (0 + df_regre$poverty_rate|df_regre$v024)
                       + (0 + df_regre$literacy_rate|df_regre$v024),
                       data = df_regre,family = binomial(),weights = as.numeric(df_regre$v005/1e6),
                       nAGQ= 0)
# nAGQ= 0 coverger faster
fit_all <- glmer(anemia ~ df_regre$age_group + edu + marriage + job + wealth
                 + head + media + toilet + water + term_preg + housesize
                 + facility_dist + contraceptive  + pregn_now + breastfeeding
                 + residence + (1|df_regre$v024) + (1|df_regre$v000)
                 +(1|df_regre$v007)
                 + (0 + df_regre$poverty_rate|df_regre$v024)
                 + (0 + df_regre$literacy_rate|df_regre$v024),
                 data = df_regre, family = binomial(),weights = as.numeric(df_regre$v005/1e6),
                 nAGQ = 0)
library(dplyr)
library(broom.mixed)
tidy_all <- tidy(fit.all[[1]],conf.int=TRUE,exponentiate=TRUE,effects="fixed") %>% select(-effect,-statistic)%>%
  mutate("95%CI" = paste0("(",round(conf.low,2),",",round(conf.high,2),")",
                          ifelse(p.value<0.05,"*",""))) %>% 
  select(-p.value, -conf.low,-conf.high) 
tidy(fit_all,exponentiate=TRUE,effects="ran_pars")
tidy_all
summary(fit_all)
kableExtra::kable(tidy_all,format = "latex",digits = 2)
library(see)
performance(fit_null)
performance(fit_community)
performance(fit_individual)
performance(fit_all)
t(glance(fit_null))
t(glance(fit_community))
glance(fit_individual)
glance(fit_all)

gl_fit <- cbind(t(glance(fit_null)),t(glance(fit_community)),
                t(glance(fit_individual)),t(glance(fit_all)))
colnames(gl_fit) <- c("NULL","Community","Individual","ALL")
kableExtra::kable(gl_fit,format = "latex",digits = 2)


performance_roc(fit_community)
results <- binned_residuals(fit_community)
check_model(fit_null)
library(broom.mixed)
tidy(fit_all,conf.int=TRUE,exponentiate=TRUE,effects="ran_pars")

glance(fit_all)
# summary estimate results
library(broom.mixed)
tidy(fit_all,conf.int=TRUE,exponentiate=TRUE,effects="ran_pars")


# Fit imputed data and original data with missing, by new age group

load("H:/.shortcut-targets-by-id/1ZATlcMsSk7V-s_381TnmG_KnIDDxgK2w/GLM_FinalProject/Data_clean/imputed_noAnemia.RData")
load("H:/.shortcut-targets-by-id/1ZATlcMsSk7V-s_381TnmG_KnIDDxgK2w/GLM_FinalProject/Data_clean/data_cleaned_v2.RData")

df_final["age_group"] = cut(df_final$age, c(15,24,29,34,44,Inf), 
                            c("15-24","25-29","30-34","35-44",">45"), include.lowest=TRUE)

df_final$anemia <- as.factor(df_final$anemia)

df_final <- df_final[!is.na(df_final$anemia),]

df_final <- df_final[, !names(df_final) %in% c("country_names", "abbre","age")]

imputed1 <- imputed1 %>% mutate(anemia = df_final$anemia,v005 = df_final$v005,age_group = df_final$age_group)
imputed2 <- imputed2 %>% mutate(anemia = df_final$anemia,v005 = df_final$v005,age_group = df_final$age_group)
imputed3 <- imputed3 %>% mutate(anemia = df_final$anemia,v005 = df_final$v005,age_group = df_final$age_group)
imputed4 <- imputed4 %>% mutate(anemia = df_final$anemia,v005 = df_final$v005,age_group = df_final$age_group)
imputed5 <- imputed5 %>% mutate(anemia = df_final$anemia,v005 = df_final$v005,age_group = df_final$age_group)
# list to save all transformed imputed data

colnames(df_final)
colnames(imputed1)
df.list <- list(imputed1,imputed2,imputed3,imputed4,imputed5,df_final)


fit.null <- NULL
fit.individual <- NULL
fit.community <- NULL
fit.all <- NULL

for(i in 1:6){
  fit.null[[i]] <- glmer(anemia ~ (1|v024) + (1|v000)
                         +(1|v007),
                         data = df.list[[i]],family = binomial,weights = as.numeric(v005/1e6),
                         nAGQ= 0 )
  
  fit.individual[[i]] <- glmer(anemia ~  age_group + edu + marriage + job + wealth
                               + head + media + toilet + water + term_preg + housesize
                               + facility_dist + contraceptive  + pregn_now + breastfeeding
                               + residence +  (1|v024) +(1|v000)+(1|v007),
                               data = df.list[[i]], family = binomial,
                               weights = as.numeric(v005/1e6), nAGQ= 0) 
  
  fit.community[[i]] <- glmer(anemia ~ (1|v024) + (1|v000)
                              +(1|v007)
                              + (0 + poverty_rate|v024)
                              + (0 + literacy_rate|v024),
                              data = df.list[[i]],family = binomial,weights = as.numeric(v005/1e6),
                              nAGQ= 0)
  # nAGQ= 0 coverger faster
  fit.all[[i]] <- glmer(anemia ~ age_group + edu + marriage + job + wealth
                        + head + media + toilet + water + term_preg + housesize
                        + facility_dist + contraceptive  + pregn_now + breastfeeding
                        + residence +(1|v024) + (1|v000)
                        +(1|v007)
                        + (0 + poverty_rate|v024)
                        + (0 + literacy_rate|v024),
                        data = df.list[[i]], family = binomial,weights = as.numeric(v005/1e6),
                        nAGQ = 0)
}



