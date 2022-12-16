library(rdhs)
library(data.table)
library(ggplot2)
library(survey)
library(haven)
library(lme4)
library(performance)
library(kableExtra)
setwd("H:/.shortcut-targets-by-id/1ZATlcMsSk7V-s_381TnmG_KnIDDxgK2w/GLM_FinalProject")
load("H:/.shortcut-targets-by-id/1ZATlcMsSk7V-s_381TnmG_KnIDDxgK2w/GLM_FinalProject/Data_clean/data_cleaned.RData")

df_regre <- df_final
#group age
df_regre["age_group"] = cut(df_regre$age, c(0, 14,19,24,29,34,39,44,49, Inf), c("0-14", "15-19", "20-24",
                                                                                "25-29","30-34","35-39",
                                                                                "40-44","45-49",">50"), include.lowest=TRUE)
df_regre <- df_regre[,-5] #delete age ,use group age


p_vector <- NULL
bi_table<- NULL
for(i in names(df_regre[,c(2,5:19,23)])){
  re <-  table(df_regre[[i]],df_regre$anemia)
  bi_table <- rbind(bi_table,re)
  p <- chisq.test(df_regre[[i]],df_regre$anemia)$p.value
  p_vector <- cbind(p_vector,p)
}
chisq.test(df_regre$job,df_regre$anemia)
dim(table(df_regre$v024,df_regre$anemia))
kableExtra::kable(bi_table[1:63,], caption = "Bi variable analysis for country_year variable" ,format = "latex",booktabs = TRUE) #contry_year 
kableExtra::kable(bi_table[64:(64+49-1),], caption = "Bi variable analysis for group variable" ,format = "latex",booktabs = TRUE) #
kableExtra::kable(bi_table[c(150:158,(64+49):149),], caption = "Bi variable analsis for other variable",format = "latex",booktabs = TRUE)
