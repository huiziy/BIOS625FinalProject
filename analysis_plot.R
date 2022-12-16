## Creating full dataset
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

### Percentage of anemia by group
load("/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Data_clean/data_cleaned.RData")
df_ss <- df_final[,!names(df_final) %in% c("v005","v007")]
names(df_ss) <- c("Country","Region","Age (years)", "Educational level","Marital status", "Occupation", 
               "Household wealth quintile", "Sex of household head", "Media exposure", "Type of toilet facility", 
               "Source of drinking water", "Ever had a terminated pregnancy", "Household size", "Distance from the health facility",
               "Modern contraceptive use", "Currently pregnant", "Currently breastfeeding", 
               "Residence", "Community poverty level", "Community literacy level", "Anemia")

setwd("/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Analysis")
st(df_ss, summ.names = c('N','Mean'),out='latex',file='descriptive_stats.tex')


#
### Trend in distribution of Anemia level by year 
df_final$v000 <- as.factor(substr(df_final$v000,1,2))

anemia_trend <- df_final %>% 
  group_by(v000,v007) %>% 
  dplyr::summarise(Count = n()
            , Ratio = mean(anemia,na.rm = TRUE)) %>% 
  group_by(v000) %>% dplyr::summarise(pct = mean(Ratio,na.rm = TRUE))

ggplot(anemia_trend, aes(x=reorder(v000, -pct), y=pct)) + 
  geom_bar(stat = "identity") + theme_bw() 



