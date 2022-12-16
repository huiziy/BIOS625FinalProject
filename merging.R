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

setwd("/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Datasets")

vars_use <- c("v000","v024","v005","v007","v012","v106","v501","v151",
              "v121","v116","v113","v228","v136","v364","v213",
              "v404","v102","v190","v155","v322", "v717", "v454", "v455", "v456", "v467d")

df <- loadRData(file_names[1])[,vars_use]
## Checking missing variable labels
missing_var <- list()
count = 1
file_names=dir(pattern="*RData")
error_index <- vector(mode = "numeric")

for (i in 2:length(file_names)) {
  print(i)
  skip_to_next <- FALSE
  tryCatch(dat <- loadRData(file_names[i])[,vars_use], 
           error = function(e) {skip_to_next <<- TRUE})
  if(skip_to_next) {
    dat <- loadRData(file_names[i])
    if ((sum(c("v454", "v455", "v456") %in% names(dat)) == 3) == T) {
      # has all three dependent variables
      error_index <- c(error_index,i)
      missing_var[[count]] <- vars_use[!vars_use %in% names(dat)]
      count = count + 1
      vars <- vars_use[vars_use %in% names(dat)]
      dat <- dat[vars]
      df <- rbind.fill(df,dat)}
    next
    } else {
      df <- bind_rows(df,dat)
    }
}

## check missing values
colSums(is.na(df)) / nrow(df)

## remove all missing v454 v455 and v456
df <- df[!(is.na(df$v454) | is.na(df$v455) | is.na(df$v456)),]
save(df, file = "/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Data_clean/data_full.RData")
load("/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Data_clean/data_full.RData")

df_full <- df
### age
colnames(df)[names(df) == "v012"] <- "age"
### categorize variables
#### Education
df$edu <- as.factor(ifelse(df$v106 == 9 | df$v106 == 6, NA, df$v106))
levels(df$edu) <- c("No Education","Primary","Secondary","Higher")
#### Marital Status 
df$marriage <- ifelse(df$v501 == 1 | df$v501 == 2, 1, df$v501)
df$marriage <- ifelse(df$v501 == 3 | df$v501 == 4 | df$v501 == 5, 3, df$marriage)
df$marriage <- ifelse(df$v501 == 9, NA, df$marriage)
df$marriage <- as.factor(df$marriage)
levels(df$marriage) <- c("Never married","Married","Divorced/widowed/separated")
#### Occupation
df$job <- ifelse(df$v717 >= 97, NA, df$v717)
df$job <- ifelse(df$v717 == 0, 0, df$job)
df$job <- as.factor(ifelse((df$job != 0 & df$v717 <= 97), 1, df$job))
levels(df$job) <- c("Not working", "Working")
#### Household wealth quintile
df$wealth <- as.factor(ifelse(df$v190 == 9, NA, df$v190))
levels(df$wealth) <- c("Poorest","Poorer","Middle","Richer","Richest")
#### Sex of householdhead
df$head <- as.factor(ifelse(df$v151 == 9, NA, df$v151))
levels(df$head) <- c("Male","Female")
#### Media exposure
df$media <- as.factor(ifelse(df$v121 == 9 | df$v121 == 7, NA, df$v121))
levels(df$media) <- c("No","Yes")
#### Type of toilet facility 
df$toilet <- as.factor(ifelse(df$v116 == 97 | df$v116 == 99, NA, df$v116))
## improved (flush)
df$toilet <- as.factor(ifelse(df$v116 >= 10 | df$v116 < 20, 1, df$toilet))
## unimproved (latrine etc)
df$toilet <- as.factor(ifelse(df$v116 >= 20, 0, df$toilet))
levels(df$toilet) <- c("Unimproved", "Improved")
#### Source of drkinking water 
df$water <- as.factor(ifelse(df$v113 == 97 | df$v113 == 99, NA, df$v113))
## improved (pipped water)
df$water <- as.factor(ifelse(df$v113 >= 10 | df$v113 <=  20 | df$v113 == 71, 1, df$water))
## unimproved (well and others)
df$water <- as.factor(ifelse(df$v113 >  20 & df$v113 != 71, 0, df$water))
levels(df$water) <- c("Unimproved", "Improved")
### Terminated pregnancy
df$term_preg <- as.factor(ifelse(df$v228 == 9, NA, df$v228))
levels(df$term_preg) <- c("No", "Yes")
### Parity 
# df$parity <- 
### Household size 
df$housesize <- as.factor(ifelse(df$v136 <= 2, 0, df$v136))
df$housesize <- as.factor(ifelse(df$v136 > 3 & df$v136 <= 5, 1, df$housesize))
df$housesize <- as.factor(ifelse(df$v136 > 6, 3, df$housesize))
levels(df$housesize) <- c("1-2", "3-5", "6 and above")
### Distance to facility 
df$facility_dist <- as.factor(ifelse(df$v467d == 9, NA, df$v467d))
df$facility_dist <- as.factor(ifelse(df$v467d == 1, 1, df$facility_dist))
df$facility_dist <- as.factor(ifelse(df$v467d != 1 & df$v467d != 9, 0, df$facility_dist))
levels(df$facility_dist) <- c("Not a big problem", "Big Problem")
### Modern contraceptive use 
df$contraceptive <- as.factor(ifelse(df$v364 == 9, NA, df$v364))
df$contraceptive <- as.factor(ifelse(df$v364 == 1, 1, df$contraceptive))
df$contraceptive <- as.factor(ifelse(df$v364 != 1 & df$v364 != 9, 0, df$contraceptive))
levels(df$contraceptive) <- c("No", "Yes")
### Currently pregnant
df$pregn_now <- as.factor(ifelse(df$v213 == 9, NA, df$v213))
levels(df$pregn_now) <- c("No", "Yes")
### Currently breastfeed
df$breastfeeding <- as.factor(ifelse(df$v404 == 9, NA, df$v404))
levels(df$breastfeeding) <- c("No", "Yes")
### Residence
df$residence <- as.factor(ifelse(df$v102 == 9, NA, df$v102))
levels(df$residence) <- c("Urban", "Rural")
### Community poverty level (group by survey year, country, and region)
df <- df %>% group_by(v000, v024, v007) %>% dplyr::mutate(cnt = n()) %>% dplyr::mutate(pov_count = sum(wealth == "Poorest" | wealth == "Poorer", na.rm = T)) %>%
 dplyr::mutate(poverty_rate =  pov_count / cnt)
### Community literacy level (can read whole sentences)
df <- df %>% group_by(v000, v024, v007) %>% dplyr::mutate(literate_count = sum(v155 == 2, na.rm = T)) %>% 
  dplyr::mutate(literacy_rate =  literate_count / cnt)
### Response variable 
df$anemia <- ifelse(df$v456 >= 994, NA, df$v456)
df$anemia <- as.integer(df$anemia  < ifelse(df$v454 == 1, 110, 120))

### remove columns 
var_names <- c("v155","v322","v228","v454","v455","v456","v106","v501","v717","v190","v151","v121","v116","v113","228","v136","v467d","v364","v213","v404","v102","cnt","pov_count","literate_count")
df_final <- df[, !names(df) %in% var_names]
df_final$abbre <- substr(df_final$v000,1,2)
save(df_final, file = "/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Data_clean/data_cleaned.RData")
write.csv(df_final, file = "/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Data_clean/data_cleaned.csv")

### Extract country names
countries <- dhs_countries()
setwd("~/Dropbox/ResearchCode/GLMProject")
country_names <- trim(read.csv("country_names.csv", header = F)$V1)
dhscc <- countries[CountryName %in% country_names, DHS_CountryCode]
country_names <- cbind(country_names, dhscc)

colnames(country_names)[2] <- "abbre"
df_merged <- merge(df_final, country_names, by = "abbre")

df_final <- df_merged %>%
  select(country_names, everything())
save(df_final, file = "/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Data_clean/data_cleaned_v2.RData")



