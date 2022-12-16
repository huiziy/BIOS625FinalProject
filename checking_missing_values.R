library(rdhs)
library(data.table)
library(ggplot2)
library(survey)
library(haven)
library(gdata)
library(labelled)
library(Kmisc)

setwd("/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Datasets")

vars_use <- c("v000","v001","v007","v012","v106","v501","v151",
              "v121","v116","v113","v228","v136","v467d","v364","v213",
              "v404","v102","v190","v155","v454", "v455", "v456","v457","v404")
## Didplay variable and variable labels
i <- 1
file_names=dir(pattern="*RData")
dat <- loadRData(file_names[i])[,vars_use]
a <- var_label(dat)

## Checking missing variable labels
missing_var <- list()
count = 1

error_index <- vector()

for (i in 1:length(file_names)) {
  print(i)
  skip_to_next <- FALSE
  tryCatch(dat <- loadRData(file_names[i])[,vars_use], 
           error = function(e) {skip_to_next <<- TRUE})
  if(skip_to_next) {
    dat <- loadRData(file_names[i])
    error_index <- c(error_index,i)
    missing_var[[count]] <- vars_use[!vars_use %in% names(dat)]
    count = count + 1
    next} 
}

names(missing_var) <- file_names[error_index]
missing_table <- unlist(missing_var)
table(missing_table)

