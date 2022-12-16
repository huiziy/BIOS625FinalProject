library(rdhs)
library(data.table)
library(ggplot2)
library(survey)
library(haven)
library(gdata)
library(CoordinateCleaner)
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

## Reading the datafile from DHS
# setwd("/Volumes/GoogleDrive/My Drive/GLM_FinalProject")
setwd("~/Dropbox/ResearchCode/GLMProject")

country_names <- trim(read.csv("country_names.csv", header = F)$V1)
set_rdhs_config(data_frame = "data.table::as.data.table")

## Checking indivators and anemia related variables
indicators <- dhs_indicators()
tail(indicators[grepl("anemia", Label), .(IndicatorId, ShortName, Label)])
countries <- dhs_countries()
dhscc <- countries[CountryName %in% country_names, DHS_CountryCode]
dhscc

statcomp <- dhs_data(indicatorIds = "AN_ANEM_W_ANY", countryIds = dhscc)
statcomp[,.(Indicator, CountryName, SurveyYear, Value, DenominatorWeighted)]
surveychar <- dhs_survey_characteristics()
surveychar[grepl("anemia", SurveyCharacteristicName, ignore.case=TRUE)]

## Survey 
surveys <- dhs_surveys(surveyCharacteristicIds = 41, countryIds = dhscc)
surveys[,c("SurveyId", "CountryName", "SurveyYear", "NumberOfWomen", "SurveyNum", "FieldworkEnd")]
datasets <- dhs_datasets(surveyIds = surveys$SurveyId, fileType = "IR", fileFormat="flat")
datasets[, c("SurveyId", "SurveyNum", "FileDateLastModified", "FileName")]

## set up your credentials
set_rdhs_config(email = "huizi_yu@brown.edu",
                project = "Reproducing the study on anemia and its associated factors among women of reproductive age in eastern Africa",
                password_prompt  = F)

## checking some data 
datasets$path <- unlist(get_datasets(datasets$FileName))
setwd("/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Datasets")
for (i in 1:length(datasets$FileName)) {
  country_name <- paste(datasets$FileName[i])
  assign(country_name, readRDS(datasets$path[i]))
  save(list=country_name, file = paste0(country_name,".RData"))
}




