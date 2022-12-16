library(rdhs)
library(data.table)
library(ggplot2)
library(survey)
library(haven)

country_names <- read.csv("country_names.csv", header = F)

set_rdhs_config(data_frame = "data.table::as.data.table")

indicators <- dhs_indicators()
tail(indicators[grepl("anemia", Label), .(IndicatorId, ShortName, Label)])
countries <- dhs_countries()
dhscc <- countries[CountryName %in% c("Ethiopia"), DHS_CountryCode]
dhscc

statcomp <- dhs_data(indicatorIds = "AN_ANEM_W_ANY", countryIds = dhscc)
statcomp[,c(Indicator, CountryName, SurveyYear, Value, DenominatorWeighted)]

surveychar <- dhs_survey_characteristics()
surveychar[grepl("anemia", SurveyCharacteristicName, ignore.case=TRUE)]

surveys <- dhs_surveys(surveyCharacteristicIds = 41, countryIds = dhscc)
surveys[,c("SurveyId", "CountryName", "SurveyYear", "NumberOfWomen", "SurveyNum", "FieldworkEnd")]

datasets <- dhs_datasets(surveyIds = surveys$SurveyId, fileType = "IR", fileFormat="flat")
datasets[, c("SurveyId", "SurveyNum", "FileDateLastModified", "FileName")]

## set up your credentials
set_rdhs_config(email = "huizi_yu@brown.edu",
                project = "Reproducing the study on anemia and its associated factors among women of reproductive age in eastern Africa",
                password_prompt  = T)

## checking some data 
datasets$path <- unlist(get_datasets(datasets$FileName))
head(search_variable_labels(datasets$FileName[3], "literacy")[,1:2])
ir <- readRDS(datasets$path[2])
table(as_factor(ir$v012))
table(as_factor(ir$v155))

datlst <- list()
vars <- c("SurveyId", "CountryName", "SurveyYear", "v000", "v001", "v005",
          "v012", "v024", "v025", "v106", "v042", "v454", "v455", "v456")

vars_use <- c("SurveyId","CountryName","SurveyYear","v012","v106","v501","v151",
              "v121","v116","v113","v228","v136","v467d","v364","v213",
              "v3a08g","v102","v190","v155","v454", "v455", "v456")
vars <- c(
          "marital status",
          "wealth status",
          "sex of household head"
)
datlst <- list()

for(i in 1:nrow(datasets)){
  
  if(file.exists(datasets$path[i])){
    
    print(paste(i, datasets$SurveyId[i]))
    ir <- readRDS(datasets$path[i])
    
    ir$SurveyId <- datasets$SurveyId[i]
    ir$CountryName <- datasets$CountryName[i]
    ir$SurveyYear <- datasets$SurveyYear[i]
    
    datlst[[datasets$SurveyId[i]]] <- ir[vars]
  }
}

datlst$LS2014DHS
 