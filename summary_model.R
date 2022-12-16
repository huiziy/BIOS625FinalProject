library(rdhs)
library(data.table)
library(ggplot2)
library(survey)
library(haven)
library(lme4)
library(performance)
library(kableExtra)
library(dplyr)
library(broom.mixed)
library(see)
library(xtable)
library(stargazer)
library(mice)
library(mitml)
load("H:/.shortcut-targets-by-id/1ZATlcMsSk7V-s_381TnmG_KnIDDxgK2w/GLM_FinalProject/fitted models/new_fitmodel.RData")

tidy(all.model,conf.int=TRUE,exponentiate=TRUE,effects=c("fixed")) %>% select(-effect,-statistic)%>%
  mutate("95%CI" = paste0("(",round(conf.low,2),",",round(conf.high,2),")",
                          ifelse(p.value<0.05,"*",""))) %>% 
  select(-p.value, -conf.low,-conf.high) %>% kable(format = "latex",booktabs = TRUE,escape = F, align = "c", digits = 3) %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))%>%
  kable_material(c("striped", "hover", "condensed"))

tidy(all.model,exponentiate=TRUE,effects=c("ran_pars")) %>% kable(format = "latex",booktabs = TRUE,escape = F, align = "c", digits = 3) %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))%>%
  kable_material(c("striped", "hover", "condensed"))


table(df_final$age_group)

#### get regression results and CI from all full.
tidy(fit.all[[1]],conf.int=TRUE,exponentiate=TRUE,effects="fixed") %>% select(-effect,-statistic)%>%
  mutate("95%CI" = paste0("(",round(conf.low,2),",",round(conf.high,2),")",
                          ifelse(p.value<0.05,"*",""))) %>% 
  select(-p.value, -conf.low,-conf.high) 


### bivariable 

bi_matrix <- df_final$edu
for(i in bi_results){
  rnam <- rownames(i)
  rnam_bi <- rownames(bi_matrix)
  bi_matrix <- rbind(bi_matrix,i)
  rownames(bi_matrix) <- c(rnam_bi,rnam)
}

bi_table<- NULL
for(i in names(df_final[,c(1,2,4,5:19,23)])){
  re <-  table(df_final[[i]],df_final$anemia)
  bi_table <- rbind(bi_table,re)
}
table(df_final$v024,df_final$anemia)

bi_table[120,]
bi_group <- as.data.frame(bi_table[72:120,])

table(df_final$v024,df_final$anemia) %>%  kable(caption = "bi vairable analysis of country",
                                                format = "latex",booktabs = TRUE,escape = F, align = "c", digits = 3) %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))%>%
  kable_material(c("striped", "hover", "condensed"))

as.data.frame(bi_table[142:183,]) %>%  kable(caption = "bi vairable analysis of country",
                                               format = "latex",booktabs = TRUE,escape = F, align = "c", digits = 3) %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))%>%
  kable_material(c("striped", "hover", "condensed"))

table(df_final, df_final$anemia)
bi_table

tidy.all <- NULL
tidy.null <- NULL
tidy.individual <- NULL
tidy.community <- NULL

for(i in 1:6){
  tidy.null[[i]] <- tidy(fit.null[[i]],exponentiate=TRUE,effects = c("fixed", "ran_pars")) %>% 
    select(-effect,-statistic,-p.value,-std.error)
  #c.tidy.null <- cbind(c.tidy.null,tidy.null[[i]][,3])
  tidy.individual[[i]] <- tidy(fit.individual[[i]],exponentiate=TRUE,effects = c("fixed", "ran_pars")) %>% 
    select(-effect,-statistic,-p.value,-std.error)
  tidy.community[[i]] <- tidy(fit.community[[i]],exponentiate=TRUE,effects = c("fixed", "ran_pars")) %>% 
    select(-effect,-statistic,-p.value,-std.error)
  tidy.all[[i]] <- tidy(fit.all[[i]],exponentiate=TRUE,effects = c("fixed", "ran_pars")) %>% 
    select(-effect,-statistic,-p.value,-std.error)
}


c.tidy.null <- tidy.null[[6]][,1:3]
c.tidy.individual <- tidy.individual[[6]][,1:3]
c.tidy.community <- tidy.community[[6]][,1:3]
c.tidy.all <- tidy.all[[6]][,1:3]

for(i in 1:5){
  c.tidy.null <- cbind(c.tidy.null,tidy.null[[i]][,3])
  c.tidy.individual <-cbind(c.tidy.individual,tidy.individual[[i]][,3])
  c.tidy.community <-cbind(c.tidy.community,tidy.community[[i]][,3])
  c.tidy.all <- cbind(c.tidy.all,tidy.all[[i]][,3])
}
colnames(c.tidy.null) <- c("group","term","original",paste0("imputed",1:5))
colnames(c.tidy.individual)  <- c("group","term","original",paste0("imputed",1:5))
colnames(c.tidy.community)  <- c("group","term","original",paste0("imputed",1:5))
colnames(c.tidy.all) <- c("group","term","original",paste0("imputed",1:5))
###  latex output of seperate imputation

xtidy.null <- c.tidy.null[,c(1,2,4:8)]%>%
  kable(format = "latex",  booktabs = TRUE,escape = F, align = "c", digits = 4) %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))%>%
  kable_material(c("striped", "hover", "condensed"))
xtidy.individual <- c.tidy.individual[,c(1,2,4:8)]%>%
  kable(format = "latex",booktabs = TRUE,escape = F, align = "c", digits = 4) %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))%>%
  kable_material(c("striped", "hover", "condensed"))
xtidy.community <- c.tidy.community[,c(1,2,4:8)]%>%
  kable(format = "latex",booktabs = TRUE,escape = F, align = "c", digits = 4) %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))%>%
  kable_material(c("striped", "hover", "condensed"))
xtidy.all <- c.tidy.all[,c(1,2,4:8)]%>%
  kable(format = "latex",booktabs = TRUE,escape = F, align = "c", digits = 4) %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))%>%
  kable_material(c("striped", "hover", "condensed"))

tidy.all %>% kbl(caption = "regression results for imputed 1", 
    col.names = linebreak(c("term", "estimate", 
                            "std.error", "$95\\%CI$")), 
    booktabs = T, escape = F, align = "c", digits = 2) %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))%>%
  kable_material(c("striped", "hover", "condensed"))

library(see)
performance(fit.null[[6]])
performance(fit.community[[6]])
performance(fit.individual[[6]])
performance(fit.all[[6]])
t(glance(fit.null[[1]]))
t(glance(fit_community))
glance(fit_individual)
glance(fit_all)

gl.fit <- cbind(t(glance(fit.null[[6]])),t(glance(fit.community[[6]])),
                t(glance(fit.individual[[6]])),t(glance(fit.all[[6]])))

colnames(gl.fit) <- c("NULL","Community","Individual","Full")

xtable.gl <- gl.fit%>%
  kable(format = "latex",booktabs = TRUE,escape = F, align = "c", digits = 4) %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))%>%
  kable_material(c("striped", "hover", "condensed"))

# save all original data fitted model
null.model <- fit.null[[6]]
community.model <- fit.community[[6]]
individual.model <- fit.individual[[6]]
all.model <- fit.all[[6]]

compar<- compare_performance(null.model,community.model,
                             individual.model,all.model,rank = TRUE)
compar$Sigma <- NULL
compar$Score_log <- NULL
compar$Log_loss <- NULL
compar$R2_conditional <- NULL
plot(compar)



library(broom.mixed)
tidy(fit.all,conf.int=TRUE,exponentiate=TRUE,effects="ran_pars")

glance(fit.all)
# summary estimate results
library(broom.mixed)
tidy(fit.all[[1]],conf.int=TRUE,exponentiate=FALSE,effects="ran_pars")

summary(pool(fit.all))
summary(fit.null[[1]])

# delete original data fitted model, get the list only have imputed model
fit.null[[6]] <- NULL
fit.community[[6]] <- NULL
fit.individual[[6]] <- NULL
fit.all[[6]] <- NULL
 
pool.null <- testEstimates(fit.null,extra.pars = TRUE)
pool.individual<- testEstimates(fit.individual,extra.pars = TRUE)
pool.community<- testEstimates(fit.community,extra.pars = TRUE)
pool.all <- testEstimates(fit.all,extra.pars = TRUE)

# function extra pool result
pool.re.function <- function(pooled){
  fixed <- exp(pooled$estimates)
  random.eff <- sqrt(pooled$extra.pars)
  fixed <- cbind(rownames(fixed),fixed)
  random.eff <- cbind(rownames(random.eff),random.eff)
  re <- rbind(fixed[,1:2],random.eff[,1:2])
  rownames(re) <- NULL
  re[,2] <- round(as.numeric(re[,2]),3)
  re <- as.data.frame(re)
  colnames(re) <- c("term", "Estimate")
  return(re)
}


null.pool.re <-pool.re.function(pool.null)
indivi.pool.re <-pool.re.function(pool.individual)
commu.pool.re <-pool.re.function(pool.community)
all.pool.re <-pool.re.function(pool.all)


null.origi.re <- tidy(null.model,exponentiate=TRUE,effects = c("fixed", "ran_pars")) %>% 
  select(-effect,-statistic,-p.value,-std.error)
indivi.origi.re <- tidy(individual.model,exponentiate=TRUE,effects = c("fixed", "ran_pars")) %>% 
  select(-effect,-statistic,-p.value,-std.error)
commu.origi.re <- tidy(community.model,exponentiate=TRUE,effects = c("fixed", "ran_pars")) %>% 
  select(-effect,-statistic,-p.value,-std.error)
all.origi.re <- tidy(all.model,exponentiate=TRUE,effects = c("fixed", "ran_pars")) %>% 
  select(-effect,-statistic,-p.value,-std.error)

null.re <- cbind(null.pool.re[,1],null.origi.re[,3],null.pool.re[,2]) 
colnames(null.re) <- c("term","original", "pooled imputed")
indivi.re <- cbind(indivi.pool.re[,1],indivi.origi.re[,3],indivi.pool.re[,2])
colnames(indivi.re) <- c("term","original", "pooled imputed")
commu.re <- cbind(commu.pool.re[,1],commu.origi.re[,3],commu.pool.re[,2])
colnames(commu.re) <- c("term","original", "pooled imputed")
all.re <- cbind(all.pool.re[,1],all.origi.re[,3],all.pool.re[,2])
colnames(all.re) <- c("term","original", "pooled imputed")

xtable.compare.null <- null.re %>% kable(format = "latex",booktabs = TRUE,escape = F, align = "c", digits = 3) %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))%>%
  kable_material(c("striped", "hover", "condensed"))
xtable.compare.indivi <-  indivi.re %>% kable(format = "latex",booktabs = TRUE,escape = F, align = "c", digits = 4) %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))%>%
  kable_material(c("striped", "hover", "condensed"))
xtable.compare.commu <- commu.re %>% kable(format = "latex",booktabs = TRUE,escape = F, align = "c", digits = 4) %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))%>%
  kable_material(c("striped", "hover", "condensed"))
xtable.compare.all <- all.re %>% kable(format = "latex",booktabs = TRUE,escape = F, align = "c", digits = 3) %>%
  kable_styling(full_width = FALSE, latex_options = c("hold_position"))%>%
  kable_material(c("striped", "hover", "condensed"))


kable(null.re, format = "latex",booktabs = TRUE,escape = F, align = "c", digits = 3) %>% 
  kable_styling(full_width = FALSE, latex_options = c("hold_position")) %>%
  kable_material(c("striped", "hover", "condensed"))


xtable.compare.all
xtable.compare.commu
xtable.compare.indivi
xtable.compare.null
xtable.gl
xtidy.all
xtidy.community
xtidy.individual
xtidy.null

coef(fit.individual[[1]])

library(lme4)
 predict.test <- predict(fit.null[[1]], imputed1, type = "response")
 roc(testmodel ~ predict.test, plot = TRUE, print.auc = TRUE)
 
library(see)
performance_roc(predict.test,imputed1$anemia)
 
library("ggplot2") ## Graphing
library("scales") ## Graphing
library("GGally") ## Extension to ggplot
library("lme4") ## for Hierarchical Models
library("sjPlot") ## Lovely Presentation of Model Output
library("rcompanion") ## Pairwise nominal test
library("ROCR") ## ROC Curves
library("pROC") ## ROC Curves
library("MuMIn") ## MuMIn::r.squaredGLMM()

rocplot <- function(pred, truth, ...) {
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
  area <- auc(truth, pred)
  area <- format(round(area, 4), nsmall = 4)
  text(x=0.7, y=0.1, labels = paste("AUC =", area))
  # the reference x=y line
  segments(x0=0, y0=0, x1=1, y1=1, col="gray", lty=2)
}

pkgs_CRAN <- c("lme4","MCMCglmm","blme",
               "pbkrtest","coda","aods3","bbmle","ggplot2",
               "reshape2","plyr","numDeriv","Hmisc",
               "plotMCMC","gridExtra","R2admb",
               "broom.mixed","dotwhisker")
install.packages(pkgs_CRAN)

library("lme4")
library("glmmADMB")      ## (not on CRAN)
library("glmmTMB")
library("MCMCglmm")
library("blme")
library("MASS")          ## for glmmPQL (base R)
library("nlme")          ## for intervals(), tundra example (base R)
## auxiliary
library("ggplot2")    

library(lme4)
library(dotwhisker)
library("gridExtra")     ## for grid.arrange()
library("broom.mixed")
dotplot(ranef(fit.all,condVar=TRUE))
library(lattice)
library(latticeExtra)
install.packages("glmmADMB",type="source",repos=rr)
library("devtools")

library("numDeriv")
par(mfrow = c(1,3))
rr <- ranef(fit.all[[6]])
rancompon <- ranef(fit.all[[6]],condVar=TRUE)
names(rancompon) <- c("Community intercept", "Proverty rate", "Literacy rate")
dotplot(rancompon,main = FALSE, layout = c(1,1))
ranef(fit.all[[1]],condVar=TRUE)

library(sjPlot)
library(sjmisc)
library(lattice)
library(lme4)
ranef(fit.all[[1]],condVar=TRUE)
dotplot(fixef(fit.all[[6]],condVar=TRUE),layout = c(1,1),aspect = 0.5)

library(ggplot2)
performance_roc(fit.null[[6]])
plot(fit.null[[6]])


confint.merMod(all.model)
plot(all.model,v024~resid(.,type="pearson"))
