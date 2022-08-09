

library(crayon)
library(devtools)
library(plyr)
library(dplyr)
library(tidyverse)
library(haven)
library(arrow)
library(data.table)
library(tableone)
library(nnet)
library(lubridate)
library(ggplot2)
library(lme4)
library(lmtest)

COC_data <- s3read_using(readRDS
                            , object= '/Output/COC_data.rds' # File to open
                            , bucket = buck) # Bucket name defined above
ethnonmiss_pats <- s3read_using(readRDS # Which function are we using to read
                                , object= '/Output/ethnonmiss_pats.rds' # File to open
                                , bucket = buck) # Bucket name defined above

COC_data <- COC_data %>%
  mutate(exclmiss=ifelse(is.na(age_baseline) | is.na(gender) | is.na(highdeprivIMD) | is.na(eth11), 1, 0))
LTC_data <- subset(ethnonmiss_pats, select=c('patid', 'total', 'anyMH', 'PHMHLTC', 'complexMM', 'MM')) 
COC_data <- merge(COC_data, LTC_data, by="patid", all.x=TRUE)




#############################################
catvars <- c("age_baselineg", "gender", "highdeprivIMD", "eth11")
catTableStrat <- CreateCatTable(vars=catvars, strata=c("exclmiss"), data=COC_data)
catTableStrat
summary(catTableStrat)
outputcatTableStrat <- print(catTableStrat, quote=FALSE, noSpaces=TRUE)
write.csv(outputcatTableStrat, file=(str_c(outputs_path, 'coc_supp1.csv')))



## remove patients with missing data
nonmiss_pats<- COC_data %>% filter(exclmiss==0)
## Characteristics of analytical sample with complete data
vars <- c("age_baselineg", "gender", "highdeprivIMD", "eth11", "PHMHLTC", "MM", "UPC", "BiceBox")
catvars <- c("age_baselineg", "gender", "highdeprivIMD", "eth11")
COCTable1 <- CreateTableOne(vars=vars, data=nonmiss_pats, factorVars=catvars)
COCTable1
outputTable <- print(COCTable1, quote=FALSE, noSpaces=TRUE)
write.csv(outputTable, file=(str_c(outputs_path, 'COCTable1.csv')))

catvars <- c("PHMHLTC")
catTableStrat <- CreateCatTable(vars=catvars, strata=c("eth11"), data=nonmiss_pats)
catTableStrat

## Covariates by ethnicity
vars <- c("age_baselineg", "gender", "highdeprivIMD", "MM", "UPC", "BiceBox")
catvars <- c("age_baselineg", "gender", "highdeprivIMD", "MM")
TableStrat <- CreateTableOne(vars=vars, strata="eth11", data=nonmiss_pats, factorVars = catvars)
outputTableStrat <- print(TableStrat, quote=FALSE, noSpaces=TRUE)
write.csv(outputTableStrat, file=(str_c(outputs_path, 'COCsupplTable1_byeth.csv')))

vars <- c("PHMHLTC")
catvars <- c("PHMHLTC")
TableStrat <- CreateTableOne(vars=vars, strata="eth11", data=nonmiss_pats, factorVars = catvars)
outputTableStrat <- print(TableStrat, quote=FALSE, noSpaces=TRUE)
write.csv(outputTableStrat, file=(str_c(outputs_path, 'COCsupplTable1PHMHLTC_byeth.csv')))

## Mean UPC and BiceBox by demographics
contvars <- c("UPC", "BiceBox")
TableStrat <- CreateTableOne(vars=contvars, strata="gender", data=nonmiss_pats)
outputTableStrat <- print(TableStrat, quote=FALSE, noSpaces=TRUE)
write.csv(outputTableStrat, file=(str_c(outputs_path, 'COCTable2gender.csv')))
TableStrat <- CreateTableOne(vars=contvars, strata="age_baselineg", data=nonmiss_pats)
outputTableStrat <- print(TableStrat, quote=FALSE, noSpaces=TRUE)
write.csv(outputTableStrat, file=(str_c(outputs_path, 'COCTable2age.csv')))
TableStrat <- CreateTableOne(vars=contvars, strata="imd2015_5", data=nonmiss_pats)
outputTableStrat <- print(TableStrat, quote=FALSE, noSpaces=TRUE)
write.csv(outputTableStrat, file=(str_c(outputs_path, 'COCTable2IMD.csv')))
TableStrat <- CreateTableOne(vars=contvars, strata="eth11", data=nonmiss_pats)
outputTableStrat <- print(TableStrat, quote=FALSE, noSpaces=TRUE)
write.csv(outputTableStrat, file=(str_c(outputs_path, 'COCTable2ethnic.csv')))
TableStrat <- CreateTableOne(vars=contvars, strata="MM", data=nonmiss_pats)
outputTableStrat <- print(TableStrat, quote=FALSE, noSpaces=TRUE)
write.csv(outputTableStrat, file=(str_c(outputs_path, 'COCTable2MM.csv')))
TableStrat <- CreateTableOne(vars=contvars, strata="PHMHLTC", data=nonmiss_pats)
outputTableStrat <- print(TableStrat, quote=FALSE, noSpaces=TRUE)
write.csv(outputTableStrat, file=(str_c(outputs_path, 'COCTable2PHMHLTC.csv')))


nonmiss_pats <- nonmiss_pats %>%
  mutate(IMDquint=factor(imd2015_5)) %>% 
  mutate(followup=as.numeric(followup)) %>%
  mutate(fupc=(followup-365)/365) %>%  ## centre followup time at 365 days and convert to years
  mutate(eth11=factor(eth11, levels=c( "White","Bangladeshi", "Pakistani", "Indian", "Any other Asian background" , "African", "Caribbean", "Any other Black background", "Chinese", "Mixed", "All other ethnic groups")))  ## reorder categories for models
summary(nonmiss_pats$followup)

ggplot(nonmiss_pats, aes(x=UPC)) +
  geom_histogram(binwidth=0.05)  ## small peak at 1 so repeat sensitivity analysis with UPC as categorical


regrUPC1 <- lmer(UPC ~ fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrUPC1)$coefficients
write.csv(summary(regrUPC1)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel0.csv')))
regrUPC1 <- lmer(UPC ~ gender + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrUPC1)$coefficients
write.csv(summary(regrUPC1)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel1a.csv')))
regrUPC1 <- lmer(UPC ~ age_baselineg + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrUPC1)$coefficients
write.csv(summary(regrUPC1)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel1b.csv')))
regrUPC1 <- lmer(UPC ~ IMDquint + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrUPC1)$coefficients
write.csv(summary(regrUPC1)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel1c.csv')))
regrUPC1 <- lmer(UPC ~ eth11 + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrUPC1)$coefficients
write.csv(summary(regrUPC1)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel1d.csv')))
regrUPC1 <- lmer(UPC ~ total + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrUPC1)$coefficients
write.csv(summary(regrUPC1)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel1e.csv')))
regrUPC1 <- lmer(UPC ~ PHMHLTC + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrUPC1)$coefficients
write.csv(summary(regrUPC1)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel1f.csv')))
regrUPC1 <- lmer(UPC ~ gender + age_baselineg + IMDquint + eth11 + total + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrUPC1)$coefficients
(A <- logLik(regrUPC1))
write.csv(summary(regrUPC1)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel2a.csv')))
regrUPC1 <- lmer(UPC ~ gender + age_baselineg + IMDquint + eth11 + PHMHLTC + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrUPC1)$coefficients
write.csv(summary(regrUPC1)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel2b.csv')))
# repeat above model with complete  followup only
threeyrfup <- nonmiss_pats %>% filter(fupc==3)
regrUPC1 <- lm(UPC ~ gender + age_baselineg + IMDquint + eth11 + PHMHLTC + totcons , data=nonmiss_pats) # can't install lme4 package
summary(regrUPC1)$coefficients


## Interaction of ethnicity and total number of LTCs
regrUPC2 <- lmer(UPC ~ gender + age_baselineg + IMDquint+ eth11*total + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrUPC2)$coefficients
(B <- logLik(regrUPC2))
(teststat <- -2*(as.numeric(B)-as.numeric(A)))
(p.val <-pchisq(teststat, df=10, lower.tail=FALSE))
write.csv(summary(regrUPC2)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel3.csv')))
nonmiss_patsMMphys <- filter(nonmiss_pats, PHMHLTC=="MM phys")
regrUPC2 <- lmer(UPC ~ gender + age_baselineg + IMDquint+ eth11 + fupc + totcons + (1|pracid), data=nonmiss_patsMMphys)
summary(regrUPC2)$coefficients
write.csv(summary(regrUPC2)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel3MMphys.csv')))
nonmiss_patsMMphysment <- filter(nonmiss_pats, PHMHLTC=="MM phys ment")
regrUPC2 <- lmer(UPC ~ gender + age_baselineg + IMDquint+ eth11 + fupc + totcons + (1|pracid), data=nonmiss_patsMMphysment)
summary(regrUPC2)$coefficients
write.csv(summary(regrUPC2)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel3MMphysment.csv')))
nonmiss_patsnotMMC <- filter(nonmiss_pats, total<2)
regrUPC2 <- lmer(UPC ~ gender + age_baselineg + IMDquint+ eth11 + fupc + totcons + (1|pracid), data=nonmiss_patsnotMM)
summary(regrUPC2)$coefficients
write.csv(summary(regrUPC2)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel3notMM.csv')))
nonmiss_patsMM <- filter(nonmiss_pats, total>1)
regrUPC2 <- lmer(UPC ~ gender + age_baselineg + IMDquint+ eth11 + fupc + totcons + (1|pracid), data=nonmiss_patsMM)
summary(regrUPC2)$coefficients
write.csv(summary(regrUPC2)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel3MM.csv')))


## sensitivty analysis with UPC as categorical
summary(nonmiss_pats$UPC)
UPCtert=quantile(nonmiss_pats$UPC, c(0:3/3))
UPCtert
nonmiss_pats <- nonmiss_pats %>%
  mutate(UPChigh=ifelse(UPC>0.57778, 1, 0)) ## UPChigh is top quartile of UPC index
regrUPC1 <- glm(UPChigh ~  age_baselineg + IMDquint + eth11 + total + fupc, data=nonmiss_pats, family=binomial("logit"))
summary(regrUPC1)$coefficients
exp(cbind(coef(regrUPC1), confint(regrUPC1)))


## Bice-Boxerman
ggplot(nonmiss_pats, aes(x=BiceBox)) +
  geom_histogram(binwidth=0.05)

regrBiceBox1 <- lmer(BiceBox ~ fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrBiceBox1)$coefficients
write.csv(summary(regrBiceBox1)$coefficients, file=(str_c(outputs_path, 'COCBiceBoxmodel0.csv')))
regrBiceBox1 <- lmer(BiceBox ~ gender+ fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrBiceBox1)$coefficients
write.csv(summary(regrBiceBox1)$coefficients, file=(str_c(outputs_path, 'COCBiceBoxmodel1a.csv')))
regrBiceBox1 <- lmer(BiceBox ~ age_baselineg + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrBiceBox1)$coefficients
write.csv(summary(regrBiceBox1)$coefficients, file=(str_c(outputs_path, 'COCBiceBoxmodel1b.csv')))
regrBiceBox1 <- lmer(BiceBox ~ IMDquint + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrBiceBox1)$coefficients
write.csv(summary(regrBiceBox1)$coefficients, file=(str_c(outputs_path, 'COCBiceBoxmodel1c.csv')))
regrBiceBox1 <- lmer(BiceBox ~ eth11 + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrBiceBox1)$coefficients
write.csv(summary(regrBiceBox1)$coefficients, file=(str_c(outputs_path, 'COCBiceBoxmodel1d.csv')))
regrBiceBox1 <- lmer(BiceBox ~ total + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrBiceBox1)$coefficients
write.csv(summary(regrBiceBox1)$coefficients, file=(str_c(outputs_path, 'COCBiceBoxmodel1e.csv')))
regrBiceBox1 <- lmer(BiceBox ~ PHMHLTC + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrBiceBox1)$coefficients
write.csv(summary(regrBiceBox1)$coefficients, file=(str_c(outputs_path, 'COCBiceBoxmodel1f.csv')))
regrBiceBox1 <- lmer(BiceBox ~ gender + age_baselineg + IMDquint + eth11 + total + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrBiceBox1)$coefficients
(A <- logLik(regrBiceBox1))
write.csv(summary(regrBiceBox1)$coefficients, file=(str_c(outputs_path, 'COCBiceBoxmodel2a.csv')))
regrBiceBox1 <- lmer(BiceBox ~ gender + age_baselineg + IMDquint + eth11 + PHMHLTC + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrBiceBox1)$coefficients
write.csv(summary(regrBiceBox1)$coefficients, file=(str_c(outputs_path, 'COCBiceBoxmodel2b.csv')))

## Interaction of ethnicity and total number of LTCs
regrBiceBox2 <- lmer(BiceBox ~ gender + age_baselineg + IMDquint+ eth11*total + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrBiceBox2)$coefficients
(B <- logLik(regrBiceBox2))
(teststat <- -2*(as.numeric(B)-as.numeric(A)))
(p.val <-pchisq(teststat, df=10, lower.tail=FALSE))
write.csv(summary(regrBiceBox2)$coefficients, file=(str_c(outputs_path, 'COCBiceBoxmodel3.csv')))

## sensitivty analysis with BiceBox as categorical
summary(nonmiss_pats$BiceBox)
BBtert=quantile(nonmiss_pats$BiceBox, c(0:3/3))
BBtert

nonmiss_pats <- nonmiss_pats %>%
  mutate(BBhigh=ifelse(UPC>0.333333, 1, 0))
regrBiceBox1 <- glm(BBhigh ~  age_baselineg + IMDquint + eth11 + total + fupc + totcons, data=nonmiss_pats, family=binomial("logit"))
summary(regrBiceBox1)$coefficients
exp(cbind(coef(regrBiceBox1), confint(regrBiceBox1)))

