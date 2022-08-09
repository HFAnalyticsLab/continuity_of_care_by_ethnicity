


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

demogdata <- readRDS(str_c(outputs_path, 'all_data.rds'))  ## read in previously derived demographic vars
demogdata <- subset(demogdata, select=c('patid', "ethnic16label"))

COC_data <- readRDS(str_c(outputs_path, 'COC_data.rds'))
COC_data <- subset(COC_data, select=-c(ethnic16label))
ethnonmiss_pats <- readRDS(str_c(outputs_path, 'ethnonmiss_pats.rds'))
LTC_data <- subset(ethnonmiss_pats, select=c('patid', 'total', 'anyMH', 'PHMHLTC', 'complexMM', 'MM')) 
COC_data <- merge(COC_data, LTC_data, by="patid", all.x=TRUE)
COC_data <- merge(COC_data, demogdata, by="patid", all.x=TRUE)
COC_data <- COC_data %>%
  mutate(ethnic16label=na_if(ethnic16label, "No data")) %>%
  mutate(ethnic16label=na_if(ethnic16label, "Insufficient information")) %>%
  mutate(gen_ethnicity=na_if(gen_ethnicity, "Unknown")) %>%
  mutate(exclmiss=ifelse(is.na(age_baseline) | is.na(gender) | is.na(highdeprivIMD) | is.na(ethnic16label), 1, 0))


#############################################

## remove patients with missing data
nonmiss_pats<- COC_data %>% filter(exclmiss==0)
## Characteristics of analytical sample with complete data
vars <- c("age_baselineg", "gender", "highdeprivIMD", "ethnic16label", "PHMHLTC", "MM", "UPC", "BiceBox")
catvars <- c("age_baselineg", "gender", "highdeprivIMD", "ethnic16label")
COCTable1 <- CreateTableOne(vars=vars, data=nonmiss_pats, factorVars=catvars)
COCTable1
outputTable <- print(COCTable1, quote=FALSE, noSpaces=TRUE)
write.csv(outputTable, file=(str_c(outputs_path, 'COC_suppl_detailed_whiteethnicity.csv')))


nonmiss_pats <- nonmiss_pats %>%
  mutate(IMDquint=factor(imd2015_5)) %>% 
  mutate(followup=as.numeric(followup)) %>%
  mutate(fupc=(followup-365)/365) %>%  ## centre followup time at 365 days and convert to years
  mutate(ethnic16label=factor(ethnic16label, levels=c( "British", "Irish", "Other white", "Bangladeshi", "Pakistani", "Indian", "Other Asian" , "African", "Caribbean", "Other Black", "Chinese", "White & Asian", "White & Black African", "White & Black Caribbean", "Other mixed", "Other")))  ## reorder categories for models
summary(nonmiss_pats$followup)

regrUPC1 <- lmer(UPC ~ gender + age_baselineg + IMDquint + ethnic16label + total + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrUPC1)$coefficients
write.csv(summary(regrUPC1)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel2a_detailed_white_ethnic.csv')))


## Bice-Boxerman
regrBiceBox1 <- lmer(BiceBox ~ gender + age_baselineg + IMDquint + ethnic16label + total + fupc + totcons + (1|pracid), data=nonmiss_pats)
summary(regrBiceBox1)$coefficients
write.csv(summary(regrBiceBox1)$coefficients, file=(str_c(outputs_path, 'COCBiceBoxmodel2a_detailed_white_ethnic.csv')))
