


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


COC_data <- readRDS(str_c(outputs_path, 'COC_data1yr.rds'))
COC_data <- COC_data %>%
  mutate(exclmiss=ifelse(is.na(age_baseline) | is.na(gender) | is.na(highdeprivIMD) | is.na(eth11), 1, 0))
ethnonmiss_pats <- readRDS(str_c(outputs_path, 'ethnonmiss_pats.rds'))
LTC_data <- subset(ethnonmiss_pats, select=c('patid', 'total', 'anyMH', 'PHMHLTC', 'complexMM', 'MM')) 
COC_data <- merge(COC_data, LTC_data, by="patid", all.x=TRUE)




#############################################

## remove patients with missing data
nonmiss_pats<- COC_data %>% filter(exclmiss==0)
## Characteristics of analytical sample with complete data
vars <- c("age_baselineg", "gender", "highdeprivIMD", "eth11", "PHMHLTC", "MM", "UPC", "BiceBox")
catvars <- c("age_baselineg", "gender", "highdeprivIMD", "eth11")
COCTable1 <- CreateTableOne(vars=vars, data=nonmiss_pats, factorVars=catvars)
COCTable1
outputTable <- print(COCTable1, quote=FALSE, noSpaces=TRUE)
write.csv(outputTable, file=(str_c(outputs_path, 'COCTable11yr.csv')))

catvars <- c("PHMHLTC")
catTableStrat <- CreateCatTable(vars=catvars, strata=c("eth11"), data=nonmiss_pats)
catTableStrat


nonmiss_pats <- nonmiss_pats %>%
  mutate(IMDquint=factor(imd2015_5)) %>% 
  mutate(eth11=factor(eth11, levels=c( "White","Bangladeshi", "Pakistani", "Indian", "Any other Asian background" , "African", "Caribbean", "Any other Black background", "Chinese", "Mixed", "All other ethnic groups")))  ## reorder categories for models
summary(nonmiss_pats$followup)

regrUPC1 <- lmer(UPC ~ gender + age_baselineg + IMDquint + eth11 + total + totcons + (1|pracid), data=nonmiss_pats)
summary(regrUPC1)$coefficients
write.csv(summary(regrUPC1)$coefficients, file=(str_c(outputs_path, 'COCUPCmodel2a_1yr.csv')))




## Bice-Boxerman
regrBiceBox1 <- lmer(BiceBox ~ gender + age_baselineg + IMDquint + eth11 + total + totcons + (1|pracid), data=nonmiss_pats)
summary(regrBiceBox1)$coefficients
write.csv(summary(regrBiceBox1)$coefficients, file=(str_c(outputs_path, 'COCBiceBoxmodel2a_1yr.csv')))
