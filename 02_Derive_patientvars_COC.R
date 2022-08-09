

countcons <- readRDS(str_c(outputs_path, 'countcons.rds'))
followuptime <- countcons %>%
  group_by(patid) %>%
  filter(row_number()==1) 
followuptime <- subset(followuptime, select=c('patid', 'followup'))


## derive UPC Usual Provider of Care Index
## Calculated as proportion of patients total contacts that were with their most regularly seen GP

COC_data <- filter(countcons, !is.na(staffid))

totcons <- COC_data %>% 
  group_by(patid) %>%
  summarise(totcons=n())  ## count total number of consultations in follow-up period of 4 years

eachGPtotcons <- COC_data %>% 
  group_by(patid, staffid) %>%
  summarise(eachGPtotcons=n())  ## count number of consultations with each GP
eachGPtotcons <- eachGPtotcons %>%
  mutate(maxeachGPtotcons=max(eachGPtotcons), by="patid")

GPtotcons <- merge(eachGPtotcons, totcons, by="patid")
GPtotcons <- GPtotcons %>%
  mutate(UPC=maxeachGPtotcons/totcons)  ## calculate UPC index
GPtotcons <- GPtotcons %>%
  group_by(patid) %>%
  mutate(BBnuma=sum(eachGPtotcons^2))
GPtotcons <- GPtotcons %>%
  mutate(BBnum=BBnuma-totcons) %>%
  mutate(BBdenom=totcons*(totcons-1)) %>%
  mutate(BiceBox=BBnum/BBdenom)

patGPtotcons <- GPtotcons %>%
  group_by(patid) %>%
  filter(row_number()==1 & totcons>2)  ## Limit analysis to patients with at least 3 consultations (required for Bice-Boxerman index)

cor(patGPtotcons$UPC, patGPtotcons$BiceBox)

demogdata <- readRDS(str_c(outputs_path, 'all_data.rds'))  ## read in previously derived demographic vars
demogdata <- subset(demogdata, select=c('patid', 'pracid', 'yob', 'gender', 'imd2015_5', "ddate", "ethnic16label", "gen_ethnicity", "region", "lcd", "regstartdate", "regenddate", "cprd_ddate", "lcd"))

## data for analysis  
COC_data=merge(patGPtotcons, demogdata, by = 'patid', all.x = T) %>%
  merge(followuptime)

## Demographic variable derivations
COC_data <- COC_data %>% 
  mutate(highdeprivIMD=ifelse(imd2015_5==5, 1, 0)) %>%
  mutate(age_baseline=2016-yob) %>% ## derive age at baseline
  mutate(ethnic16label=na_if(ethnic16label, "No data")) %>%
  mutate(ethnic16label=na_if(ethnic16label, "Insufficient information")) %>%
  mutate(gen_ethnicity=na_if(gen_ethnicity, "Unknown"))


COC_data$ethnic16label[COC_data$ethnic16label=="British"] <- "White"
COC_data$ethnic16label[COC_data$ethnic16label=="Irish"] <- "White"
COC_data$ethnic16label[COC_data$ethnic16label=="Other white"] <- "White"
COC_data$ethnic16label[COC_data$ethnic16label=="Other"] <- "All other ethnic groups"
COC_data$ethnic16label[COC_data$ethnic16label=="Other Black"] <- "Any other Black background"
COC_data$ethnic16label[COC_data$ethnic16label=="Other Asian"] <- "Any other Asian background"
COC_data$ethnic16label[COC_data$ethnic16label=="White & Asian"] <- "Mixed"
COC_data$ethnic16label[COC_data$ethnic16label=="White & Black African"] <- "Mixed"
COC_data$ethnic16label[COC_data$ethnic16label=="White & Black Caribbean"] <- "Mixed"
COC_data$ethnic16label[COC_data$ethnic16label=="Other mixed"] <- "Mixed"

COC_data$gen_ethnicity[COC_data$gen_ethnicity=="Bl_Afric"] <- "African"
COC_data$gen_ethnicity[COC_data$gen_ethnicity=="Bl_Carib"] <- "Caribbean"
COC_data$gen_ethnicity[COC_data$gen_ethnicity=="Bl_Other"] <- "Any other Black background"
COC_data$gen_ethnicity[COC_data$gen_ethnicity=="Oth_Asian"] <- "Any other Asian background"
COC_data$gen_ethnicity[COC_data$gen_ethnicity=="Other"] <- "All other ethnic groups"

COC_data <- COC_data %>% 
  mutate(eth11=ifelse(!is.na(ethnic16label), ethnic16label, gen_ethnicity)) %>%
  mutate(eth11=factor(eth11, levels=c("Bangladeshi", "Pakistani", "Indian", "Any other Asian background" , "African", "Caribbean", "Any other Black background", "Chinese", "Mixed", "All other ethnic groups", "White")))  ## reorder categories for tables

COC_data$age_baselineg <- cut(COC_data$age_baseline, 
                               breaks=c(-Inf, 28, 38, 48, 58, 68, 78, Inf),
                               labels=c("18-29y", "30-39y", "40-49y", "50-59y", "60-69y", "70-79y", "80+y"))  
COC_data$gender <- factor(COC_data$gender, levels=c(1,2), labels=c("men", "women"))
COC_data$region <- factor(COC_data$region, labels=c("NE", "NW", "Yorks & Humber", "E Mids", "W Mids",
                                                      "East Engl","SW", "S Central", "London", "SE Coast")) 


saveRDS(COC_data, file=(str_c(outputs_path, 'COC_data.rds')))



## flow chart
allpatid <- readRDS(str_c(outputs_path, 'all_data.rds'))  ## read in previously derived demographic vars
allpatid1 <- subset(allpatid, select=c('patid', 'pracid', 'yob', 'gender', 'imd2015_5'))
allpatid1 <- allpatid1 %>%
  mutate(highdeprivIMD=ifelse(imd2015_5==5, 1, 0)) %>%
  mutate(age_baseline=2016-yob) ## derive age at baseline
  
COC_data1 <- subset(COC_data, select=c('patid', 'eth11', 'totcons'))
COC_flow=merge(COC_data1, allpatid1, by = 'patid', all.y = T) 

COC_flow <- COC_flow %>%
  mutate(totconsn=ifelse(is.na(totcons), 0, totcons)) %>%
  mutate(consthree=ifelse(totconsn>2,1,0)) %>%
  mutate(misscovar=ifelse(is.na(age_baseline) | is.na(gender) | is.na(imd2015_5), 1, 0)) %>%
  mutate(misscovareth=ifelse(is.na(age_baseline) | is.na(gender) | is.na(imd2015_5) | is.na(eth11), 1, 0)) 


COC_flow$age_baselineg <- cut(COC_flow$age_baseline, 
                              breaks=c(-Inf, 28, 38, 48, 58, 68, 78, Inf),
                              labels=c("18-29y", "30-39y", "40-49y", "50-59y", "60-69y", "70-79y", "80+y"))  
COC_flow$gender <- factor(COC_flow$gender, levels=c(1,2), labels=c("men", "women"))


catvars <- c("age_baselineg", "gender", "highdeprivIMD")
catTableStrat <- CreateCatTable(vars=catvars, strata=c("consthree"), data=COC_flow)
catTableStrat
COC_flow1=filter(COC_flow, consthree==1)
catvars <- c("age_baselineg", "gender", "highdeprivIMD")
catTableStrat <- CreateCatTable(vars=catvars, strata=c("misscovar"), data=COC_flow1)
catTableStrat
COC_flow2=filter(COC_flow1, misscovar==0)
catvars <- c("age_baselineg", "gender", "highdeprivIMD")
catTableStrat <- CreateCatTable(vars=catvars, strata=c("misscovareth"), data=COC_flow2)
catTableStrat




