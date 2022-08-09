

library(aurumpipeline)
library(data.table)
library(tidyverse)
library(here)
library(dplyr)
library(arrow)
library(naniar)
library(tableone)
library(bit64)
ls('package:aurumpipeline')


## Get all out paths
source('00_filepaths.R')
all_data <- file.path(data_path, list.files(data_path))

study_start <- as.Date("2016-01-01")
study_end <- as.Date("2016-12-31")



## Load in Staff roles
staff <- opendt(all_data[25])
jobcatlkup <- read.delim(
  'lookuptable_path/JobCat.txt')
jobs <- merge(staff,jobcatlkup, by = "jobcatid", all.x = TRUE)
jobs <- jobs %>%
  rename(staffroledescrip=Description)

catvars <- c("staffroledescrip")
catTableOverall <- CreateCatTable(vars=catvars, data=jobs)
catTableOverall


## load in consultations
consultations <- opendt((all_data[3]),
 cols_in = c('patid', 'consid', 'staffid', 'consdate', 'conssourceid', 'consmedcodeid'),
 date_in = 'consdate', start_date = study_start, end_date = study_end)  #filter by date

## Consultation types
conssourcelkup <- read.delim(
  'lookuptable_path/ConsSource.txt')
conssourcelkup <- conssourcelkup %>%
  replace_with_na(replace=list(Description ="Awaiting review")) %>%
  rename(consid=Id)

catvars <- c("Description")
catTableOverall <- CreateCatTable(vars=catvars, data=conssourcelkup)
catTableOverall

consultations_types <- merge(consultations, conssourcelkup, by="consid", all.x = TRUE)

## merge jobs and consultations
consstaff <- merge(consultations_types, jobs, by="staffid", all.x=TRUE)
consultations_GP <- filter(consstaff, staffroledescrip=="Associate Practitioner - General Practitioner" | staffroledescrip=="Assistant GP" |
                             staffroledescrip=="General Medical Practitioner" | staffroledescrip=="GP Registrar" | staffroledescrip=="Locum GP" |
                             staffroledescrip=="Salaried General Practitioner" | staffroledescrip=="Sessional GP") ## select only GP consultations

rm(consultations, consultations_types,conssourcelkup) # tidy up


## Identify consultation modes (We are only interested in F2F, telephone, online and other, not administrative)
## We can do this from the consultation file based on the consmedcodeid AND from the observation file based on the medcodeid

cons_mode <- fread('lookuptable_path/categorisation_cons_mode.txt')[ 
  , .(conssourceid, consmedcodeid = as.integer64(substring(consmedcodeid, 2)), type)] 

## read in observations
observations <- opendt((all_data[18]),
                       cols_in = c('patid', 'consid', 'obsid', 'obsdate', 'medcodeid'),
                       date_in = 'obsdate', start_date = study_start, end_date = study_end)  #filter by date

observations1 <- filter(observations, !is.na(consid))

obs_mode <- fread('lookuptable_path/categorisation_obs_mode.txt')[  #read in EMIS codes for consultation mode from observation file
  , .(medcodeid = as.integer64(substring(medcodeid, 2)), obs_type)]

obsmerged <- merge(observations1, obs_mode, by='medcodeid', all.x = TRUE) 

types <- c('default', 'f2f', 'visit', 'online', 'phone', 'other') #list the mode types/categories we are interested in

for (i in types){ #for each of the mode types...
  obsmerged[obs_type == i, paste(i) := 1] #create a column for the type with value = 1 for rows where it exists
}

obskeep <- filter(obsmerged, default==1 | f2f==1 | visit==1 | online==1 | phone==1 | other==1)


## merge the GP consultations with the consultation modes the keep those modes we are interested in
consmerged <- merge(consultations_GP, cons_mode,by = c('conssourceid', 'consmedcodeid'), all.x = TRUE)
consobsmerged <- merge(obskeep, consmerged, by =c('consid','patid'), all=TRUE)
consobsmerged <- filter(consobsmerged, !is.na(consdate))
consobskeep <- filter(consobsmerged, type=="default" | type=="f2f" | type=="online" | type=="other" | type=="phone" | type=="visit" |
                        default==1 | f2f==1 | visit==1 | online==1 | phone==1 | other==1)  
select(consobskeep, -c(patid.y))
consobskeep <- consobskeep %>%
  rename(patid=patid.x) 
  
rm(consmerged, obskeep,observations) # tidy up

saveRDS(consobskeep, file=(str_c(outputs_path, 'consobskeep1yr.rds')))


## Load patient data
pats <- opendt(all_data[19])

# At least 365 days continuous registration at any time in the period 1 Jan 2016 to latest date available, 

pats_dates <- pats %>% 
  mutate(emis_ddate = as.Date(emis_ddate, format= "%d/%m/%Y"),
         regstartdate = as.Date(regstartdate, format= "%d/%m/%Y"),
         regenddate = as.Date(regenddate, format= "%d/%m/%Y"),
         cprd_ddate = as.Date(cprd_ddate, format= "%d/%m/%Y")
  )

pats_incl <- pats_dates %>% 
  mutate(startdate = pmax(regstartdate, study_start, na.rm = TRUE),
         enddate = pmin(regenddate, cprd_ddate, study_end, na.rm=TRUE),
         followup = enddate-startdate
  ) 


pats_incl <- pats_incl %>% 
  filter(followup >= 365)
nrow(pats_incl)


## merge patient, consultation and observation files 
consobskeep1yr <- readRDS(str_c(outputs_path, 'consobskeep1yr.rds'))
patsconsGP <- merge(pats_incl, consobskeep1yr, by="patid",  all.x = TRUE) 
patsconsGP <- patsconsGP %>%
  rename(pracid=pracid.x) 
  


## I want to identify multiple consultations on the same day with the same staffid and retain just 1 of these
## Keep consultations that are on the same day but with a different staffid
countcons <- patsconsGP %>%
  group_by(patid, staffid) %>%
  mutate(dupcons=sum(duplicated(consdate)))

catvars <- c("dupcons")
catTableOverall <- CreateCatTable(vars=catvars, data=countcons)  
catTableOverall


saveRDS(countcons, file=(str_c(outputs_path, 'countcons1yr.rds')))

