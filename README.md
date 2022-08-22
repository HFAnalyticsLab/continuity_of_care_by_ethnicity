# Ethnic inequalities in continuity of care

Project Status: Under review

Project Description
Relational continuity of care in primary care is about having an ongoing therapeutic relationship between the patient and practitioner. Studies have linked it to a range of positive outcomes including lower mortality and higher patient satisfaction. Evidence from surveys of patients who use General Practice points to lower relational continuity of care for some ethnic minority groups than for patients of white ethnicity. We set out to describe continuity of care from primary care records.

Data Sources
We are using data from the Clinical Practice Research Datalink (CPRD), protocol number 21_000333. This includes linked HES, ONS death registration and area deprivation (IMD) data for the subset of patients for whom this is available.

Data used for this analysis were anonymised in line with the ICO's Anonymisation Code of Practice. The data were accessed in The Health Foundation's Data Analytics Platform, which is a secure data analysis facility (accredited for the ISO27001 information security standard, and recognised for the NHS Digital Data Security and Protection Toolkit). No information that could directly identify a patient or other individual was used. Variables labelled 'patid' do not refer to NHS IDs or other identifiable patient data.

How does it work?
As the data used for this analysis is not publically available, the code cannot be used to replicate the analysis on this dataset. However, with modifications the code will be able to be used on other patient-level CPRD extracts.

Requirements
These scripts were written in R version 3.6.2 and RStudio version 1.2.5033.

Getting started
The 'R' folder contains:
01_Process_raw_data_COC.R - data derivations capturing continuity for up to 4 years of followup
01_Process_raw_data_COC1yr.R - data derivations capturing continuity for 1 year of follow
02_Derive_patientvars_COC.R - merge in patient characteristics
03_Descriptive_models_COC.R - Creates tables and regression models for up to 4 years of followup
03_Descriptive_models_COC1yr.R - Creates tables and regression models for 1 year of followup
03_Descriptive_models_COC_white_ethnicity.R - Creates tables and regression models with the white ethnic group disaggregated


License
This project is licensed under the MIT License.

Code Authors - please feel free to get in touch
* **Mai Stafford** - on [Twitter](https://twitter.com/stafford_xm) or [Website](https://www.health.org.uk/about-the-health-foundation/our-people/data-analytics-team/mai-stafford)
* **Jay Hughes** - on [Website](https://health.org.uk/about-the-health-foundation/our-people/data-analytics-team/jay-hughes)

