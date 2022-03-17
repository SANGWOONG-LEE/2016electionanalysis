#### Preamble ####
# Purpose: Prepare the 2021 US GSS data
# Author: SangWoong Lee, Young Suk
# Data: 17 March 2022
# Contact: sangwoong.lee@mail.utoronto.ca, young.suk@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the GSS data and saved it to inputs/data


#### Workspace setup ####
# Use R Projects, not setwd().
library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data <- haven::read_dta("gss_analysis/gss2021.dta"
                     )
# Just keep some variables that may be of interest (change 
# this depending on your interests)
names(raw_data)

reduced_data <- 
  raw_data %>% 
  select(sexnow1, 
         degree,
         born,
         degree,
         income16,
         raceacs1,
         raceacs2,
         raceacs3,
         raceacs4,
         raceacs5,
         raceacs6,
         raceacs7,
         raceacs8,
         raceacs9,
         raceacs10,
         raceacs15,
         raceacs16,
         nateduc,
         natrace,
         natarms,
         natfare,
         natfarey,
         nataidy,
         natsoc,
         vote16) %>% 
  rename(gender = sexnow1,
         born_US = born,
         Family_income = income16,
         nation_education = nateduc,
         nation_arms = natarms,
         nation_welfare = natfare,
         Assistance_to_the_poor = natfarey,
         Assistance_to_other_countries = nataidy,
         social_security = natsoc)

rm(raw_data)
# Recode to rename for gender according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was Do you describe yourself as male, female, or transgender?
reduced_data <- 
  reduced_data %>% 
  mutate(gender = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female",
    gender == 3 ~ "Transgender",
    gender == 4 ~ "Other",
  ))
# Recode to rename for born_US according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was were you born in this country, US?
reduced_data <- 
  reduced_data %>% 
  mutate(born_US = case_when(
    born_US == 1 ~ "Yes",
    born_US == 2 ~ "No",
  ))
# Recode to rename for degree according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was the respondent's degree.
reduced_data <- 
  reduced_data %>% 
  mutate(degree = case_when(
    degree == 0 ~ "Less than high school",
    degree == 1 ~ "High School",
    degree == 2 ~ "Associate College",
    degree == 3 ~ "Bachelors",
    degree == 4 ~ "Graduate"
  ))
# Recode to rename for Family_income according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was in which of these groups did your total family income from all souces, fall last year?
# Before taxes.For simplicity, I divided groups into 
# 1) under $10,000
# 2) between $10,000 ~ $19,999
# 3) between $20,000 ~ $39,999
# 4) between $40,000 ~ $59,999
# 5) between $60,000 ~ $89,999
# 6) between $90,000 ~ $129,999
# 7) between $130,000 ~ $169,999
# 8) $170,000 or over
# 9) Refused 
reduced_data <- 
  reduced_data %>% 
  mutate(Family_income = case_when(
    Family_income == 1 ~ "Under $10,000",
    Family_income == 2 ~ "Under $10,000",
    Family_income == 3 ~ "Under $10,000",
    Family_income == 4 ~ "Under $10,000",
    Family_income == 5 ~ "Under $10,000",
    Family_income == 6 ~ "Under $10,000",
    Family_income == 7 ~ "Under $10,000",
    Family_income == 8 ~ "Under $10,000",
    Family_income == 9 ~ "$10,000 ~ $19,999",
    Family_income == 10 ~ "$10,000 ~ $19,999",
    Family_income == 11 ~ "$10,000 ~ $19,999",
    Family_income == 12 ~ "$10,000 ~ $19,999",
    Family_income == 13 ~ "$20,000 ~ $39,999",
    Family_income == 14 ~ "$20,000 ~ $39,999",
    Family_income == 15 ~ "$20,000 ~ $39,999",
    Family_income == 16 ~ "$20,000 ~ $39,999",
    Family_income == 17 ~ "$20,000 ~ $39,999",
    Family_income == 18 ~ "$40,000 ~ $59,999",
    Family_income == 19 ~ "$40,000 ~ $59,999",
    Family_income == 20 ~ "$60,000 ~ $89,999",
    Family_income == 21 ~ "$60,000 ~ $89,999",
    Family_income == 22 ~ "$90,000 ~ $129,999",
    Family_income == 23 ~ "$90,000 ~ $129,999",
    Family_income == 24 ~ "$130,000 ~ $169,999",
    Family_income == 25 ~ "$130,000 ~ $169,999",
    Family_income == 26 ~ "$170,000 or over",
    Family_income == 27 ~ "Refused"
  ))

#### What's next? ####



         