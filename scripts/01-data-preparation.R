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
raw_data <- haven::read_dta("inputs/data/2021_stata/gss2021.dta")

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
         pres16) %>% 
  rename(gender = sexnow1,
         born_US = born,
         Family_income = income16,
         nation_education = nateduc,
         nation_arms = natarms,
         nation_race = natrace,
         nation_welfare = natfare,
         Assistance_to_the_poor = natfarey,
         Assistance_to_other_countries = nataidy,
         social_security = natsoc)

rm(raw_data)

# Recode to rename variables in gender according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was Do you describe yourself as male, female, or transgender?
reduced_data <- 
  reduced_data %>% 
  mutate(gender = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female",
    gender == 3 ~ "Transgender",
    gender == 4 ~ "Other",
  ))
# Recode to rename variables in born_US according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was were you born in this country, US?
reduced_data <- 
  reduced_data %>% 
  mutate(born_US = case_when(
    born_US == 1 ~ "Yes",
    born_US == 2 ~ "No",
  ))
# Recode to rename variables in degree according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
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
# Recode to rename variables in Family_income according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
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
# Recode to rename variables in nation_education according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was whether US is spending too much, too little, or about the right amount on 
# improving the nation's education system.
# One thing to notice here is that there exists 50.7% of respondents who answered "Not Applicable".
# Unfortunately this falls into "NA". 
reduced_data <- 
  reduced_data %>% 
  mutate(nation_education = case_when(
    nation_education == 1 ~ "Too little",
    nation_education == 2 ~ "About right",
    nation_education == 3 ~ "Too much",
  ))
# Recode to rename variables in nation_race according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was whether US is spending too much, too little, or about the right amount on 
# improving the conditions of Blacks
# Similar to nation education, this has 50.7% of respondents who answered "Not Applicable".
# Unfortunately this falls into "NA". 
reduced_data <- 
  reduced_data %>% 
  mutate(nation_race = case_when(
    nation_race == 1 ~ "Too little",
    nation_race == 2 ~ "About right",
    nation_race == 3 ~ "Too much",
  ))
# Recode to rename variables in nation_arms according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was whether US is spending too much, too little, or about the right amount on 
# The military, armaments, and defense
# Unfortunetly 50.7% of respondents answered "Not Applicable" which falls into "NA".
reduced_data <- 
  reduced_data %>% 
  mutate(nation_arms = case_when(
    nation_arms == 1 ~ "Too little",
    nation_arms == 2 ~ "About right",
    nation_arms == 3 ~ "Too much",
  ))

# First step of merging process of survey response related to race. Create a column race_sum which each row represents number of races of each person.
# If value at column race_sum is greater than 1, that person is of mixed race.
reduced_data$race_sum <- reduced_data %>%
  select(raceacs1, raceacs2, raceacs3, raceacs4, raceacs5, raceacs6, raceacs7, raceacs8, raceacs9, raceacs10, raceacs15, raceacs16) %>%
  rowSums(.)

# Create a column named "race" and assign values according to the columns from the survey.
reduced_data <- reduced_data %>%
  mutate(race = case_when(
    raceacs1 == 1 ~ "white",
    raceacs2 == 1 ~ "black",
    raceacs3 == 1 ~ "american_native",
    raceacs4 == 1 ~ "asian",
    raceacs5 == 1 ~ "asian",
    raceacs6 == 1 ~ "asian",
    raceacs7 == 1 ~ "asian",
    raceacs8 == 1 ~ "asian",
    raceacs9 == 1 ~ "asian",
    raceacs10 == 1 ~ "asian",
    raceacs15 == 1 ~ "other",
    raceacs16 == 1 ~ "hispanic"
    ))

# If a person is of mixed race, that person is assigned value "other"
reduced_data <- reduced_data %>%
  mutate(race = if_else(race_sum == 1, reduced_data$race, "other"))

# Remove columns that are no longer needed.
reduced_data <- reduced_data %>%
  select(-c(raceacs1, raceacs2, raceacs3, raceacs4, raceacs5, raceacs6, raceacs7, raceacs8, raceacs9, raceacs10, raceacs15, raceacs16, race_sum))

  

# Recode to rename variables in nation_welfare according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was whether US is spending too much, too little, or about the right amount on 
# Welfare
# Unfortunetly 50.7% of respondents answered "Not Applicable" which falls into "NA".
reduced_data <- 
  reduced_data %>% 
  mutate(nation_welfare = case_when(
    nation_welfare == 1 ~ "Too little",
    nation_welfare == 2 ~ "About right",
    nation_welfare == 3 ~ "Too much",
  ))
# Recode to rename variables in Assistance_to_the_poor according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was whether US is spending too much, too little, or about the right amount on 
# assistance to the poor
# Unfortunetly 49.3% of respondents answered "Not Applicable" which falls into "NA".

reduced_data <- 
  reduced_data %>% 
  mutate(Assistance_to_the_poor = case_when(
    Assistance_to_the_poor == 1 ~ "Too little",
    Assistance_to_the_poor == 2 ~ "About right",
    Assistance_to_the_poor == 3 ~ "Too much",
  ))
# Recode to rename variables in Assistance_to_other_countries according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was whether US is spending too much, too little, or about the right amount on 
# assistance to the other countries.
# Unfortunately 49.3% of respondents answered "Not Applicable" which falls into "NA".

reduced_data <- 
  reduced_data %>% 
  mutate(Assistance_to_other_countries = case_when(
    Assistance_to_other_countries == 1 ~ "Too little",
    Assistance_to_other_countries == 2 ~ "About right",
    Assistance_to_other_countries == 3 ~ "Too much",
  ))
# Recode to rename variables in social_security according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was whether US is spending too much, too little, or about the right amount on 
# social security
reduced_data <- 
  reduced_data %>% 
  mutate(social_security = case_when(
    social_security == 1 ~ "Too little",
    social_security == 2 ~ "About right",
    social_security == 3 ~ "Too much",
  ))
# Recode to rename variables in pres16 according to options from the Codebook 'GSS 2021 Codebook R1b.pdf'.
# The question was whether a respondent voted for Hillary Clinton or Donald Trump
reduced_data <- 
  reduced_data %>% 
  mutate(pres16 = case_when(
    pres16 == 1 ~ "Clinton",
    pres16 == 2 ~ "Trump",
    pres16 == 3 ~ "Other",
    pres16 == 4 ~ "Did not vote",
  ))

reduced_data <- reduced_data[!is.na(reduced_data$pres16), ]
  

#### Save ####
write_csv(reduced_data, "outputs/data/prepared_gss.csv")


         