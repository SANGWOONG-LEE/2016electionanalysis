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
raw_data <- haven::read_dta("gss_analysis/gss2021.dta")

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
         natsoc) %>% 
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
# Recode for sexnow1. Corresponding to options from the Codebook
#'GSS 2021 Codebook R1b.pdf', variables are modified as below: 
reduced_data <- 
  reduced_data %>% 
  mutate(gender == case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female",
    gender == 3 ~ "Transgender",
    gender == 4 ~ "Other",
  ))


race_data <- data.frame(white = sum(reduced_data$raceacs1, na.rm = T), black = sum(reduced_data$raceacs2, na.rm = T),
                        native_american = sum(reduced_data$raceacs3, na.rm = T),
                        asian = sum(reduced_data$raceacs4, reduced_data$raceacs5, 
                     reduced_data$raceacs6, reduced_data$raceacs7, 
                     reduced_data$raceacs8, reduced_data$raceacs9, 
                     reduced_data$raceacs10, na.rm = T),
                     hispanic = sum(reduced_data$raceacs15, na.rm = T),
                     other = sum(reduced_data$raceacs16, na.rm = T))

  

  #pivot_longer(everything(), names_to = "name", values_to = "value")

#### What's next? ####



         