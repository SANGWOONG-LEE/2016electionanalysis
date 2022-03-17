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
         natsoc)
rm(raw_data)

race_data <- reduced_data %>%
  select(raceacs1, 
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
         raceacs16) %>%
  

  #pivot_longer(everything(), names_to = "name", values_to = "value")

#### What's next? ####



         