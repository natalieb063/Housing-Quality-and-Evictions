###############################################################################
### PROGRAM NAME:  EVICTIONS - 2026                                         ###
### DESCRIPTION:   Cleaning Violations and Initial Description              ###
### ANALYST:       Natalie Brown                                            ###
### DATE:          1/12/2026                                                ###
###############################################################################

rm(list = ls())
options(scipen=999)

#LIBRARIES######################################################################

library(stringr)
library(dplyr)
library(haven)
library(tidyverse)
library(skimr)
library(data.table)
library(reshape2)

#PATHS & VARS###################################################################

violations_path <- '/Users/nataliebrown/Desktop/housing_quality_nyc/violations/'

#DATA##########################################################################

violations_2019 <- read.csv(paste0(violations_path, 'violations_2019.csv'))
violations_2020 <- read.csv(paste0(violations_path, 'violations_2020.csv'))
violations_2021 <- read.csv(paste0(violations_path, 'violations_2021.csv'))
violations_2022 <- read.csv(paste0(violations_path, 'violations_2022_premoratorium.csv'))

list_of_violations_dfs <- list(violations_2019 = violations_2019,
                               violations_2020 = violations_2020,
                               violations_2021 = violations_2021,
                               violations_2022 = violations_2022)

cols_to_keep <- c("ViolationID","Borough", "Class", "HouseNumber", "StreetName", "Postcode",
                  "InspectionDate", "BIN")

cleaned_violations_dfs <- lapply(list_of_violations_dfs, function(df) {
  df %>% select(all_of(cols_to_keep))
})

list2env(cleaned_violations_dfs, envir = .GlobalEnv)

violations_combined <- bind_rows(violations_2019, violations_2020, violations_2021, violations_2022) %>%
  mutate(viol_date = as.Date(InspectionDate, "%m/%d/%Y"),
         viol_year = year(viol_date))

violation_bins <- violations_combined %>% #assessing junk bins
  group_by(BIN) %>%
  summarise(n = n())

junk_bin <- c(1000000, 2000000, 3000000, 4000000, 5000000)

junk_bins <- violations_combined %>%
  filter(BIN %in% junk_bin | is.na(BIN))

write.csv(junk_bins, "viol_junk_bins_6mar2026.csv")

####BRINGING BACK GEOCODED DATA         ########################################

geocoding_folder <- "/Users/nataliebrown/Desktop/housing_quality_nyc/outputs/geocoded/"

violations_by_add <- fread(paste0(geocoding_folder, 'viol_junk_bins_SUCCESS_E.csv')) %>%
  select(-c(V1:V2, V10))

new_names_by_add<- c("ViolationID", "borough", "house_number","street", "zipcode",
                     "inspection_date", "BIN_old", "viol_year", "BIN_new")

colnames(violations_by_add) <- new_names_by_add

#matching back to original violations and replacing junk bins with geocoded counterparts
viol_updated <- left_join(violations_combined, violations_by_add %>% select(BIN_new, ViolationID)) %>%
  mutate(BIN=ifelse(!is.na(BIN_new), BIN_new, BIN)) %>%
  filter(!(BIN %in% junk_bin) & !is.na(BIN))
                       
viol_year <- viol_updated %>%
  group_by(BIN, Class, viol_year) %>% 
  summarise(n=n()) %>%
  pivot_wider(names_from = Class,
              values_from = n)

#replace all NA's with 0
viol_year[is.na(viol_year)]=0

viol_year <- viol_year %>%
  mutate(viol_total = sum(A,B,C,I))

write.csv(viol_year, 'viol_by_year_6mar2026', row.names=F)

