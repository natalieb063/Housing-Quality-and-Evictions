###############################################################################
### PROGRAM NAME:  EVICTIONS - 2026                                         ###
### DESCRIPTION:   Cleaning HPD Jurisdiction                                ###
### ANALYST:       Natalie Brown                                            ###
### DATE:          1/20/2026                                                ###
###############################################################################

options(scipen = 999)
rm(list = ls())

#LIBRARIES######################################################################

library(stringr)
library(dplyr)
library(haven)
library(tidyverse)
library(skimr)
library(data.table)
library(reshape2)

#PATHS & VARS###################################################################

hpd_juris_path <- 
output.path <- 

#DATA##########################################################################

hpd_juris <- fread(paste0(hpd_juris_path, 'Buildings_Subject_to_HPD_Jurisdiction_20260202.csv')) %>%
  select(-c(ManagementProgram, DoBBuildingClassID, DoBBuildingClass, LegalStories,
            LegalClassB, RegistrationID, RecordStatusID, RecordStatus)) %>%
  filter(LifeCycle != "Demolished")
  
weird_bins_hpdjuris <- hpd_juris %>%
  filter(BIN == 1000000 | BIN == 2000000| BIN == 3000000 |BIN == 4000000 |BIN == 5000000 | is.na(BIN) | BIN == 0)

not_weird_bins_hpdjuris <- hpd_juris %>%
  filter(BIN != 1000000 & BIN != 2000000 & BIN != 3000000 & BIN != 4000000 & BIN != 5000000 & !is.na(BIN) | BIN != 0)

#write.csv(weird_bins_hpdjuris, "hpd_juris_add.csv")
#write.csv(not_weird_bins_hpdjuris, "hpd_juris_BIN.csv")

####BRINGING BACK GEOCODED DATA         ########################################

geocoded_path <-

hpd_juris_add2 <- fread(paste0(geocoded_path, 'hpd_juris_add_SUCCESS_E.csv')) %>%
  select(-c(V1:V4, V7:V8, V11:V12, V14:V17))

new_names_hpd_juris_bins <- c("borough", "house_number", "street", "zipcode",
                         "BIN_old", "BIN_new", "x", "y")

colnames(hpd_juris_add2) <- new_names_hpd_juris_bins

junkbins=c(1000000,2000000,3000000,300000,4000000,5000000,0)

hpd_juris_updated=left_join(hpd_juris,hpd_juris_add2, by = join_by(HouseNumber == house_number,
                                                                   StreetName == street,
                                                                   Zip == zipcode,
                                                                   BIN == BIN_old,
                                                                   Boro == borough)) %>%
  select(-c(x, y)) %>%
  mutate(BIN=ifelse(!is.na(BIN_new), BIN_new, BIN)) %>%
  filter(BIN %in% junkbins==F & !is.na(BIN))

hpd_juris_output <- hpd_juris_updated %>%
  mutate(full_address = case_when(HouseNumber == 0 ~ paste0(StreetName),
                                  HouseNumber != 0 ~ paste0(HouseNumber, " ", StreetName))) %>%
  rename(n_units = LegalClassA) %>%
  select(BIN, full_address, Boro, Zip, n_units) %>%
  distinct(BIN, .keep_all=T)

hpd_juris_output %>%
   group_by(BIN) %>%
   summarise(n = n()) %>%
   arrange(desc(n))


write.csv(hpd_juris_output, paste0(output.path,'hpdjuris_by_BIN.csv'), row.names = F)
         
