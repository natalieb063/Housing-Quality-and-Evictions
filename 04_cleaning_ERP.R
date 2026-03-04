###############################################################################
### PROGRAM NAME:  EVICTIONS - 2026                                         ###
### DESCRIPTION:   03- Cleaning Charges                                     ###
### ANALYST:       Natalie Brown                                            ###
### DATE:          2/16/2026                                                ###
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

#PATHS & VARS###################################################################

charges_path <-
output.path <- 
geocoded_path <- 
geo.output.path <- 
#DATA##########################################################################

invoices <- read_csv(_)
charges_invoices <- read_csv(_) %>%
  mutate(OMOCreateDate = mdy(OMOCreateDate)) %>%
  filter(year(OMOCreateDate) > 2022 & year(OMOCreateDate) < 2026) %>%
  left_join(invoices, by = "OMONumber")

#charges_distinct=charges %>% distinct()
#charges_distinct=charges %>% distinct(OMONumber)

#skim(charges)

#counting charges by BIN for our time period 
charges_bins = charges_invoices %>%
  group_by(BIN) %>%
  summarise(n = n())

#isolating junk bins
weird_bins <- charges_invoices %>%
  filter(BIN == 1000000 | BIN == 2000000| BIN == 3000000 |BIN == 4000000 |BIN == 5000000 | is.na(BIN))

not_weird_bins <- charges_invoices %>%
  filter(BIN != 1000000 & BIN != 2000000 & BIN != 3000000 & BIN != 4000000 & BIN != 5000000 & !is.na(BIN))

#light address cleaning
weird_bins$HouseNumber = gsub("\\GAR.*", "", weird_bins$HouseNumber)
weird_bins$HouseNumber = gsub("\\REAR.*", "", weird_bins$HouseNumber)
weird_bins$HouseNumber = gsub("\\N.*", "", weird_bins$HouseNumber)
weird_bins$HouseNumber = gsub("\\A.*", "", weird_bins$HouseNumber)
weird_bins$HouseNumber = gsub("\\FRO.*", "", weird_bins$HouseNumber)

#ADDING IN SPECIFIC ZIPCODES FOR GEOCODING
weird_bins <- weird_bins %>%
  mutate(Zip = case_when(OMONumber == "DN00325" ~ 10464,
                        OMONumber == "DO00058" ~ 11436,
                        OMONumber == "DQ00057" ~ 11429,
                        OMONumber == "DP00193" ~ 11432,
                        OMONumber == "DP00126" ~ 11357,
                        OMONumber == "DP00403" ~ 11225,
                        OMONumber == "DQ00152" ~ 11433,
                        OMONumber == "EN20033" ~ 11237,
                        OMONumber == "EN16808" ~ 10468,
                        OMONumber == "EN20974" ~ 11226,
                        OMONumber == "DP00021" ~ 11420,
                        OMONumber == "DP00325" ~ 10474,
                        OMONumber == "EN21073" ~ 11694,
                        OMONumber == "DO00057" ~ 11436,
                        OMONumber == "DP00242" ~ 11420,
                        OMONumber == "DN00399" ~ 10464,
                        OMONumber == "EO29176" ~ 11411,
                        OMONumber == "EO00358" ~ 11235,
                        OMONumber == "DO00260" ~ 11436,
                        OMONumber == "DQ00201" ~ 11106,
                        OMONumber == "EQ00754" ~ 11235,
                        OMONumber == "EO07308" ~ 11235,
                        OMONumber == "EN30841" ~ 11235,
                        OMONumber == "EP00648" ~ 11235,
                        OMONumber == "EP00646" ~ 11235,
                        OMONumber == "EN24468" ~ 11203,
                        OMONumber == "DO00066" ~ 11215,
                        OMONumber == "EN26546" ~ 10003,
                        OMONumber == "EP05460" ~ 11235,
                        OMONumber == "EO08657" ~ 11235,
                        OMONumber == "EO15151" ~ 11221,
                        OMONumber == "EN21896" ~ 10461,
                        OMONumber == "EN20786" ~ 10003,
                        OMONumber == "EQ00614" ~ 11235,
                        OMONumber == "EO10014" ~ 11235,
                        OMONumber == "DN00336" ~ 10464,
                        OMONumber == "EN21957" ~ 11226,
                        OMONumber == "EN24235" ~ 11694,
                        OMONumber == "EP07085" ~ 10467,
                        OMONumber == "DP00179" ~ 11420,
                        OMONumber == "DP00179" ~ 11420,
                        OMONumber == "DQ00056" ~ 11434,
                        OMONumber == "DP00246" ~ 11355,
                        OMONumber == "DP00246" ~ 11355,
                        OMONumber == "DP00444" ~ 11419,
                        OMONumber == "DQ00244" ~ 11106,
                        OMONumber == "DP00461" ~ 11419,
                        OMONumber == "DQ00242" ~ 11106,
                        OMONumber == "DQ00243" ~ 11106,
                        OMONumber == "DQ00135" ~ 11207,
                        OMONumber == "DQ00185" ~ 11207,
                        OMONumber == "DQ00205" ~ 11106,
                             .default = Zip
                             ))

#write.csv(weird_bins, paste0(geo.output.path, "charges_faulty_bins_4mar2026.csv"), row.names = F)
#write.csv(not_weird_bins_charges, "charges_bins_18feb2026.csv") 

####BRINGING BACK GEOCODED DATA         ########################################

charges_add2 <- fread(paste0(geocoded_path, 'charges_faulty_bins_18feb2026_SUCCESS_E.csv')) %>%
  select(-c(V1:V3, V5:V6, V12:V14, V16, V18, V20, V24:V31, V33:V34))

new_names_charges_add <- c("OMONumber","Boro", "HouseNumber", "StreetName", "Apartment", 
                            "Zip", "WorkTypeGeneral", "OMOAwardAmount", "NetChangeOrders",
                            "IsAEP", "IsCommercialDemolition", "ServiceChargeFlag", "BIN",
                            "BIN_new", "x", "y")

colnames(charges_add2) <- new_names_charges_add 
