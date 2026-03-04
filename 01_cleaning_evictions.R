###############################################################################
### PROGRAM NAME:  EVICTIONS - 2026                                         ###
### DESCRIPTION:   Cleaning Evictions and Initial Description               ###
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

marshals_evictions_path <- "_"
data.path <- "_" 

#DATA##########################################################################

evictions_20260219 <- read.csv(paste0(marshals_evictions_path, 'Evictions_20260219.csv'))

evictions_distinct=evictions_20260219 %>% distinct() #there are 6 duplicates

evictions_distinct_limited=
  evictions_20260219 %>% distinct(BIN, Eviction.Address, Eviction.Apartment.Number, Executed.Date, BOROUGH, 
                                  Court.Index.Number, Docket.Number. )

#removing commercial evictions and names for Marshals
evictions_res_20260219 <- evictions_20260219 %>%
  filter(Residential.Commercial == "Residential") %>% 
  select(-c(Marshal.First.Name, Marshal.Last.Name, Residential.Commercial)) %>%
  mutate(evic_date = as.Date(Executed.Date, "%m/%d/%Y"),
         evic_year = year(evic_date))

#quick check of evictions per building
evictions_res_20260219 %>%
  group_by(BIN) %>%
  summarise(n_evic = n()) %>%
  arrange(desc(n_evic))
#some bins are fillers for the borough but not attached to the building itself, need to adjust before matching 

#isolating junk bins
junk_bin <- c(1000000, 2000000, 3000000, 4000000, 5000000)

junk_bins <- evictions_res_20260219 %>%
  filter(BIN %in% junk_bin | is.na(BIN)) %>%
  mutate(address = Eviction.Address) %>%
  separate(col = address,
           into = c("house_number", "street"),
           sep = " ",
           extra = "merge")

#isolating the evictions that already have BINs
not_junk_bins <- evictions_res_20260219 %>%
filter(!BIN %in% junk_bin | !is.na(BIN))

#ADDRESS CLEANING FOR GEOCODING TO GET BIN

#remove punctuation from street field
junk_bins$street = gsub("[[:punct:]]", '', junk_bins$street)

#trim leading and ending whitespace
junk_bins$street=trimws(junk_bins$street)

#remove non-numeric values from the house number field.
junk_bins$house_number=gsub('[^0-9.-]','',junk_bins$house_number)

#cleaning specific data issues (NB: can this code be done more efficiently?)
junk_bins$street = gsub("\\ NO. .*","", junk_bins$street)
junk_bins$street = gsub("\\ APT .*","", junk_bins$street)
junk_bins$street = gsub("\\ APT.*","", junk_bins$street)
junk_bins$street = gsub("\\APT.*","", junk_bins$street)
junk_bins$street = gsub("\\ APARTMENT .*","", junk_bins$street)
junk_bins$street = gsub("\\ APARTMENT.*","", junk_bins$street)
junk_bins$street = gsub("\\ BLDG .*","", junk_bins$street)
junk_bins$street = gsub("\\ BUILDING .*","", junk_bins$street)
junk_bins$street = gsub("\\ UNIT .*","", junk_bins$street)
junk_bins$street = gsub("\\ UNIT.*","", junk_bins$street)
junk_bins$street = gsub("\\ FL .*","", junk_bins$street)
junk_bins$street = gsub("\\ FLR .*","", junk_bins$street)
junk_bins$street = gsub("\\ FLOOR .*","", junk_bins$street)
junk_bins$street = gsub("\\ RM .*","", junk_bins$street)
junk_bins$street = gsub("\\ ROOM .*","", junk_bins$street)
junk_bins$street = gsub("\\ 1ST FL.*","", junk_bins$street)
junk_bins$street = gsub("\\ FIRST FL.*","", junk_bins$street)
junk_bins$street = gsub("\\ 1ST FLR.*","", junk_bins$street)
junk_bins$street = gsub("\\ 2ND FL.*","", junk_bins$street)
junk_bins$street = gsub("\\ SECOND FL.*","", junk_bins$street)
junk_bins$street = gsub("\\ 2ND FLR.*","", junk_bins$street)
junk_bins$street = gsub("\\ 3RD FL.*","", junk_bins$street)
junk_bins$street = gsub("\\ THIRD FL.*","", junk_bins$street)
junk_bins$street = gsub("\\ 3RD FLR.*","", junk_bins$street)
# junk_bins$street = gsub("\\ A/K/A.*", "", junk_bins$street)
junk_bins$street = gsub("\\ ENTIRE.*", "", junk_bins$street)
junk_bins$street = gsub("\\ENTIRE.*", "", junk_bins$street)
junk_bins$street = gsub("\\(.*", "", junk_bins$street)
junk_bins$street = gsub("\\CLOSET.*", "", junk_bins$street)
junk_bins$street = gsub("\\GROUND.*", "", junk_bins$street)
junk_bins$street = gsub("\\BASEMENT.*", "", junk_bins$street)
junk_bins$street = gsub("\\DOOR.*", "", junk_bins$street)
junk_bins$street = gsub("\\CONSISTING.*", "", junk_bins$street)
#junk_bins$street = gsub("\\AKA.*", "", junk_bins$street)
junk_bins$street = gsub(".* AKA ", "", junk_bins$street)
junk_bins$street = gsub(".* A K A ", "", junk_bins$street)
junk_bins$street = gsub("AKA","", junk_bins$street)
junk_bins$street = gsub("\\LIVING.*", "", junk_bins$street)
junk_bins$street = gsub("\\ALL ROOMS.*", "", junk_bins$street)
junk_bins$street = gsub("\\,.*", "", junk_bins$street)
junk_bins$street = gsub("\\ROOMS.*", "", junk_bins$street)
junk_bins$street = gsub("\\ALL COMMON AREAS.*", "", junk_bins$street)
junk_bins$street = gsub("\\FRONT ENTRANCE.*", "", junk_bins$street)
junk_bins$street = gsub("\\  ", " ", junk_bins$street)
junk_bins$street = gsub("\\   ", " ", junk_bins$street)
junk_bins$street = gsub("\\    ", " ", junk_bins$street)
junk_bins$street = gsub("\\STREE T", "STREET", junk_bins$street)
junk_bins$street = gsub("\\STRE ET", "STREET", junk_bins$street)
junk_bins$street = gsub("\\STR EET", "STREET", junk_bins$street)
junk_bins$street = gsub("\\ST REET", "STREET", junk_bins$street)
junk_bins$street = gsub("\\S TREET", "STREET", junk_bins$street)
junk_bins$street = gsub("\\P LACE", "PLACE", junk_bins$street)
junk_bins$street = gsub("\\PL ACE", "PLACE", junk_bins$street)
junk_bins$street = gsub("\\PLA CE", "PLACE", junk_bins$street)
junk_bins$street = gsub("\\PLAC E", "PLACE", junk_bins$street)
junk_bins$street = gsub("\\BOYL AND", "BOYLAND", junk_bins$street)
junk_bins$street = gsub("\\BBOYLAND", "BOYLAND", junk_bins$street)
junk_bins$street = gsub("\\PO WELL", "POWELL", junk_bins$street)
junk_bins$street = gsub("\\WES T", "WEST", junk_bins$street)
junk_bins$street = gsub("\\WE ST", "WEST", junk_bins$street)
junk_bins$street = gsub("\\W EST", "WEST", junk_bins$street)
junk_bins$street = gsub("\\S OUTH", "SOUTH", junk_bins$street)
junk_bins$street = gsub("\\SO UTH", "SOUTH", junk_bins$street)
junk_bins$street = gsub("\\SOU TH", "SOUTH", junk_bins$street)
junk_bins$street = gsub("\\SOUT H", "SOUTH", junk_bins$street)
junk_bins$street = gsub("\\B OULEVARD", "BOULEVARD", junk_bins$street)
junk_bins$street = gsub("\\BO ULEVARD", "BOULEVARD", junk_bins$street)
junk_bins$street = gsub("\\BOU LEVARD", "BOULEVARD", junk_bins$street)
junk_bins$street = gsub("\\BOUL EVARD", "BOULEVARD", junk_bins$street)
junk_bins$street = gsub("\\BOULE VARD", "BOULEVARD", junk_bins$street)
junk_bins$street = gsub("\\BOULEV ARD", "BOULEVARD", junk_bins$street)
junk_bins$street = gsub("\\BOULEVA RD", "BOULEVARD", junk_bins$street)
junk_bins$street = gsub("\\BOULEVAR D", "BOULEVARD", junk_bins$street)
junk_bins$street = gsub("\\BLVD", "BOULEVARD", junk_bins$street)
junk_bins$street = gsub("\\BL VD", "BOULEVARD", junk_bins$street)
junk_bins$street = gsub("\\A VENUE", "AVENUE", junk_bins$street)
junk_bins$street = gsub("\\AV ENUE", "AVENUE", junk_bins$street)
junk_bins$street = gsub("\\AVE NUE", "AVENUE", junk_bins$street)
junk_bins$street = gsub("\\AVEN UE", "AVENUE", junk_bins$street)
junk_bins$street = gsub("\\AVENU E", "AVENUE", junk_bins$street)
junk_bins$street = gsub("\\PARKWA Y", "PARKWAY", junk_bins$street)
junk_bins$street = gsub("\\PARKW AY", "PARKWAY", junk_bins$street)
junk_bins$street = gsub("\\PARK WAY", "PARKWAY", junk_bins$street)
junk_bins$street = gsub("\\PAR KWAY", "PARKWAY", junk_bins$street)
junk_bins$street = gsub("\\PA RKWAY", "PARKWAY", junk_bins$street)
junk_bins$street = gsub("\\P ARKWAY", "PARKWAY", junk_bins$street)
junk_bins$street = gsub("\\PKWAY", "PARKWAY", junk_bins$street)
junk_bins$street = gsub("\\PKWY", "PARKWAY", junk_bins$street)
junk_bins$street = gsub("\\R OAD", "ROAD", junk_bins$street)
junk_bins$street = gsub("\\RO AD", "ROAD", junk_bins$street)
junk_bins$street = gsub("\\ROA D", "ROAD", junk_bins$street)
junk_bins$street = gsub("\\ RD", "ROAD", junk_bins$street)
junk_bins$street = gsub("\\BBOULEVARD", "BOULEVARD", junk_bins$street)

#writing datasets for geocoding export 
#write.csv(junk_bins, "junk_bins_cleaned.csv")
#write.csv(junk_bins, "junk_bins_cleaned_v2.csv")


#write.csv(junk_bins, "faulty_bins_19feb2026.csv")
write.csv(not_junk_bins, paste0(data.path, "evic_bins_16feb2026.csv"), row.names = F)

#BRINGING BACK CORRECTED BINS FOR DATA AGGREGATION

geocoding_folder <- "_"

evictions_by_add_attempt2 <- fread(paste0(geocoding_folder, 'junk_bins_take2_SUCCESS.csv')) %>%
  select(-c(V1:V2, V5, V10:V16, V18:V20))

new_names_by_add_attempt2 <- c("court_index", "docket_number", "apt_number",
                          "evic_date", "borough", "zipcode", "BIN_old",
                          "evic_year", "house_number","street", "BIN_new","x", "y")

colnames(evictions_by_add_attempt2) <- new_names_by_add_attempt2 

#standardizing datasets for combination

evictions_ungeocoded <- not_junk_bins %>%
  rename(court_index = Court.Index.Number,
         docket_nuber = Docket.Number.,
         full_address = Eviction.Address,
         apt_number = Eviction.Apartment.Number,
         borough = BOROUGH,
         zipcode = Eviction.Postcode) %>% 
  select(BIN, full_address, borough, zipcode, evic_year, evic_date)

evic_geocoded_by_add <- evictions_by_add_attempt2 %>%
  mutate(full_address = paste0(house_number, " ", street), 
         BIN_new = as.numeric(BIN_new)) %>%
  filter(!is.na(BIN_new)) %>%
  select(BIN_new, full_address, borough, zipcode, evic_year, evic_date) %>%
  rename(BIN = BIN_new) %>%
  mutate(BIN = as.integer(BIN),
         evic_date = as.Date(evic_date, "%m/%d/%Y"))

#combining evictions with and without BINs
evictions_combined <- bind_rows(evictions_ungeocoded, evic_geocoded_by_add)

evictions_combined %>% group_by(BIN) %>% summarise(n = n()) %>% arrange(desc(n))

#creating summary statistics
evic_by_year <- 
  evictions_combined  %>%
  group_by(BIN, evic_year) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = evic_year, values_from = n) %>%
  rename(evic_2023 = `2023`,
         evic_2024 = `2024`,
         evic_2025 = `2025`) %>%
  mutate(across(c(evic_2025:evic_2024), ~ replace_na(.x, 0)),
         n_evic = sum(evic_2023, evic_2024, evic_2025)) %>%
  select(BIN, n_evic, evic_2023, evic_2024, evic_2025)

evic_output <- evictions_combined %>% 
  select(BIN, full_address, borough, zipcode) %>%
  left_join(evic_by_year, by = "BIN") %>%
  distinct(BIN, .keep_all = TRUE)

#exporting summary tables
#write.csv(evic_output, 'evic_by_year_19feb2026.csv')
write.csv(evic_output, paste0(data.path,'evic_by_year_20feb2026.csv'), row.names = F)


