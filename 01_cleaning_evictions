###############################################################################
### PROGRAM NAME:  EVICTIONS - 2026                                         ###
### DESCRIPTION:   Cleaning Evictions and Initial Description               ###
### ANALYST:       Natalie Brown                                            ###
### DATE:          1/12/2026                                                ###
###############################################################################


Sys.setenv(http_proxy = "bcpxy.nycnet:8080")
rm(list = ls())
options(scipen=999)

#LIBRARIES######################################################################

library(stringr)
library(dplyr)
library(haven)
library(tidyverse)
library(skimr)
library(data.table) #added by JS 2/19

#PATHS & VARS###################################################################

marshals_evictions_path <- "//Chgoldfs/dmhhs/CIDI Staff/Projects/Evictions/2026_top_evicters/data/marshal_evictions/"
data.path='//CHGOLDFS/Cidi_Secure/eviction/evictions_2026/cleaned_data/'

#DATA##########################################################################

evictions_20260219 <- read.csv(paste0(marshals_evictions_path, 'Evictions_20260219.csv'))

evictions_distinct=evictions_20260219 %>% distinct() #there are 6 duplicates

evictions_distinct_limited=
  evictions_20260219 %>% distinct(BIN, Eviction.Address, Eviction.Apartment.Number, Executed.Date, BOROUGH, 
                                  Court.Index.Number,Docket.Number. )

#removing commercial evictions and names for Marshals
evictions_res_20260219 <- evictions_20260219 %>%
  filter(Residential.Commercial == "Residential") %>% 
  select(-c(Marshal.First.Name, Marshal.Last.Name, Residential.Commercial)) %>%
  mutate(evic_date = as.Date(Executed.Date, "%m/%d/%Y"),
         evic_year = year(evic_date))



evictions_res_20260219 %>%
  group_by(BIN) %>%
  summarise(n_evic = n()) %>%
  arrange(desc(n_evic))
#junk bins are present

#isolating junk bins
weird_bins <- evictions_res_20260219 %>%
  filter(BIN == 1000000 | BIN == 2000000| BIN == 3000000 |BIN == 4000000 |BIN == 5000000 | is.na(BIN)) %>%
  mutate(address = Eviction.Address) %>%
  separate(col = address,
           into = c("house_number", "street"),
           sep = " ",
           extra = "merge")

#remove apartment numbers, building numbers, and floor numbers from the street field.
#remove punctuation 
weird_bins$street = gsub("[[:punct:]]", '', weird_bins$street)
#trim leading and ending whitespace
weird_bins$street=trimws(weird_bins$street)

weird_bins$street = gsub("\\ NO. .*","", weird_bins$street)
weird_bins$street = gsub("\\ APT .*","", weird_bins$street)
weird_bins$street = gsub("\\ APT.*","", weird_bins$street)
weird_bins$street = gsub("\\APT.*","", weird_bins$street)
weird_bins$street = gsub("\\ APARTMENT .*","", weird_bins$street)
weird_bins$street = gsub("\\ APARTMENT.*","", weird_bins$street)
weird_bins$street = gsub("\\ BLDG .*","", weird_bins$street)
weird_bins$street = gsub("\\ BUILDING .*","", weird_bins$street)
weird_bins$street = gsub("\\ UNIT .*","", weird_bins$street)
weird_bins$street = gsub("\\ UNIT.*","", weird_bins$street)
weird_bins$street = gsub("\\ FL .*","", weird_bins$street)
weird_bins$street = gsub("\\ FLR .*","", weird_bins$street)
weird_bins$street = gsub("\\ FLOOR .*","", weird_bins$street)
weird_bins$street = gsub("\\ RM .*","", weird_bins$street)
weird_bins$street = gsub("\\ ROOM .*","", weird_bins$street)
weird_bins$street = gsub("\\ 1ST FL.*","", weird_bins$street)
weird_bins$street = gsub("\\ FIRST FL.*","", weird_bins$street)
weird_bins$street = gsub("\\ 1ST FLR.*","", weird_bins$street)
weird_bins$street = gsub("\\ 2ND FL.*","", weird_bins$street)
weird_bins$street = gsub("\\ SECOND FL.*","", weird_bins$street)
weird_bins$street = gsub("\\ 2ND FLR.*","", weird_bins$street)
weird_bins$street = gsub("\\ 3RD FL.*","", weird_bins$street)
weird_bins$street = gsub("\\ THIRD FL.*","", weird_bins$street)
weird_bins$street = gsub("\\ 3RD FLR.*","", weird_bins$street)
# weird_bins$street = gsub("\\ A/K/A.*", "", weird_bins$street)
weird_bins$street = gsub("\\ ENTIRE.*", "", weird_bins$street)
weird_bins$street = gsub("\\ENTIRE.*", "", weird_bins$street)
weird_bins$street = gsub("\\(.*", "", weird_bins$street)
weird_bins$street = gsub("\\CLOSET.*", "", weird_bins$street)
weird_bins$street = gsub("\\GROUND.*", "", weird_bins$street)
weird_bins$street = gsub("\\BASEMENT.*", "", weird_bins$street)
weird_bins$street = gsub("\\DOOR.*", "", weird_bins$street)
weird_bins$street = gsub("\\CONSISTING.*", "", weird_bins$street)
#weird_bins$street = gsub("\\AKA.*", "", weird_bins$street)
weird_bins$street = gsub(".* AKA ", "", weird_bins$street)
weird_bins$street = gsub(".* A K A ", "", weird_bins$street)
weird_bins$street = gsub("AKA","", weird_bins$street)
weird_bins$street = gsub("\\LIVING.*", "", weird_bins$street)
weird_bins$street = gsub("\\ALL ROOMS.*", "", weird_bins$street)
weird_bins$street = gsub("\\,.*", "", weird_bins$street)
weird_bins$street = gsub("\\ROOMS.*", "", weird_bins$street)
weird_bins$street = gsub("\\ALL COMMON AREAS.*", "", weird_bins$street)
weird_bins$street = gsub("\\FRONT ENTRANCE.*", "", weird_bins$street)
weird_bins$street = gsub("\\  ", " ", weird_bins$street)
weird_bins$street = gsub("\\   ", " ", weird_bins$street)
weird_bins$street = gsub("\\    ", " ", weird_bins$street)
weird_bins$street = gsub("\\STREE T", "STREET", weird_bins$street)
weird_bins$street = gsub("\\STRE ET", "STREET", weird_bins$street)
weird_bins$street = gsub("\\STR EET", "STREET", weird_bins$street)
weird_bins$street = gsub("\\ST REET", "STREET", weird_bins$street)
weird_bins$street = gsub("\\S TREET", "STREET", weird_bins$street)
weird_bins$street = gsub("\\P LACE", "PLACE", weird_bins$street)
weird_bins$street = gsub("\\PL ACE", "PLACE", weird_bins$street)
weird_bins$street = gsub("\\PLA CE", "PLACE", weird_bins$street)
weird_bins$street = gsub("\\PLAC E", "PLACE", weird_bins$street)
weird_bins$street = gsub("\\BOYL AND", "BOYLAND", weird_bins$street)
weird_bins$street = gsub("\\BBOYLAND", "BOYLAND", weird_bins$street)
weird_bins$street = gsub("\\PO WELL", "POWELL", weird_bins$street)
weird_bins$street = gsub("\\WES T", "WEST", weird_bins$street)
weird_bins$street = gsub("\\WE ST", "WEST", weird_bins$street)
weird_bins$street = gsub("\\W EST", "WEST", weird_bins$street)
weird_bins$street = gsub("\\S OUTH", "SOUTH", weird_bins$street)
weird_bins$street = gsub("\\SO UTH", "SOUTH", weird_bins$street)
weird_bins$street = gsub("\\SOU TH", "SOUTH", weird_bins$street)
weird_bins$street = gsub("\\SOUT H", "SOUTH", weird_bins$street)
weird_bins$street = gsub("\\B OULEVARD", "BOULEVARD", weird_bins$street)
weird_bins$street = gsub("\\BO ULEVARD", "BOULEVARD", weird_bins$street)
weird_bins$street = gsub("\\BOU LEVARD", "BOULEVARD", weird_bins$street)
weird_bins$street = gsub("\\BOUL EVARD", "BOULEVARD", weird_bins$street)
weird_bins$street = gsub("\\BOULE VARD", "BOULEVARD", weird_bins$street)
weird_bins$street = gsub("\\BOULEV ARD", "BOULEVARD", weird_bins$street)
weird_bins$street = gsub("\\BOULEVA RD", "BOULEVARD", weird_bins$street)
weird_bins$street = gsub("\\BOULEVAR D", "BOULEVARD", weird_bins$street)
weird_bins$street = gsub("\\BLVD", "BOULEVARD", weird_bins$street)
weird_bins$street = gsub("\\BL VD", "BOULEVARD", weird_bins$street)
weird_bins$street = gsub("\\A VENUE", "AVENUE", weird_bins$street)
weird_bins$street = gsub("\\AV ENUE", "AVENUE", weird_bins$street)
weird_bins$street = gsub("\\AVE NUE", "AVENUE", weird_bins$street)
weird_bins$street = gsub("\\AVEN UE", "AVENUE", weird_bins$street)
weird_bins$street = gsub("\\AVENU E", "AVENUE", weird_bins$street)
weird_bins$street = gsub("\\PARKWA Y", "PARKWAY", weird_bins$street)
weird_bins$street = gsub("\\PARKW AY", "PARKWAY", weird_bins$street)
weird_bins$street = gsub("\\PARK WAY", "PARKWAY", weird_bins$street)
weird_bins$street = gsub("\\PAR KWAY", "PARKWAY", weird_bins$street)
weird_bins$street = gsub("\\PA RKWAY", "PARKWAY", weird_bins$street)
weird_bins$street = gsub("\\P ARKWAY", "PARKWAY", weird_bins$street)
weird_bins$street = gsub("\\PKWAY", "PARKWAY", weird_bins$street)
weird_bins$street = gsub("\\PKWY", "PARKWAY", weird_bins$street)
weird_bins$street = gsub("\\R OAD", "ROAD", weird_bins$street)
weird_bins$street = gsub("\\RO AD", "ROAD", weird_bins$street)
weird_bins$street = gsub("\\ROA D", "ROAD", weird_bins$street)
weird_bins$street = gsub("\\ RD", "ROAD", weird_bins$street)
weird_bins$street = gsub("\\BBOULEVARD", "BOULEVARD", weird_bins$street)
#remove non-numeric values from the house number field.
weird_bins$house_number=gsub('[^0-9.-]','',weird_bins$house_number)

#write.csv(weird_bins, "weird_bins_cleaned.csv")
#write.csv(weird_bins, "weird_bins_cleaned_v2.csv")

#this didn't add up to 45,210 (weird + not weird = 45,226)
# not_weird_bins <- evictions_res_20260219 %>% #note: this originally called "evictions_res" without the date
#   filter(BIN %in% c(1000000,2000000,3000000,4000000,500000)==F & !is.na(BIN)) 

not_weird_bins=anti_join(evictions_res_20260219, weird_bins) 


#write.csv(weird_bins, "faulty_bins_19feb2026.csv")
write.csv(not_weird_bins, paste0(data.path, "evic_bins_16feb2026.csv"), row.names = F)

#bringing back corrected bins

geocoding_folder <- "//CHGOLDFS/CIDI_Secure/eviction/evictions_2026/geocoding/"

evictions_by_add_attempt2 <- fread(paste0(geocoding_folder, 'weird_bins_take2_SUCCESS.csv')) %>%
  select(-c(V1:V2, V5, V10:V16, V18:V20))

new_names_by_add_attempt2 <- c("court_index", "docket_number", "apt_number",
                          "evic_date", "borough", "zipcode", "BIN_old",
                          "evic_year", "house_number","street", "BIN_new","x", "y")

colnames(evictions_by_add_attempt2) <- new_names_by_add_attempt2 

###preparing datasets for combination

evictions_ungeocoded <- not_weird_bins %>%
  rename(court_index = Court.Index.Number,
         docket_nuber = Docket.Number.,
         full_address = Eviction.Address,
         apt_number = Eviction.Apartment.Number,
         borough = BOROUGH,
         zipcode = Eviction.Postcode) %>% 
  #mutate(evic_year = year(as.Date(evic_date, "%m/%d/%Y"))) %>% #js added as.Date
  select(BIN, full_address, borough, zipcode, evic_year, evic_date) #NB added to dataset per JS request

evic_geocoded_by_add <- evictions_by_add_attempt2 %>%
  mutate(full_address = paste0(house_number, " ", street), #NB added 2/20
         BIN_new = as.numeric(BIN_new)) %>%
  filter(!is.na(BIN_new)) %>%
  select(BIN_new, full_address, borough, zipcode, evic_year, evic_date) %>%
  rename(BIN = BIN_new) %>%
  mutate(BIN = as.integer(BIN),
         evic_date = as.Date(evic_date, "%m/%d/%Y")) #NB added 2/20


evictions_combined <- 
  bind_rows(evictions_ungeocoded, evic_geocoded_by_add)
  #distinct() # this removed multiple evictions on the same date
#%>%
  #distinct(BIN, .keep_all = TRUE) #- you lost multiple records for each BIN

evictions_combined %>% group_by(BIN) %>% summarise(n = n()) %>% arrange(desc(n))
#this should have set off an alarm for you that you have one eviction per bin
# NB no longer one eviction per bin

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

#write.csv(evic_output, 'evic_by_year_19feb2026.csv')
write.csv(evic_output, paste0(data.path,'evic_by_year_20feb2026.csv'), row.names = F)


