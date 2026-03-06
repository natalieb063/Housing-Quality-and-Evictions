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
  summarise(n=n())

for(i in 2019:2022){
assign(paste0('viol',i), 
       dcast(viol_year %>% filter(viol_year==i), 
             BIN ~ paste0(Class,'_',i), value.var = 'n', fill=0))
       
}

viol2019$viol_2019=viol2019$A_2019+viol2019$B_2019+viol2019$C_2019+viol2019$I_2019
viol2020$viol_2020=viol2020$A_2020+viol2020$B_2020+viol2020$C_2020+viol2020$I_2020
viol2021$viol_2021=viol2021$A_2021+viol2021$B_2021+viol2021$C_2021+viol2021$I_2021
viol2022$viol_2022=viol2022$A_2022+viol2022$B_2022+viol2022$C_2022+viol2022$I_2022

#join all summary data by BIN
viol_all=full_join(viol2019, viol2020) %>%
  full_join(.,viol2021)%>%
  full_join(.,viol2022)
  left_join(viol_addresses)


#replace all NA's with 0
viol_all[is.na(viol_all)]=0
viol_all$total_viol=viol_all$viol_2023+viol_all$viol_2024+viol_all$viol_2025

write.csv(viol_all, paste0(output.path, 'Viol_by_Year.csv'), row.names=F)

# 
# viol_by_class <- violations_full  %>%
#   group_by(BIN, violation_class) %>%
#   summarise(n = n()) %>%
#   pivot_wider(names_from = violation_class, values_from = n) %>%
#   mutate(across(c(A:B), ~ replace_na(.x, 0)))
# 
# #checking distinctness
# viol_by_year %>%
#   group_by(BIN) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# viol_adds <- violations_full  %>%
#   mutate(full_address = case_when(house_number == 0 ~ paste0(street),
#                                   house_number != 0 ~ paste0(house_number, " ", street))) %>%
#   select(BIN, full_address,x ,y) %>%
#   distinct()
# 
# viol_by_BIN <- viol_by_year %>% left_join(viol_by_class, by = "BIN") %>%
#   rename(total_A = A,
#          total_B = B,
#          total_C = C,
#          total_I = I) %>%
#   left_join(viol_adds, by = "BIN") %>%
#   distinct(BIN, .keep_all = TRUE) %>%
#   select(BIN, full_address, x, y, n_viol, viol_2023, viol_2024, viol_2025, 
#          total_A, total_B, total_C, total_I)
#   
# 
# viol_by_BIN %>%
#   group_by(BIN) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 

#write.csv(viol_by_year, "viol_by_year.csv")
#write.csv(viol_by_class, "viol_by_class.csv")
#write.csv(viol_by_BIN, "viol_by_bin.csv")

#weird_bins_return <- viol_by_year %>% filter(BIN == 3000000 | BIN == 1000000 | BIN == 2000000 | BIN == 4000000 | BIN == 5000000)
#no weird bins!

#write.csv(evic_output, 'viol_by_year.csv')


