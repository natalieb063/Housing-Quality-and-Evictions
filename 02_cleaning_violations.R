###############################################################################
### PROGRAM NAME:  EVICTIONS - 2026                                         ###
### DESCRIPTION:   Cleaning Violations and Initial Description              ###
### ANALYST:       Natalie Brown                                            ###
### DATE:          1/12/2026                                                ###
###############################################################################

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

violations_path <- 
output.path <- 
#DATA##########################################################################

violations <- fread(paste0(violations_path, 'Housing_Maintenance_Code_Violations_20260108.csv')) %>%
  mutate(inspection_date = mdy(InspectionDate)) %>%
  filter(year(inspection_date) != 2022)

violations.distinct=violations %>% distinct()

violations.distinct=violations %>% distinct(ViolationID)

#skim(violations)

#REMOVE 2022 - JS note:  you dont' remove 2022 here, does that happen later or did that already happen?
#NB:on line 30
violation_bins <- violations %>%
  group_by(BIN) %>%
  summarise(n = n())

weird_bins_viol <- violations %>%
  filter(BIN == 1000000 | BIN == 2000000| BIN == 3000000 |BIN == 4000000 |BIN == 5000000 | is.na(BIN))

not_weird_bins_viol <- violations %>%
  filter(BIN != 1000000 & BIN != 2000000 & BIN != 3000000 & BIN != 4000000 & BIN != 5000000 & !is.na(BIN)) %>%
  select(-c(NOVDescription))

#write.csv(weird_bins_viol, "viol_faulty_bins_16feb2026.csv")
#write.csv(not_weird_bins_viol, "viol_bins_16feb2026.csv")

#seems like this code is not needed anymore? from lines 49-56?
weird_bins_distinct <- weird_bins_viol %>%
  distinct(HouseNumber, StreetName, Borough)

violations_cleaned <- violations %>%
  mutate(inspection_date = mdy(InspectionDate))

is_unique_key <- n_distinct(violations_cleaned$ViolationID) == nrow(violations_cleaned)
print(is_unique_key) #true

#ANALYSES #######################################################################
#this is just get descriptives of the original dataset, right?  
#NB: yes

# #violations by borough
# 
# #1 = MN, #2 = BX, #3 = BK, #4 = QN, #5 = SI
# violations_cleaned %>%
#   group_by(BoroID) %>%
#   summarise(total_by_borough = n()) %>%
#   mutate(percentage_share = (total_by_borough / sum(total_by_borough)) * 100)
# 
# #violations by year
# #lowest number in 2022, could be worth omitting? 
# 
# violations_cleaned %>%
#   group_by(year(inspection_date)) %>%
#   summarise(total_by_year = n()) %>%
#   mutate(percentage_share = (total_by_year / sum(total_by_year)) * 100)
# 
# #violations by borough and year
# 
# violations_cleaned %>%
#   group_by(BoroID, year(inspection_date)) %>%
#   summarise(total_by_year_and_borough = n()) %>%
#   ungroup() %>%
#   group_by(`year(inspection_date)`) %>%
#   mutate(percentage_share_by_year = (total_by_year_and_borough / sum(total_by_year_and_borough)) * 100)
# 

#write_csv(violations_cleaned, "violations_13Jan2026.csv")

####BRINGING BACK GEOCODED DATA         ########################################

geocoded_path <- 

#note - commented out pulling back in the geocoded data because you're going to merge with 
#the violations_bins to the weird bins that were geocoded to obtain a real bin

# viol_bins <- fread(paste0(geocoded_path, 'viol_bins_SUCCESS.csv')) %>% 
#   select(-c(V1:V2, V4:V6, V9:V10, V12, V15:V17, V20:V22, V20:V39, V41:V43))
# viol_add1 <- fread(paste0(geocoded_path, 'viol_bins_attempt2_add_SUCCESS.csv')) %>%
#   select(-c(V1:V2, V4:V6, V9:V10, V12, V14:V17, V19:V22, V20:V39, V41:V43))
viol_add2 <- fread(paste0(geocoded_path, 'viol_faulty_bins_add_SUCCESS_E.csv')) %>%
  select(-c(V1:V2, V4:V6, V9:V10, V12, V14:V17, V20:V40, V42:V44))

# new_names_viol_bins <- c("violation_ID", "borough", "house_number", "street", "zipcode",
#                          "apt_number", "violation_class", "inspection_date", 
#                          "BIN", "x", "y")
# 
# colnames(viol_bins) <- new_names_viol_bins
# 
# new_names_viol_add1 <- c("violation_ID", "borough", "house_number", "street", "zipcode",
#                           "violation_class", "BIN_new", "BIN_old", 
#                          "x", "y")
# 
# colnames(viol_add1) <- new_names_viol_add1

new_names_viol_add2 <- c("ViolationID", "borough", "house_number", "street", "zipcode",
                         "violation_class", "inspection_date", "BIN_old", "BIN_new",
                         "x", "y")

colnames(viol_add2) <- new_names_viol_add2

junkbins=c(1000000,2000000,3000000,4000000,5000000)

viol_updated=left_join(violations,viol_add2 %>% select(BIN_new, ViolationID)) %>%
  mutate(BIN=ifelse(!is.na(BIN_new), BIN_new, BIN)) %>%
  filter(BIN %in% junkbins==F & !is.na(BIN))
                       
                       
                       #commented this out because the process is a bit different now that we are merging
                       #not weird bins to newly geocoded data, then summarizing
                       #getting all data into same row and column format

#first adding dates back into _add1
# 
# violation_dates <- violations %>% 
#   select(ViolationID, InspectionDate)
# 
# viol_add1_uniform <- viol_add1 %>%
#   left_join(violation_dates, by = join_by(violation_ID == ViolationID)) %>%
#   mutate(inspection_date = mdy(InspectionDate)) %>%
#   select(violation_ID, borough, house_number, street, zipcode, inspection_date, 
#         violation_class, BIN_new, x, y) %>%
#   rename(BIN = BIN_new)
# 
# viol_add2_uniform <- viol_add2 %>%
#   mutate(inspection_date = mdy(inspection_date)) %>%
#   select(violation_ID, borough, house_number, street, zipcode, inspection_date, 
#          violation_class, BIN_new, x, y) %>%
#   rename(BIN = BIN_new)
# 
# viol_bins_uniform <- viol_bins %>%
#   mutate(inspection_date = mdy(inspection_date)) %>%
#   select(violation_ID, borough, house_number, street, zipcode, inspection_date, 
#          violation_class, BIN, x, y) %>%
#   mutate(x = as.integer(x),
#          y = as.integer(y))
# 
# 
# #binding rows into one dataset
# violations_full <- bind_rows(viol_bins_uniform, viol_add1_uniform, 
#                             viol_add2_uniform) 
# 
# violations_full %>%
#   group_by(BIN) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# #junk bins no longer in the top!
# 
# 
# viol_by_year <- viol_updated  %>%
#   mutate(inspec_year = year(inspection_date)) %>%
#   group_by(BIN, inspec_year) %>%
#   summarise(n = n()) %>%
#   filter(inspec_year != 2022) %>%
#   pivot_wider(names_from = inspec_year, values_from = n) %>%
#   rename(viol_2023 = `2023`,
#          viol_2024 = `2024`,
#          viol_2025 = `2025`) %>%
#   mutate(across(c(viol_2025:viol_2024), ~ replace_na(.x, 0)),
#          n_viol = sum(viol_2023, viol_2024, viol_2025)) %>%
#   select(BIN, n_viol, viol_2023, viol_2024, viol_2025)

viol_class_year=
  viol_updated %>%
  mutate(inspec_year = year(inspection_date)) %>%
  group_by(BIN, Class, inspec_year) %>% 
  summarise(n=n())

for(i in 2023:2025){
assign(paste0('viol',i), 
       dcast(viol_class_year %>% filter(inspec_year==i), 
             BIN ~ paste0(Class,'_',i), value.var = 'n', fill=0))
       
}

viol2023$viol_2023=viol2023$A_2023+viol2023$B_2023+viol2023$C_2023+viol2023$I_2023
viol2024$viol_2024=viol2024$A_2024+viol2024$B_2024+viol2024$C_2024+viol2024$I_2024
viol2025$viol_2025=viol2025$A_2025+viol2025$B_2025+viol2025$C_2025+viol2025$I_2025

viol_addresses=viol_updated %>% 
  distinct(BIN, .keep_all=T) %>%
  select(BIN, HouseNumber, StreetName, Borough, Postcode)

#join all summary data by BIN
viol_all=full_join(viol2023, viol2024) %>%
  full_join(.,viol2025)%>%
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


