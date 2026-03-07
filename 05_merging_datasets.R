###############################################################################
### PROGRAM NAME:  EVICTIONS - 2026                                         ###
### DESCRIPTION:   Merging All Datasets Together                            ###
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

#DATA PATHS#####################################################################

data.path <- '/Users/nataliebrown/Desktop/housing_quality_nyc/outputs/summary_tables/'
output.path <- '/Users/nataliebrown/Desktop/housing_quality_nyc/outputs/final_df/'

#BRINGING DATA IN###############################################################

violations <- read_csv(paste0(data.path, 'viol_by_year_6mar2026.csv')) %>% rename(Year = viol_year)
evictions <- read_csv(paste0(data.path, 'evic_by_year_6mar2026.csv')) %>% select(-1)
hpdjuris <- read_csv(paste0(data.path, 'hpdjuris_by_BIN.csv'))
buildings <- read_csv(paste0(data.path, 'BUILDING_20260306.csv')) %>% select(BIN, 'Construction Year')

#charges_invoices <- read_csv(paste0(data.path, 'erp_by_year.csv'))

#MERGING########################################################################

#merging in evictions
master_diff_n_diff <- full_join(violations, evictions, by = join_by(BIN, Year))
sum(is.na(master_diff_n_diff$BIN))


match1_qa=
  master_diff_n_diff %>%
  group_by(BIN, Year) %>%
  summarise(n=n())
#no years with multiple BINs - merging violations and evictions was successful

#merging in hpdjuris 
master_diff_n_diff_juris <- left_join(master_diff_n_diff, hpdjuris, by = "BIN") %>%
  select(-c(full_address, Zip)) %>%
  mutate(Boro = case_when(substr(BIN, 1, 1) == "1" ~ "MANHATTAN",
                          substr(BIN, 1, 1) == "2" ~ "BRONX",
                          substr(BIN, 1, 1) == "3" ~ "BROOKLYN",
                          substr(BIN, 1, 1) == "4" ~ "QUEENS",
                          substr(BIN, 1, 1) == "5" ~ "STATEN ISLAND")) 

#rows missing units have lower violations and evictions, likely because they're 
#smaller

master_diff_n_diff_juris %>%
  mutate(missing_units = is.na(n_units)) %>%
  group_by(missing_units) %>%
  summarise(
    mean_evictions = mean(Evictions, na.rm = TRUE),
    mean_violations = mean(viol_total, na.rm = TRUE),
    n = n()
  )

master_diff_n_diff_juris <- master_diff_n_diff_juris %>%
  filter(!is.na(n_units) | n_units != 0)
#exclude 0 units because those are demos, vacant, nonresidential, or have had vacate orders
#exclude NA because they likely have less than 3 units and are not required to report n_units

master_diff_n_diff_final <- master_diff_n_diff_juris %>%
  left_join(buildings, by = "BIN") %>%
  rename(construction_year = 'Construction Year')

master_diff_n_diff_final %>%
  mutate(missing_year = is.na(construction_year)) %>%
  group_by(missing_year) %>%
  summarise(
    mean_evictions = mean(Evictions, na.rm = TRUE),
    mean_violations = mean(viol_total, na.rm = TRUE),
    n = n()
  )
  

na_counts <- colSums(is.na(master_diff_n_diff_final))
print(na_counts) 

#charges_invoices_qa=
#  charges_invoices %>%
#  group_by(BIN, year) %>%
#  summarise(n=n())%>%
#  filter(n>1)

#merging in charges_invoices
#df2 <- full_join(df1, charges_invoices, by = "BIN") %>%
#  rename(full_address_charge_invoice = full_address,
#         borough_charge_invoice = Boro,
#         zip_charge_invoice = Zip)

#sum(is.na(df2$BIN))

#QA to ensure that there is one line per BIN

#df2_bin_qa=
#  df2%>%
#  group_by(BIN) %>%
#  summarise(n=n()) %>%
#  filter(n>1)
#merging ERP was successful - one line per BIN




master_dataset[is.na(master_dataset)] <- 0

#QA completeness
master_summ=
  skim(master_dataset)

#QA one line per BIN
master_bin_QA=
  master_dataset%>%
  group_by(BIN)%>%
  summarise(n=n())%>%
  filter(n>1)

#QA no repeat addresses 

write.csv(master_dataset, paste0(output.path,'master_dataset.csv'), row.names=F)

