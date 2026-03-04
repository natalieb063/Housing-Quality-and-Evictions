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

data.path=
output.path=

#BRINGING DATA IN###############################################################

violations <- read_csv(paste0(data.path, 'viol_by_year_final.csv'))
evictions <- read_csv(paste0(data.path, 'evic_by_year_final.csv'))
hpdjuris <- read_csv(paste0(data.path, 'hpdjuris_by_BIN.csv'))
charges_invoices <- read_csv(paste0(data.path, 'erp_by_year.csv'))

#MERGING########################################################################

violations <- violations %>%
  mutate(full_address = case_when(HouseNumber == 0 ~ paste0(StreetName),
                                  HouseNumber != 0 ~ paste0(HouseNumber, " ", StreetName))) %>%
  select(-c(HouseNumber, StreetName))

#merging in evictions
df1 <- full_join(violations, evictions, by = "BIN") %>% 
  rename(full_address_viol = full_address.x,
         full_address_evic = full_address.y,
         borough_viol = Borough,
         borough_evic = borough,
         zip_viol = Postcode,
         zip_evic = zipcode)

sum(is.na(df1$BIN))

df1_bin_qa=
  df1%>%
  group_by(BIN) %>%
  summarise(n=n()) %>%
  filter(n>1)

#merging violations and evictions was successful - one line per BIN
charges_invoices_qa=
  charges_invoices %>%
  group_by(BIN) %>%
  summarise(n=n())%>%
  filter(n>1)

#merging in charges_invoices
df2 <- full_join(df1, charges_invoices, by = "BIN") %>%
  rename(full_address_charge_invoice = full_address,
         borough_charge_invoice = Boro,
         zip_charge_invoice = Zip)

sum(is.na(df2$BIN))

#QA to ensure that there is one line per BIN

df2_bin_qa=
  df2%>%
  group_by(BIN) %>%
  summarise(n=n()) %>%
  filter(n>1)
#merging ERP was successful - one line per BIN

#now that we have a dataset with all of the indicators, we can left join 
# hpd juris

#merging in hpdjuris 
# df3 <- full_join(df2, hpdjuris, by = "BIN") %>%
#   rename(full_address_hpd_juris = full_address,
#          borough_hpd_juris = Boro,
#          zip_hpd_juris = Zip)

df3 <- left_join(df2, hpdjuris, by = "BIN") %>%
  rename(full_address_hpd_juris = full_address,
         borough_hpd_juris = Boro,
         zip_hpd_juris = Zip)

na_counts <- colSums(is.na(hpdjuris))
print(na_counts) #16 rows with no zip in raw hpdjuris data

sum(is.na(df3$BIN))
sum(is.na(df3$n_units)) #not 0, so some units do not have # of units from hpd

#picking addresses

master_dataset <- df3 %>%
  mutate(full_address = coalesce(full_address_viol, full_address_hpd_juris, 
                                 full_address_charge_invoice, full_address_nycha,
                                 full_address_aep, full_address_evic),
         zipcode = coalesce(zip_viol, zip_hpd_juris, zip_charge_invoice, 
                            zip_nycha, zip_aep, zip_evic),
         borough = coalesce(borough_viol, borough_hpd_juris, borough_charge_invoice, 
                            borough_nycha, borough_aep, borough_evic)) %>%
  select(BIN, full_address, zipcode, borough, A_2023:viol_2025, 
         n_evic:total_dollars_invoiced, n_units, is_AEP, n_units_aep, is_nycha) 

master_dataset %>% filter(is.na(zipcode))

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

