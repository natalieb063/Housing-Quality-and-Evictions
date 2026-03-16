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

violations <- read_csv(paste0(data.path, 'viol_by_year_13mar2026.csv')) %>% rename(Year = viol_year)
evictions <- read_csv(paste0(data.path, 'evic_by_year_15mar2026.csv')) %>% select(-1)
hpdjuris <- read_csv(paste0(data.path, 'hpdjuris_by_BIN.csv'))
buildings <- read_csv(paste0(data.path, 'BUILDING_20260306.csv')) %>% select(BIN, 'Construction Year')

#charges_invoices <- read_csv(paste0(data.path, 'erp_by_year.csv'))

#MERGING########################################################################

#merging in evictions
master_diff_n_diff <- left_join(violations, evictions, by = join_by(BIN, Year))
sum(is.na(master_diff_n_diff$BIN))


match1_qa=
  master_diff_n_diff %>%
  group_by(BIN, Year) %>%
  summarise(n=n())
#no years with multiple BINs - merging violations and evictions was successful

#merging in hpdjuris 
master_diff_n_diff_juris <- left_join(master_diff_n_diff, hpdjuris, by = "BIN") %>%
  select(-c(full_address, Zip)) %>%
  mutate(Boro = case_when(substr(BIN, 1, 1) == "1" ~ "MANHATTAN", #adding in boro where missing
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

#adding in construction year to get building age
master_diff_n_diff_construct <- master_diff_n_diff_juris %>%
  left_join(buildings, by = "BIN") %>%
  rename(construction_year = 'Construction Year')

#buildings missing construction year have lower violations than those that are
#not missing, evictions for these buildings are not violation driven
master_diff_n_diff_construct %>%
  mutate(missing_year = is.na(construction_year)) %>%
  group_by(missing_year) %>%
  summarise(
    mean_evictions = mean(Evictions, na.rm = TRUE),
    mean_violations = mean(viol_total, na.rm = TRUE),
    n = n()
  )
  
#564 buildings were demolished or constructed mid-sample
#

demolished <- master_diff_n_diff_construct %>% # 168 demolished
  group_by(BIN) %>%
  summarise(min_violation_year = min(Year, na.rm = TRUE),
             construction_year = first(construction_year)) %>%
  filter(construction_year > min_violation_year) %>%
  pull(BIN)

#4702 buildings built after 2019
master_diff_n_diff_construct %>% filter(construction_year > 2019)

#removing demolished and built too late
master_diff_n_diff_full <- master_diff_n_diff_construct %>%
  filter(!BIN %in% demolished,
         construction_year <= 2019)

n_distinct(master_diff_n_diff_full$BIN) #197556 unique BINs

#checking that all of our fixed effects are present, OK if there are NAs in our
#violations and evictions
na_counts <- colSums(is.na(master_diff_n_diff_full))
print(na_counts) 

#COMPLETING THE PANEL###########################################################

all_bins <- master_diff_n_diff_full %>% distinct(BIN)
all_years <- tibble(Year = 2017:2025)

panel <- all_bins %>% cross_join(all_years)

nrow(panel) == n_distinct(master_diff_n_diff_full$BIN) * 9

panel <- panel %>%
  left_join(violations, by = c("BIN", "Year")) %>%
  left_join(evictions, by = c("BIN", "Year")) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  left_join(buildings, by = "BIN") %>%
  left_join(hpdjuris, by = "BIN")

skim(panel)

#avg annual violations in primary treatment window (2017-2019)

pre_period <- panel %>%
  filter(Year >= 2017 & Year <= 2019) %>%
  group_by(BIN) %>%
  summarise(
    avg_pre_violations = mean(viol_total, na.rm = TRUE),
    .groups = "drop"
  )

median_violations <- median(pre_period$avg_pre_violations)

pre_period <- pre_period %>%
  mutate(treated = if_else(avg_pre_violations > median_violations, 1, 0))

panel <- panel %>%
  left_join(pre_period, by = "BIN")

panel <- panel %>%
  mutate(
    post           = if_else(Year >= 2022, 1, 0),
    moratorium     = if_else(Year %in% c(2020, 2021), 1, 0),
    treated_x_post = treated * post,
    year_factor    = relevel(factor(Year), ref = "2019"),
    building_age   = 2017 - `Construction Year`
  )

#QA############################################################################

panel %>%
  count(BIN) %>%
  summarise(
    min_years = min(n),
    max_years = max(n)
  )

panel %>%
  group_by(BIN) %>%
  summarise(treatment_values = n_distinct(treated)) %>%
  filter(treatment_values > 1)

panel %>%
  filter(Year %in% c(2017, 2018, 2019)) %>%
  group_by(treated) %>%
  summarise(mean_evictions = mean(Evictions, na.rm = TRUE))

panel %>%
  filter(Year >= 2022) %>%
  group_by(treated) %>%
  summarise(mean_evictions = mean(Evictions, na.rm = TRUE))

panel %>%
  filter(Year %in% c(2017, 2018, 2019)) %>%
  group_by(Year, treated) %>%
  summarise(mean_evictions = mean(Evictions, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = treated, 
              values_from = mean_evictions,
              names_prefix = "treated_") %>%
  mutate(difference = treated_1 - treated_0)



#FOR LATER#####################################################################
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

