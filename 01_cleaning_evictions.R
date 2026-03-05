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

evictions_path <- "/Users/nataliebrown/Desktop/housing_quality_nyc/evictions/"
export_path <- "/Users/nataliebrown/Desktop/housing_quality_nyc/outputs/to_be_geocoded" 

#DATA##########################################################################

#import
evictions_2019_2021 <- read.csv(paste0(evictions_path, 'evictions_2019_2021.csv'))
evictions_2022_2025 <- read.csv(paste0(evictions_path, 'evictions_2022_2025.csv'))

#bind rows 

rm(evictions)
evictions_raw <- bind_rows(evictions_2019_2021, evictions_2022_2025)

evictions_distinct <- evictions_raw %>% distinct() #there are __ duplicates

#DATA CLEANING#################################################################
#removing commercial evictions and names for Marshals
evictions_res <- evictions_raw %>%
  filter(Residential.Commercial == "Residential") %>% 
  select(-c(Marshal.First.Name, Marshal.Last.Name, Residential.Commercial)) %>%
  mutate(evic_date = as.Date(Executed.Date, "%m/%d/%Y"),
         evic_year = year(evic_date))

#quick check of evictions per building
evictions_res %>%
  group_by(BIN) %>%
  summarise(n_evic = n()) %>%
  arrange(desc(n_evic))
#some bins are fillers for the borough but not attached to the building itself, need to adjust before matching 

#isolating junk bins
junk_bin <- c(1000000, 2000000, 3000000, 4000000, 5000000)

junk_bins <- evictions_res %>%
  filter(BIN %in% junk_bin | is.na(BIN)) %>%
  mutate(address = Eviction.Address) %>%
  separate(col = address,
           into = c("house_number", "street"),
           sep = " ",
           extra = "merge")

#isolating the evictions that already have BINs
not_junk_bins <- evictions_res %>%
filter(!BIN %in% junk_bin & !is.na(BIN))

#ADDRESS CLEANING FOR GEOCODING TO GET BIN

#remove punctuation from street field
junk_bins$street = gsub("[[:punct:]]", '', junk_bins$street)

#trim leading and ending whitespace
junk_bins$street=trimws(junk_bins$street)

#remove non-numeric values from the house number field.
junk_bins$house_number=gsub('[^0-9.-]','',junk_bins$house_number)

#cleaning specific data issues 
#remomving excess address description
junk_bins$street <- gsub(
  "\\s+(NO\\.|APT\\.?|APARTMENT|UNIT|BLDG|BUILDING|FL[R]?|FLOOR|RM|ROOM|(1ST|2ND|3RD|FIRST|SECOND|THIRD)\\s+FL[R]?)\\b.*",
  "", junk_bins$street, perl = TRUE
)
junk_bins$street <- gsub(
  "\\s*(ENTIRE|CLOSET|GROUND|BASEMENT|DOOR|CONSISTING|LIVING|ALL ROOMS|ALL COMMON AREAS|FRONT ENTRANCE|ROOMS|PARKINGSPACE|LOWER LEVEL|A PT).*",
  "", junk_bins$street, perl = TRUE
)

junk_bins$street <- gsub(
  "\\s*(COMMERCIAL SPACE|TOP F|ON THE|STUDIO|LARGE BEDROOM|ALL SPACE|GRDFLR|REAR|LEFT|BEDROOM|ALL RMS|MAKE RIGHT|THE PREMISES|BSMT|RETAIL).*",
  "", junk_bins$street, perl = TRUE
)

junk_bins$street <- gsub(
  "\\s*(GARAGE|TOP F|ON THE|STUDIO|UPPER|1ST FLR|SECOND FLOOR|ONE FAMILY|2ND FLOOR|TOP LEVEL|ATTIC|2ND FLRAPT|WHOLE HOUSE|3RD FLOOR|CELLAR).*",
  "", junk_bins$street, perl = TRUE
)

#removing commas, parentheses, AKAs, and any info following them
junk_bins$street <- gsub("\\(.*", "", junk_bins$street, perl = TRUE)
junk_bins$street <- gsub(",.*",   "", junk_bins$street, perl = TRUE)
junk_bins$street <- gsub("\\s*A ?K ?A.*", "", junk_bins$street, perl = TRUE)

#replace split words and remove abbreviations
word_fixes <- c(
  "0CEAN" = "OCEAN",
  "C OURT|CO URT|COU RT|COUR T" = "COURT",
  "SEC OND" = "SECOND",  
  "S TREET|ST REET|STR EET|STRE ET|STREE T|ST RT" = "STREET",
  "P LACE|PL ACE|PLA CE|PLAC E"             = "PLACE",
  "W EST|WE ST|WES T"                        = "WEST",
  "E AST| EAS T" = "EAST",
  "S OUTH|SO UTH|SOU TH|SOUT H"             = "SOUTH",
  "STJOHNS" = "ST JOHNS",
  "EMOSHOLU" = "EAST MOSHOLU",
  "A VENUE|AV ENUE|AVE NUE|AVEN UE|AVENU E|AVEU E" = "AVENUE",
  "R OAD|RO AD|ROA D"                        = "ROAD",
  "B OULEVARD|BO ULEVARD|BOU LEVARD|BOUL EVARD|BOULE VARD|BOULEV ARD|BOULEVA RD|BOULEVAR D" = "BOULEVARD",
  "P ARKWAY|PA RKWAY|PAR KWAY|PARK WAY|PARKW AY|PARKWA Y" = "PARKWAY",
  "T ERRACE|TE RRAACE|TER RACE|TERR ACE|TERRA CE|TERRAC E| TER R" = "TERRACE",
  "HUTCH RV" = "HUTCHINSON RIVER",
  "C HANNEL|CH ANNEL|CHA NNEL|CHAN NEL|CHANN EL|CHANNE L" = "CHANNEL",
  "C RESCENT|CR ESCENT|CRE SCENT|CRES CENT|CRESC ENT|CRESCE NT|CRESCEN T" = "CRESCENT",
  "PENNSYLVAN IA" = "PENNSYLVANIA",
  "P OLITE|PO LITE|POL ITE|POLI TE|POLIT E" = "POLITE",
  "S QUARE| SQ UARE|SQU ARE|SQUA RE|SQUAR E" = "SQUARE",
  "C ONCOURSE|CO NCOURSE|CON COURSE|CONC OURSE|CONCO URSE|CONCOU RSE|CONCOUR SE|CONCOURS E" = "CONCOURSE",
  "P ROMENADE|PR OMENADE|PRO MENADE|PROM ENADE|PROME NADE|PROMEN ADE|PROMENA DE|PROMENAD E" = "PROMENADE",
  "L AFAYETTE|LA FAYETTE|LAF AYETTE|LAFA YETTE|LAFAY ETTE|LAFAYE TTE|LAFAYET TE|LAFATETT E" = "LAFAYETTE",
  "K INGSBRIGE|KI NGSBRIDGE|KIN GSBRIDGE|KING SBRIDGE|KINGS BRIDGE|KINGSB RIDGE|KINGSBR IDGE|KINGSBRI DGE|KINGSBRID GE|KINGSBRIDG E" = "KINGSBRIDGE",
  "CHESTNU T" = "CHESTNUT",
  "LAFAYETT E" = "LAFAYETTE",
  
  # Abbreviations → full word
  "\\bBLVD\\b|\\bBL VD\\b|\\bB LVD\\b"  = "BOULEVARD",
  "\\bPKWAY\\b|\\bPKWY\\b"  = "PARKWAY",
  
  # Specific street name typos
  "\\bBOYL AND\\b|\\bBBOYLAND\\b" = "BOYLAND",
  "\\bPO WELL\\b"                  = "POWELL",
  "\\bBBOULEVARD\\b"               = "BOULEVARD"
)

junk_bins$street <- str_replace_all(junk_bins$street, word_fixes)

#additional whitespace cleanup
junk_bins$street <- trimws(gsub("\\s+", " ", junk_bins$street, perl = TRUE))

junk_bins$street <- gsub("KINGSBRIDGETERRACEACE", "KINGSBRIDGE TERRACE", junk_bins$street, perl = TRUE)
junk_bins$street <- gsub("KINGSBRIDGETERRACE", "KINGSBRIDGE TERRACE", junk_bins$street, perl = TRUE)

#writing datasets for geocoding export 
write.csv(junk_bins, "junk_bins_cleaned.csv", row.names = F)

#BRINGING BACK CORRECTED BINS FOR DATA AGGREGATION#############################

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
#write.csv(evic_output, paste0(data.path,'evic_by_year_20feb2026.csv'), row.names = F)


