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
evictions_2017_2018 <- read.csv(paste0(evictions_path, 'evictions_2017_2019.csv')) %>% #remove all 2019s
  mutate(evic_date = as.Date(Executed.Date, "%m/%d/%Y"),
         evic_year = year(evic_date)) %>%
  filter(evic_year != 2019)

evictions_2019_2021 <- read.csv(paste0(evictions_path, 'evictions_2019_2021.csv'))
evictions_2022_2025 <- read.csv(paste0(evictions_path, 'evictions_2022_2025.csv'))

#bind rows 
evictions_raw <- bind_rows(evictions_2017_2018, evictions_2019_2021, evictions_2022_2025) %>% distinct()
#115962 evictions in raw data

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

junk_bins$street <- gsub(
  "\\s*(DUPLEX|MAIN ENTRANCE|2ND FLAPT|DOWNSTAIRS| WALK IN|LOWER|BSMT).*",
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
  "N ORTH| NO RTH| NOR TH| NORT H" = "NORTH",
  "STJOHNS" = "ST JOHNS",
  "L EWIS| LE WIS| LEW IS| LEWI S" = "LEWIS",
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
  "E XPRESSWAY|EX PRESSWAY|EXP RESSWAY|EXPR ESSWAY|EXPRES SWAY|EXPRESS WAY|EXPRESSW AY|EXPRESSWA Y"  = "EXPRESSWAY",
  
  #abbreviations → full word
  "\\bBLVD\\b|\\bBL VD\\b|\\bB LVD\\b"  = "BOULEVARD",
  "\\bPKWAY\\b|\\bPKWY\\b"  = "PARKWAY",
  
  #specific street name typos
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

geocoding_folder <- "/Users/nataliebrown/Desktop/housing_quality_nyc/outputs/geocoded/"

evictions_by_add <- fread(paste0(geocoding_folder, 'junk_bins_cleaned_V2_SUCCESS.csv')) %>%
  select(-c(V1, V4:V5, V9:V15, V17:V19))

new_names_by_add<- c("court_index", "docket_number", "eviction_date", "borough",
                    "zipcode", "BIN_old", "evic_year", "house_number", 
                     "street", "BIN_new")

colnames(evictions_by_add) <- new_names_by_add

#standardizing datasets for combination

evictions_ungeocoded <- not_junk_bins %>%
  rename(court_index = Court.Index.Number,
         docket_nuber = Docket.Number.,
         borough = BOROUGH,
         zipcode = Eviction.Postcode) %>% 
  separate(col = Eviction.Address,
           into = c("house_number", "street"),
           sep = " ",
           extra = "merge") %>%
  select(BIN, house_number, street, borough, zipcode, evic_year)

evic_geocoded_by_add <- evictions_by_add %>%
  mutate(BIN_new = as.numeric(BIN_new)) %>%
  filter(!is.na(BIN_new)) %>% #removes 11 
  select(BIN_new, house_number, street, borough, zipcode, evic_year) %>%
  rename(BIN = BIN_new) %>%
  mutate(BIN = as.integer(BIN),
         house_number = as.character(house_number))

#combining evictions with and without BINs
evictions_combined <- bind_rows(evictions_ungeocoded, evic_geocoded_by_add) %>%
  mutate(row_id = row_number())

evictions_combined %>% group_by(BIN) %>% summarise(n = n()) %>% arrange(desc(n))

#creating summary statistics
evic_by_year <- evictions_combined  %>%
  group_by(BIN, evic_year) %>%
  summarise(Evictions = n()) %>%
  rename(Year = evic_year)

#exporting summary tables
write.csv(evic_by_year, 'evic_by_year_6mar2026.csv')


