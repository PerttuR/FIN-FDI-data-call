#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa, Anna-Kaisa Ylitalo, Antti Sykkö
#
# Date: JUN-2018
# Updated: JUN 2021 by Perttu
#          JUN-2022 by Anna-Kaisa and Perttu
#          MAY-2023 by Antti
#          JUN-2024 by Mira, Petri and Perttu
#          JUN-2025 by Joanne, Petri and Perttu
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is to prepare FDI data tables A, G, H, I and J       ####
#-------------------------------------------------------------------------------


# install.packages("RPostgreSQL")
# install.packages("dplyr")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)
library(magrittr)
library(openxlsx)
library(icesVocab)
library(RPostgres)
library(tidyr)
library(lubridate)
library(lubridate)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------

run.year = 2025

# Output folder
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep, run.year)
path_der <- paste0(getwd(), .Platform$file.sep, "der/", run.year,"/")
path_orig <- paste0(getwd(), .Platform$file.sep, "orig/")

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                       1. Import data for 2016-2023 A, G, H, I and J table needs   ####                 
#-------------------------------------------------------------------------------

#----------- set years ---------------
#years <- seq(2016, run.year-1)
years <- seq(2013, run.year-1)


#----------- get data ---------------

source("db.r")

## Read in data
### JCD: get newest postgres schema date ####
table.list <- list.dbTable("kake_siirto")[,1] |> as.character(table)
table.list <- substr(table.list, 30, nchar(table.list)-3)
table.dates <- sort(unique(substr(table.list,1,10)))
# only keep dates
table.dates <- grep("\\d{4}-\\d{2}-\\d{2}", table.dates, value=TRUE)

# output choice here ####
schemadate <- max(table.dates)
message("Newest schema is from: ", schemadate)

# find the correct table name and date
tbl.list <- list.dbTable.tbl(dbname = "kake_siirto", schema=paste0(schemadate, "-dcprod"))
tag <- grep("kalastusaktiviteetti", tbl.list$table)
tablename <- tbl.list$table[tag]
class(tablename) <- "character"
tablename <- unlist(strsplit(tablename, '"'))
tablename <- tablename[length(tablename)-1]

# Postgres (used with A, G, H and I)
message(paste("Reading schema:", paste0(schemadate, "-dcprod")))
message(paste("Reading table:", tablename))
              
aktiviteetti <- read.dbTable(schema=paste(schemadate, "-dcprod", sep = ""), 
                             table=tablename, dbname = "kake_siirto")

# find second table
tag <- grep("kapasiteetti", tbl.list$table)
tablename <- tbl.list$table[tag]
class(tablename) <- "character"
tablename <- unlist(strsplit(tablename, '"'))
tablename <- tablename[length(tablename)-1]

message(paste("Reading table:", tablename))

# (used with J)
kapasiteetti <- read.dbTable(schema=paste(schemadate, "-dcprod", sep = ""), 
                             table=tablename, dbname = "kake_siirto")

# TODO ask 2024 data from Pirkko /or Tapsa? to update the excel ####

# Discards excel
discards <- read.xlsx(paste0(path_orig, "Vaurioitetut lohet 2016-2024.xlsx"))
names(discards) <- toupper(names(discards))

#Table A years 2013-2015 - see import_sas_metier_2013-15.R
# tableA2013 <- read.xlsx(paste0(path_orig, "Table A 2013-2015.xlsx"), sheet = "_2013")
# tableA2014 <- read.xlsx(paste0(path_orig, "Table A 2013-2015.xlsx"), sheet = "_2014")
# tableA2015 <- read.xlsx(paste0(path_orig, "Table A 2013-2015.xlsx"), sheet = "_2015")
# 
# tableA1315 <- rbind(tableA2013, tableA2014, tableA2015)
# names(tableA1315) <- toupper(names(tableA1315))


#--------------------------

# Read in data from the correct year
aktiviteetti_all <- aktiviteetti %>% filter(KALASTUSVUOSI %in% years) %>% 
  mutate(kalastus_kk = format(PALUUPVM, "%m"), #get the correct month in order to make quarters later on
         kalastus_vuosi = format(PALUUPVM,"%Y"),
         kalastus_kk = case_when(
           kalastus_vuosi == as.character(as.numeric(KALASTUSVUOSI) - 1) ~ "01",  # Previous year, set to January
           kalastus_vuosi == as.character(as.numeric(KALASTUSVUOSI) + 1) ~ "12",  # Next year, set to December
           TRUE ~ kalastus_kk
         ))

# akt_2013_2015_headers <- c("YEAR", "ULKOINENTUNNUS", "KALASTUSPAIVAT", "MERIPAIVAT", "PAAKONETEHO", "VETOISUUS", "KALASTUSAIKAHH", "FT_REF", "VESSEL_LENGTH", "FISHING_TECH", "GEAR_TYPE", "TARGET_ASSEMBLAGE", "METIER", "SVT_KG_HER", "SVT_KG_SPR", "SVT_KG_COD", "SVT_KG_FLE", "SVT_KG_TUR", "SVT_KG_PLN", "SVT_KG_SAL", "SVT_KG_TRS", "SVT_KG_SME", "SVT_KG_FBM", "SVT_KG_FID", "SVT_KG_FRO", "SVT_KG_FPI", "SVT_KG_FPE", "SVT_KG_FPP", "SVT_KG_FBU", "SVT_KG_TRR", "SVT_KG_FVE", "SVT_KG_ELE", "SVT_KG_FIN", "SVT_KG_TOTAL", "SVT_VALUE_HER", "SVT_VALUE_SPR", "SVT_VALUE_COD", "SVT_VALUE_FLE", "SVT_VALUE_TUR", "SVT_VALUE_PLN", "SVT_VALUE_SAL", "SVT_VALUE_TRS", "SVT_VALUE_SME", "SVT_VALUE_FBM", "SVT_VALUE_FID", "SVT_VALUE_FRO", "SVT_VALUE_FPI", "SVT_VALUE_FPE", "SVT_VALUE_FPP", "SVT_VALUE_FBU", "SVT_VALUE_TRR", "SVT_VALUE_FVE", "SVT_VALUE_ELE", "SVT_VALUE_FIN", "RECTANGLE", "COUNTRY", "QUARTER", "MESH_SIZE_RANGE", "METIER_7", "SUPRA_REGION", "SUB_REGION", "EEZ_INDICATOR", "GEO_INDICATOR", "SPECON_TECH", "DEEP", "RECTANGLE_TYPE", "C_SQUARE")
# metier_sas_2013_2015_headers <- c("KALASTUSVUOSI", "ALUS", "TODO: from here: KALASTUSPAIVAT", "MERIPAIVAT", "PAAKONETEHO", "VETOISUUS", "KALASTUSAIKAHH", "FT_REF", "VESSEL_LENGTH", "FISHING_TECH", "GEAR_TYPE", "TARGET_ASSEMBLAGE", "METIER", "SVT_KG_HER", "SVT_KG_SPR", "SVT_KG_COD", "SVT_KG_FLE", "SVT_KG_TUR", "SVT_KG_PLN", "SVT_KG_SAL", "SVT_KG_TRS", "SVT_KG_SME", "SVT_KG_FBM", "SVT_KG_FID", "SVT_KG_FRO", "SVT_KG_FPI", "SVT_KG_FPE", "SVT_KG_FPP", "SVT_KG_FBU", "SVT_KG_TRR", "SVT_KG_FVE", "SVT_KG_ELE", "SVT_KG_FIN", "SVT_KG_TOTAL", "SVT_VALUE_HER", "SVT_VALUE_SPR", "SVT_VALUE_COD", "SVT_VALUE_FLE", "SVT_VALUE_TUR", "SVT_VALUE_PLN", "SVT_VALUE_SAL", "SVT_VALUE_TRS", "SVT_VALUE_SME", "SVT_VALUE_FBM", "SVT_VALUE_FID", "SVT_VALUE_FRO", "SVT_VALUE_FPI", "SVT_VALUE_FPE", "SVT_VALUE_FPP", "SVT_VALUE_FBU", "SVT_VALUE_TRR", "SVT_VALUE_FVE", "SVT_VALUE_ELE", "SVT_VALUE_FIN", "RECTANGLE", "COUNTRY", "QUARTER", "MESH_SIZE_RANGE", "METIER_7", "SUPRA_REGION", "SUB_REGION", "EEZ_INDICATOR", "GEO_INDICATOR", "SPECON_TECH", "DEEP", "RECTANGLE_TYPE", "C_SQUARE")


# akt_2013_2015_dataframe <- data.frame(akt_2013_2015_headers, metier_sas_2013_2015_headers)

# Select and mutate all the needed variables for tables H and I ####
# We seem to be missing "SVT_KG_PLE" and "SVT_KG_WHG"

akt1 <- aktiviteetti_all %>% 
  select(YEAR = KALASTUSVUOSI,
         ULKOINENTUNNUS,
         KALASTUSPAIVAT,
         MERIPAIVAT,
         PAAKONETEHO,
         VETOISUUS,
         KALASTUSAIKAHH,
         FT_REF, #trip id
         MONTH = kalastus_kk,
         VESSEL_LENGTH = VLENGTH_AER_NEW,
         FISHING_TECH = FT,
         GEAR_TYPE = GEAR,
         TARGET_ASSEMBLAGE = LEVEL5,
         MESH_SIZE = SILMAKOKO,
         PYYDYS,
         METIER,
         ICES,
         contains("SVT_KG_"),    # LE_KG_ if SVT_KG_ is missing ???
         contains("SVT_VALUE_"), # LE_VALUE_ if SVT_KG is missing ???
         -contains("SVT_KG_IND_"),
         -contains("SVT_KG_HUC_"),
         -contains("SVT_VALUE_HUC_"),
         -contains("SVT_VALUE_IND_"),
         RECTANGLE) %>% 
  mutate(
    COUNTRY = "FIN",
    QUARTER = case_when(
      MONTH %in% c("01","02","03") ~ 1,
      MONTH %in% c("04","05","06") ~ 2,
      MONTH %in% c("07","08","09") ~ 3,
      MONTH %in% c("10","11","12") ~ 4),
    TARGET_ASSEMBLAGE = case_when(
      TARGET_ASSEMBLAGE == "Small pelagic" ~ "SPF",
      TARGET_ASSEMBLAGE == "Activity missing" ~ "NK",
      TARGET_ASSEMBLAGE == "Freshwater" ~ "FWS",
      TARGET_ASSEMBLAGE == "Anadromous" ~ "ANA",
      TARGET_ASSEMBLAGE == "Finfish" ~ "FIF"),
    TARGET_ASSEMBLAGE = replace_na(TARGET_ASSEMBLAGE, "NK"),
    FISHING_TECH = case_when(
      is.na(FISHING_TECH) & (GEAR_TYPE == "LLS" | GEAR_TYPE == "LLD") ~ "PG",
      TRUE ~ FISHING_TECH
    ),
    MESH_SIZE_RANGE = case_when(
      FISHING_TECH == "TM" & MESH_SIZE < 16 ~ "00D16",
      FISHING_TECH == "TM" & 16 <= MESH_SIZE & MESH_SIZE < 32 ~ "16D32",
      FISHING_TECH == "TM" & 32 <= MESH_SIZE & MESH_SIZE < 90 ~ "32D90",
      FISHING_TECH == "TM" & 90 <= MESH_SIZE & MESH_SIZE < 105 ~ "90D105",
      FISHING_TECH == "TM" & 105 <= MESH_SIZE & MESH_SIZE < 110 ~ "105D110",
      FISHING_TECH == "TM" & 110 <= MESH_SIZE ~ "110DXX",
      FISHING_TECH == "PG" & MESH_SIZE < 16 ~ "00D16",
      FISHING_TECH == "PG" & 16 <= MESH_SIZE & MESH_SIZE < 32 ~ "16D32",
      FISHING_TECH == "PG" & 32 <= MESH_SIZE & MESH_SIZE < 90 ~ "32D90",
      FISHING_TECH == "PG" & 90 <= MESH_SIZE & MESH_SIZE < 110 ~ "90D110",
      FISHING_TECH == "PG" & 110 <= MESH_SIZE & MESH_SIZE < 157 ~ "110D157",
      FISHING_TECH == "PG" & 157 <= MESH_SIZE ~ "157DXX",
      #HUOM, jos silmakoko puuttuu (rannikkokalastus) niin laitetaan jaottelu Pirkon koodien mukaan
      is.na(MESH_SIZE) & PYYDYS %in% c(5, 16,17,18,19,20,21) ~ "16D32",
      is.na(MESH_SIZE) & PYYDYS %in% c(8,9,10,44,45,32) ~ "32D90",
      is.na(MESH_SIZE) & PYYDYS %in% c(11,12) ~ "90D110",
      is.na(MESH_SIZE) & PYYDYS == 13 ~ "110D157",
      is.na(MESH_SIZE) & PYYDYS == 22 ~ "157DXX",
      is.na(MESH_SIZE) & GEAR_TYPE %in% c("FPN", "FYK", "SSC") ~ "16D32",
      TRUE ~ "NK"),
    # add mesh size range to se for metier cleaning
    FROM = case_when(
      FISHING_TECH == "TM" & MESH_SIZE < 16 ~ 0,
      FISHING_TECH == "TM" & 16 <= MESH_SIZE & MESH_SIZE < 32 ~ 16,
      FISHING_TECH == "TM" & 32 <= MESH_SIZE & MESH_SIZE < 90 ~ 32,
      FISHING_TECH == "TM" & 90 <= MESH_SIZE & MESH_SIZE < 105 ~ 90,
      FISHING_TECH == "TM" & 105 <= MESH_SIZE & MESH_SIZE < 110 ~ 105,
      FISHING_TECH == "TM" & 110 <= MESH_SIZE ~  110,
      FISHING_TECH == "PG" & MESH_SIZE < 16 ~ 0,
      FISHING_TECH == "PG" & 16 <= MESH_SIZE & MESH_SIZE < 32 ~ 16,
      FISHING_TECH == "PG" & 32 <= MESH_SIZE & MESH_SIZE < 90 ~ 32,
      FISHING_TECH == "PG" & 90 <= MESH_SIZE & MESH_SIZE < 110 ~ 90,
      FISHING_TECH == "PG" & 110 <= MESH_SIZE & MESH_SIZE < 157 ~ 110,
      FISHING_TECH == "PG" & 157 <= MESH_SIZE ~ 157,
      #HUOM, jos silmakoko puuttuu (rannikkokalastus) niin laitetaan jaottelu Pirkon koodien mukaan
      is.na(MESH_SIZE) & PYYDYS %in% c(5, 16,17,18,19,20,21) ~ 16,
      is.na(MESH_SIZE) & PYYDYS %in% c(8,9,10,44,45,32) ~ 32,
      is.na(MESH_SIZE) & PYYDYS %in% c(11,12) ~ 90,
      is.na(MESH_SIZE) & PYYDYS == 13 ~ 110,
      is.na(MESH_SIZE) & PYYDYS == 22 ~ 157,
      is.na(MESH_SIZE) & GEAR_TYPE %in% c("FPN", "FYK", "SSC") ~ 16,
      TRUE ~ NA),
    TO = case_when(
      FISHING_TECH == "TM" & MESH_SIZE < 16 ~ 15,
      FISHING_TECH == "TM" & 16 <= MESH_SIZE & MESH_SIZE < 32 ~ 31,
      FISHING_TECH == "TM" & 32 <= MESH_SIZE & MESH_SIZE < 90 ~ 89,
      FISHING_TECH == "TM" & 90 <= MESH_SIZE & MESH_SIZE < 105 ~ 104,
      FISHING_TECH == "TM" & 105 <= MESH_SIZE & MESH_SIZE < 110 ~ 109,
      FISHING_TECH == "TM" & 110 <= MESH_SIZE ~ Inf,
      FISHING_TECH == "PG" & MESH_SIZE < 16 ~ 15,
      FISHING_TECH == "PG" & 16 <= MESH_SIZE & MESH_SIZE < 32 ~ 31,
      FISHING_TECH == "PG" & 32 <= MESH_SIZE & MESH_SIZE < 90 ~ 89,
      FISHING_TECH == "PG" & 90 <= MESH_SIZE & MESH_SIZE < 110 ~ 109,
      FISHING_TECH == "PG" & 110 <= MESH_SIZE & MESH_SIZE < 157 ~ 156,
      FISHING_TECH == "PG" & 157 <= MESH_SIZE ~ Inf,
      #HUOM, jos silmakoko puuttuu (rannikkokalastus) niin laitetaan jaottelu Pirkon koodien mukaan
      is.na(MESH_SIZE) & PYYDYS %in% c(5, 16,17,18,19,20,21) ~ 31,
      is.na(MESH_SIZE) & PYYDYS %in% c(8,9,10,44,45,32) ~ 89,
      is.na(MESH_SIZE) & PYYDYS %in% c(11,12) ~ 109,
      is.na(MESH_SIZE) & PYYDYS == 13 ~ 156,
      is.na(MESH_SIZE) & PYYDYS == 22 ~ Inf,
      is.na(MESH_SIZE) & GEAR_TYPE %in% c("FPN", "FYK", "SSC") ~ 31,
      TRUE ~ NA),
    ICES = case_when(
      ICES == 19 ~ 29, # bugi, korjataan myöhemmin dcprodiin!!
      TRUE ~ ICES
    ),
    METIER_7 = "NA",
    SUPRA_REGION = "NAO",
    SUB_REGION = paste("27.3.d.", ICES, sep = ""),
    EEZ_INDICATOR = "NA",
    GEO_INDICATOR = "NGI",
    SPECON_TECH = "NA",
    DEEP = "NA",
    RECTANGLE_TYPE = "05*1",
    C_SQUARE = "NA") %>% 
  select(-MONTH, -ICES, -MESH_SIZE, -PYYDYS) %>% mutate(
    SUB_REGION = case_when(
      SUB_REGION == "27.3.d.28" ~ "27.3.d.28.2", #Vaihtoehtoinen 28.1 ja 28.2 ja jälkimmäinen pääallas, jolla Suomi kalastaa
      TRUE ~ SUB_REGION
    )) #tehdään tarkaste myöhemmin!


# Create variables LATITUDE and LONGITUDE:
# .. define coordinates 
source("spatial.R")

midpoints <- latlon(akt1$RECTANGLE,midpoint=TRUE)

akt1 <- tibble::rowid_to_column(akt1, "ID")
midpoints <- tibble::rowid_to_column(midpoints, "ID")

akt1 <- left_join(akt1, midpoints,copy = TRUE)

akt1 <- akt1 %>% rename(LATITUDE = SI_LATI, LONGITUDE = SI_LONG) %>% select(-ID, RECTANGLE)

# Extract valid level 6 metiers 
valid_metiers <<- data.table::fread("https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv")$Metier_level6
valid_metiers <<- as.data.frame(valid_metiers)

# Check for any metiers that are not valid
metier <- akt1 %>% dplyr::select(METIER) %>% distinct()
metier_check <- metier %>% left_join(valid_metiers, by = c("METIER" = "valid_metiers"), keep = T)
missing <- metier_check %>% filter(is.na(valid_metiers))
print(missing)

# JCD: need to match SAS methodology
# Change the needed metiers into new format
# akt1 <- akt1 %>%  mutate(METIER = case_when(
#   METIER == "GNS_ANA_0_0_0" ~ "GNS_ANA_>0_0_0",         # typo
#   METIER == "GNS_SPF_16-109_0_0" ~ "GNS_SPF_32-89_0_0", # old code
#   METIER == "OTM_SPF_16-104_0_0" ~ "OTM_SPF_16-31_0_0", # old code
#   METIER == "PTM_SPF_16-104_0_0" ~ "PTM_SPF_16-31_0_0", # old code
#   METIER == "OTB_DEF_>=105_1_120" ~ "OTB_DEF_105-115_1_120", # old code
#   METIER == "OTM_DEF_>=105_1_120" ~ "OTM_DEF_105-115_1_120", # old code
#   TRUE ~ METIER))  # All other values remain unchanged

# clean metier categories based on METIER 5 and mesh size ranges
akt1  <- akt1 |> 
            mutate(metier6_orig=METIER,
                   METIER5 = stringr::str_sub(METIER, 1,7),
                   METIER = case_when(
              FROM == 0 ~ paste0(METIER5, "_<", TO+1, "_0_0"),
              TO == Inf ~ paste0(METIER5, "_>=", FROM, "_0_0"),
              FROM > 0 & TO != Inf ~ paste0(METIER5, "_", FROM, "-", TO+1, "_0_0"),
              MESH_SIZE_RANGE == "NK" | MESH_SIZE_RANGE == "NA" ~ paste0(METIER5,"_0_0_0")))

# akt1 |> count(metier6_orig) |> flextable() |> set_caption("before cleaning")                    
# akt1 |> count(METIER) |> flextable() |> autofit() |> set_caption("after cleaning") 

# AKT 1 BASIS FOR ALL FOLLOWING TABLES ####

akt1 |> count(MESH_SIZE_RANGE, FROM, TO, METIER) |> flextable()

#-------------------------------------------------------------------------------
#                   2. TABLE A (Catch summary)                              ####                
#-------------------------------------------------------------------------------

# add new 2013-15 table here

a <- akt1 %>% mutate(NEP_SUB_REGION = "NA") %>% select(-RECTANGLE,-RECTANGLE_TYPE, -LATITUDE, -LONGITUDE, -C_SQUARE, -MERIPAIVAT, -PAAKONETEHO, -VETOISUUS, -KALASTUSAIKAHH, -FT_REF)

# Pivot to longer format
a2 <- a %>%
  pivot_longer(cols = starts_with("SVT"), 
               names_to = c("type", "SPECIES"), 
               names_pattern = "SVT_(.+)_(.+)", 
               values_to = "value")%>%
  mutate(value = as.numeric(value),  # Ensure numeric type
         value = ifelse(is.na(value), as.numeric(0), value)) # Replace NAs with 0


# Handle duplicates by summarising and sum up the values of KG and VALUE (eur)
a3 <- a2 %>%
  group_by(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, METIER_7, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, NEP_SUB_REGION, SPECON_TECH, DEEP, type, SPECIES) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    n = n(),
    n2 = n_distinct(ULKOINENTUNNUS),
    .groups = 'drop'
  )

# Pivot to wider format and separate VALUE and KG
a4 <- a3 %>%
  pivot_wider(names_from = type, values_from = value)

# Rename columns and replace NA's with 0s
a5 <- a4 %>%
  rename(TOTVALLANDG = VALUE,
         TOTWGHTLANDG = KG)

# Transform the total weights into tonnes, add missing values and add the remaining variables
a6 <- a5 %>% mutate(
  TOTWGHTLANDG = TOTWGHTLANDG/1000,
  TOTWGHTLANDG = case_when(is.na(TOTWGHTLANDG) ~ 0,
                           TRUE ~ TOTWGHTLANDG),
  TOTVALLANDG = case_when(as.character(TOTVALLANDG) == 0 ~ "NK",
                          is.na(as.character(TOTVALLANDG)) ~ "NK",
                          TRUE ~ as.character(TOTVALLANDG))
)


# Remove the zero rows (no catch) and create the confidential column based on the number of observations
a7 <- a6 %>% filter(TOTWGHTLANDG > 0) %>% mutate(
  CONFIDENTIAL = case_when(
    n2 < 3 ~ "A",
    n2 >= 3 ~ "N"
  )) %>% select(-n2, -n)


# Create domain keys for landings and discards and discards variable
a8 <- a7 %>% mutate(
  GEAR = case_when(startsWith(GEAR_TYPE, "F") ~ "FPO-FPN-FYK",
                   GEAR_TYPE == "OTM" | GEAR_TYPE == "PTM" ~ "OTM-PTM",
                   SPECIES == "SPR" & (GEAR_TYPE == "GNS" | GEAR_TYPE == "FYK") ~ "GNS-FYK",
                   TRUE ~ GEAR_TYPE),
  DOMAIN_LANDINGS = paste0(COUNTRY, "_", # country
                           QUARTER, "_", # quarter
                           SUB_REGION, "_", # region
                           GEAR, "_", # gear type
                           "all_", # target assemblage
                           "all_", # mesh size range
                           "NA_", # selective device / metier
                           "NA_", # mesh size range of the selective device
                           "all_", # vessel length
                           SPECIES, "_", # species
                           "all" # commercial category
                           ),
  DOMAIN_DISCARDS = DOMAIN_LANDINGS
) %>% select(-GEAR)

# Remove observations (2016-2023 there are 8) that have missing TOTVALLANDG
a9 <- a8 %>% filter(TOTVALLANDG != "NK")


# -------------- 
# Add the discard data

discards2 <- discards %>% select(-MESH_SIZE_RANGE) %>% rename(DISCARDS = DISCARDS_KG) 

# Step 1: Add a row identifier in a9 to preserve row order
a9 <- a9 %>% mutate(row_id = row_number())

# Step 2: Sort by TOTWGHTLANDG in descending order and group by the join keys
a9_sorted <- a9 %>% arrange(YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, SUB_REGION, TARGET_ASSEMBLAGE, SPECIES, desc(TOTWGHTLANDG))

# Step 3: Create a flag for the first occurrence based on the highest TOTWGHTLANDG within each group
a9_first_occurrence <- a9_sorted %>% group_by(YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, SUB_REGION, TARGET_ASSEMBLAGE, SPECIES) %>%
  mutate(first_occurrence = row_number() == 1) %>%
  ungroup()

# Step 4: Perform the left join only on the first occurrences
a10_with_discard_matches <- a9_first_occurrence %>%
  filter(first_occurrence) %>%
  left_join(discards2, by = c("YEAR", "QUARTER", "VESSEL_LENGTH", "FISHING_TECH", "GEAR_TYPE", "SUB_REGION", "TARGET_ASSEMBLAGE", "SPECIES"))

# Step 5: Get rows from a9 that did not have a discard match (not first occurrences or those that didn't join)
a10_without_discard_matches <- a9_first_occurrence %>%
  filter(!first_occurrence) %>%
  mutate(across(starts_with("your_discard_columns"), ~NA))  # Replace 'your_discard_columns' with actual column names from discards

# Step 6: Combine matched and unmatched rows, then order by the original row order
a10 <- bind_rows(a10_with_discard_matches, a10_without_discard_matches) %>%
  arrange(row_id) %>%
  select(-row_id, -first_occurrence)  # Clean up helper columns if not needed

a11 <- a10 %>% mutate(DISCARDS = case_when(
  is.na(DISCARDS) ~ 0,
  TRUE ~ DISCARDS))

disc_joined <- a11 %>% filter(DISCARDS > 0)

# Step 1: Perform a left join from discards to a9 to check for non-matches
#discards_not_joined <- discards2 %>% left_join(a9, by = c("YEAR", "QUARTER", "VESSEL_LENGTH", "FISHING_TECH", "GEAR_TYPE", "SUB_REGION", "TARGET_ASSEMBLAGE", "SPECIES")) %>% filter(is.na(row_id)) 

# Export the filtered data to an Excel file
#write.xlsx(discards_not_joined, file = "discards_not_joined.xlsx")


#------------------------

# Add years 2013-2015

tableA20132015 <- tableA1315 %>% mutate(SUB_REGION = tolower(SUB_REGION),
                                        METIER_7 = "NA", 
                                        EEZ_INDICATOR = "NA",
                                        SPECON_TECH = "NA",
                                        DEEP = "NA",
                                        NEP_SUB_REGION = "NA",
       GEAR = case_when(startsWith(GEAR_TYPE, "F") ~ "FPO-FPN-FYK",
       GEAR_TYPE == "OTM" | GEAR_TYPE == "PTM" ~ "OTM-PTM",
       SPECIES == "SPR" & (GEAR_TYPE == "GNS" | GEAR_TYPE == "FYK") ~ "GNS-FYK",
       TRUE ~ GEAR_TYPE),
       DOMAIN_LANDINGS = paste0(COUNTRY, "_", # country
                           QUARTER, "_", # quarter
                           SUB_REGION, "_", # region
                           GEAR, "_", # gear type
                           "all_", # target assemblage
                           "all_", # mesh size range
                           "NA_", # selective device / metier
                           "NA_", # mesh size range of the selective device
                           "all_", # vessel length
                           SPECIES, "_", # species
                           "all" # commercial category
                           ),
  DOMAIN_DISCARDS = DOMAIN_LANDINGS
) %>% select(-GEAR, -METIER7)

# Check for any metiers that are not valid
metier2 <- tableA20132015 %>% dplyr::select(METIER) %>% distinct()
metier_check2 <- metier2 %>% left_join(valid_metiers, by = c("METIER" = "valid_metiers"), keep = T)
missing2 <- metier_check2 %>% filter(is.na(valid_metiers))
print(missing2)

# Change the needed metiers into new format and delete one missing value row
tableA20132015 <- tableA20132015 %>%  mutate(METIER = case_when(
  METIER == "MISSING" ~ "NK",
  METIER == "GNS_SPF_16-109" ~ "GNS_SPF_>=16_0_0",
  METIER == "GNS_SPF_16-109_0_0" ~ "GNS_SPF_>=16_0_0",
  METIER == "OTM_SPF_16-104_0_0" ~ "OTM_SPF_>0_0_0",
  METIER == "PTM_SPF_16-104_0_0" ~ "PTM_SPF_>0_0_0",
  METIER == "OTB_DEF_>=105_1_120" ~ "OTB_DEF_100-119_0_0",
  METIER == "OTM_DEF_>=105_1_120" ~ "OTM_DEF_100-119_0_0",
  TRUE ~ METIER)) %>% filter(!is.na(TOTVALLANDG))

tableA_all <- rbind(a11, tableA20132015) %>% 
  mutate(DISCARDS = DISCARDS/1000) 

#Rounding discards:
tableA_all$DISCARDS <- round(tableA_all$DISCARDS, digits = 3)      

#Rounding landings:
tableA_all$TOTWGHTLANDG <- round(tableA_all$TOTWGHTLANDG, digits = 3)

#Rounding value:
tableA_all$TOTVALLANDG <- round(as.numeric(tableA_all$TOTVALLANDG), digits = 0)
 
tableA_all <- tableA_all %>% 
  mutate (DISCARDS = case_when(
  DISCARDS == 0 ~ "NK",
  TRUE ~ as.character(DISCARDS)))


# Put the variables in the correct order:
table_A <- tableA_all %>% arrange(YEAR) %>% select(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, METIER_7, DOMAIN_DISCARDS, DOMAIN_LANDINGS, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, NEP_SUB_REGION, SPECON_TECH, DEEP, SPECIES, TOTWGHTLANDG,TOTVALLANDG, DISCARDS, CONFIDENTIAL)



# Write the resulting table A into der folder as rds file and into results folder
saveRDS(table_A, file = paste0(path_der,"table_A.rds"))

# Write the resulting table A
openxlsx::write.xlsx(table_A, paste0(path_out,.Platform$file.sep,"FIN_TABLE_A_CATCH.xlsx"), sheetName = "TABLE_A", colNames = TRUE, rowNames = FALSE)



#-------------------------------------------------------------------------------
#                   3. TABLE G (Effort summary)                             ####
#-------------------------------------------------------------------------------

# Select the variables not needed and needed in G table
g <- akt1 %>% select(-contains("SVT"), -RECTANGLE,-RECTANGLE_TYPE, -LATITUDE, -LONGITUDE, -C_SQUARE, 
                     TOTSEADAYS = MERIPAIVAT,
                     TOTFISHDAYS = KALASTUSPAIVAT,
                     HRSEA = KALASTUSAIKAHH) %>% mutate(
                       TOTKWDAYSATSEA = TOTSEADAYS*PAAKONETEHO,
                       TOTGTDAYSATSEA = TOTSEADAYS*VETOISUUS,
                       TOTKWFISHDAYS = TOTFISHDAYS*PAAKONETEHO,
                       TOTGTFISHDAYS = TOTFISHDAYS*VETOISUUS,
                       KWHRSEA = HRSEA*PAAKONETEHO,
                       GTHRSEA = HRSEA*VETOISUUS 
                     ) %>% select(-PAAKONETEHO, -VETOISUUS)

# Sum the total effort and calculate the number of distinct vessels in each strata
g2 <- g %>% group_by(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, METIER_7, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP) %>% summarise(
  TOTSEADAYS = sum(TOTSEADAYS, na.rm = TRUE),
  TOTKWDAYSATSEA = sum(TOTKWDAYSATSEA, na.rm = TRUE),
  TOTGTDAYSATSEA = sum(TOTGTDAYSATSEA, na.rm = TRUE),
  TOTFISHDAYS = sum(TOTFISHDAYS, na.rm = TRUE),
  TOTKWFISHDAYS = sum(TOTKWFISHDAYS, na.rm = TRUE),
  TOTGTFISHDAYS = sum(TOTGTFISHDAYS, na.rm = TRUE),
  HRSEA = sum(HRSEA, na.rm = TRUE),
  KWHRSEA = sum(KWHRSEA, na.rm = TRUE),
  GTHRSEA = sum(GTHRSEA, na.rm = TRUE),
  TOTVES = n_distinct(ULKOINENTUNNUS),
  .groups = 'drop') %>% mutate(
    CONFIDENTIAL = case_when(
      TOTVES < 3 ~ "Y",
      TOTVES >= 3 ~ "N"),
    # change null values into NK
    across(c(TOTSEADAYS, TOTKWDAYSATSEA, TOTGTDAYSATSEA, TOTFISHDAYS, TOTKWFISHDAYS, TOTGTFISHDAYS, HRSEA, KWHRSEA, GTHRSEA), ~ as.character(.))) %>% mutate(
      across(c(TOTSEADAYS, TOTKWDAYSATSEA, TOTGTDAYSATSEA, TOTFISHDAYS, TOTKWFISHDAYS, TOTGTFISHDAYS, HRSEA, KWHRSEA, GTHRSEA),~ replace_na(., "NK")))

# 0's in HRSEA, GTHRSEA or KWHRSEA
# missing hours and engine variables

tmp <- g2 |> filter(HRSEA == "0" | GTHRSEA == "0" | KWHRSEA == "0") |> tally() |> pull()
message(paste("0's in HRSEA, GTHRSEA or KWHRSEA: ",tmp))

# make it NK
g3 <- g2 |> mutate(HRSEA = if_else(HRSEA == "0", "NK", HRSEA),
                   GTHRSEA = if_else(GTHRSEA == "0", "NK", GTHRSEA),
                   KWHRSEA = if_else(KWHRSEA == "0", "NK", KWHRSEA))

tmp <- g3 |> filter(HRSEA == "0" | GTHRSEA == "0" | KWHRSEA == "0") |> tally() |> pull()
message(paste("0's in HRSEA, GTHRSEA or KWHRSEA: ",tmp))

# HRSEA, GTHRSEA or KWHRSEA exist, but other values are 0
tmp <- g3 |> filter(YEAR %in% seq(2016,2024)) |> 
  select(YEAR, QUARTER, VESSEL_LENGTH, GEAR_TYPE, SUB_REGION, TOTSEADAYS, TOTKWDAYSATSEA, TOTGTDAYSATSEA,
         TOTFISHDAYS, TOTKWFISHDAYS, TOTGTFISHDAYS, HRSEA, KWHRSEA, GTHRSEA, CONFIDENTIAL) |> 
  filter((TOTSEADAYS == "0" | TOTKWDAYSATSEA == "0" | TOTGTDAYSATSEA == "0" |
            TOTFISHDAYS == "0" | TOTKWFISHDAYS == "0" | TOTGTFISHDAYS == "0") & 
           ((HRSEA != "0" | GTHRSEA != "0" | KWHRSEA != "0") & 
              (HRSEA != "NK" | GTHRSEA != "NK" | KWHRSEA != "NK"))) |> tally() |> pull()

message(paste("Mismatches in TOTSEADAYS, TOTKWDAYSATSEA, TOTGTDAYSATSEA, 
            TOTFISHDAYS, TOTKWFISHDAYS or TOTGTFISHDAYS: ",tmp))

# fix mismatches
# 0's from logbook fec.R script ?
g4 <- g3 |> mutate(TOTSEADAYS     = if_else(TOTSEADAYS == "0", "NK", TOTSEADAYS), 
                   TOTKWDAYSATSEA = if_else(TOTKWDAYSATSEA == "0", "NK", TOTKWDAYSATSEA), 
                   TOTGTDAYSATSEA = if_else(TOTGTDAYSATSEA == "0", "NK", TOTGTDAYSATSEA),
                   TOTFISHDAYS    = if_else(TOTFISHDAYS == "0", "NK", TOTFISHDAYS), 
                   TOTKWFISHDAYS  = if_else(TOTKWFISHDAYS == "0", "NK", TOTKWFISHDAYS), 
                   TOTGTFISHDAYS  = if_else(TOTGTFISHDAYS == "0", "NK", TOTGTFISHDAYS))

# check
tmp <- g4 |> filter(YEAR %in% seq(2016,2024)) |> 
  select(YEAR, QUARTER, VESSEL_LENGTH, GEAR_TYPE, SUB_REGION, TOTSEADAYS, TOTKWDAYSATSEA, TOTGTDAYSATSEA,
         TOTFISHDAYS, TOTKWFISHDAYS, TOTGTFISHDAYS, HRSEA, KWHRSEA, GTHRSEA, CONFIDENTIAL) |> 
  filter((TOTSEADAYS == "0" | TOTKWDAYSATSEA == "0" | TOTGTDAYSATSEA == "0" |
            TOTFISHDAYS == "0" | TOTKWFISHDAYS == "0" | TOTGTFISHDAYS == "0") & 
           ((HRSEA != "0" | GTHRSEA != "0" | KWHRSEA != "0") & 
              (HRSEA != "NK" | GTHRSEA != "NK" | KWHRSEA != "NK"))) |> tally() |> pull()

message(paste("Mismatches in TOTSEADAYS, TOTKWDAYSATSEA, TOTGTDAYSATSEA, 
            TOTFISHDAYS, TOTKWFISHDAYS or TOTGTFISHDAYS: ",tmp))

# Put the variables in the correct order:
table_G <- g4 %>% select(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, METIER_7, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP, TOTSEADAYS, TOTKWDAYSATSEA, TOTGTDAYSATSEA, TOTFISHDAYS, TOTKWFISHDAYS, TOTGTFISHDAYS, HRSEA, KWHRSEA, GTHRSEA, TOTVES, CONFIDENTIAL)


# Write the resulting table G
write.xlsx(table_G,paste0(path_out,.Platform$file.sep,"FIN_TABLE_G_EFFORT.xlsx"), sheetName = "TABLE_G", colNames = TRUE, rowNames = FALSE)


#-------------------------------------------------------------------------------
#                   4. TABLE H (Landings by rectangle)                      ####   
#-------------------------------------------------------------------------------

# some TOTAL in SPECIES need to be removed

h <- akt1 %>% select(-KALASTUSPAIVAT, -MERIPAIVAT, -PAAKONETEHO, -VETOISUUS, -KALASTUSAIKAHH, -FT_REF) #not needed in table H

# Pivot to longer format
h2 <- h %>%
  pivot_longer(cols = starts_with("SVT"), 
               names_to = c("type", "SPECIES"), 
               names_pattern = "SVT_(.+)_(.+)", 
               values_to = "value")%>%
  mutate(value = as.numeric(value),  # Ensure numeric type
         value = ifelse(is.na(value), as.numeric(0), value)) # Replace NAs with 0


# Handle duplicates by summarising and sum up the values of KG and VALUE (eur)
h3 <- h2 %>%
  group_by(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, METIER_7, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP, RECTANGLE_TYPE, LATITUDE, LONGITUDE, C_SQUARE, type, SPECIES) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    n = n(),
    n2 = n_distinct(ULKOINENTUNNUS),
    .groups = 'drop'
  )

# Pivot to wider format and separate VALUE and KG
h4 <- h3 %>%
  pivot_wider(names_from = type, values_from = value)

# Rename columns and replace NA's with 0s
h5 <- h4 %>%
  rename(TOTVALLANDG = VALUE,
         TOTWGHTLANDG = KG)

# Transform the total weights into tonnes, add missing values and add the remaining variables
h6 <- h5 %>% mutate(
  TOTWGHTLANDG = TOTWGHTLANDG/1000,
  TOTWGHTLANDG = case_when(is.na(TOTWGHTLANDG) ~ 0,
                          TRUE ~ TOTWGHTLANDG),
  TOTVALLANDG = case_when(as.character(TOTVALLANDG) == 0 ~ "NK",
                          is.na(as.character(TOTVALLANDG)) ~ "NK",
                          TRUE ~ as.character(TOTVALLANDG))
  )


# Remove the zero rows (no catch) and create the confidential column based on the number of observations
h7 <- h6 %>% filter(TOTWGHTLANDG > 0) %>% mutate(
  CONFIDENTIAL = case_when(
    n2 < 3 ~ "A",
    n2 >= 3 ~ "N"
  )) %>% select(-n2, -n)

# throw out totals

h8 <- h7 |> filter(SPECIES != "TOTAL")

# Put the variables in the correct order:
table_H <- h8 %>% select(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, METIER_7, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP, RECTANGLE_TYPE, LATITUDE, LONGITUDE, C_SQUARE, SPECIES, TOTWGHTLANDG,TOTVALLANDG, CONFIDENTIAL)


# Write the resulting table H
openxlsx::write.xlsx(table_H, paste0(path_out,.Platform$file.sep,"FIN_TABLE_H_LANDINGS_BY_RECTANGLE.xlsx"), sheetName = "TABLE_H", colNames = TRUE, rowNames = FALSE)

#-------------------------------------------------------------------------------
#                   5. TABLE I (Effort by rectangle)                        ####
#-------------------------------------------------------------------------------

# remove variables not needed in table I (relating to catch)
i <- akt1 %>% select(-contains("SVT"), -MERIPAIVAT, -PAAKONETEHO, -VETOISUUS, -KALASTUSAIKAHH, -FT_REF)

# Sum the total of fishing days and calculate the number of distinct vessels in each strata
i2 <- i %>% group_by(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, METIER_7, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP, RECTANGLE_TYPE, LATITUDE, LONGITUDE, C_SQUARE) %>% summarise(
  TOTFISHDAYS = sum(KALASTUSPAIVAT, na.rm = TRUE),
  n2 = n_distinct(ULKOINENTUNNUS),
  .groups = 'drop')

# Create the last needed variable
table_I <- i2 %>% filter(TOTFISHDAYS > 0) %>% mutate(
  CONFIDENTIAL = case_when(
    n2 < 3 ~ "Y",
    n2 >= 3 ~ "N")) %>% select(-n2)

# Write the resulting table I
openxlsx::write.xlsx(table_I, paste0(path_out,.Platform$file.sep,"FIN_TABLE_I_EFFORT_BY_RECTANGLE.xlsx"), sheetName = "TABLE_I", colNames = TRUE, rowNames = FALSE)


#-------------------------------------------------------------------------------
#                   6. TABLE J (Capacity and fleet segment effort)          ####               
#-------------------------------------------------------------------------------

#add 2013-2015 sas data

#j_sas <- readRDS(paste0(path_der,"metier_2013_15_for_J.rds"))
j_sas <- readRDS(file = paste0(path_der,"logbook_2013_15_for_J.rds"))

# Select variables needed for J table from akt1 (DCPROD active vessels 2016 onwards):
j <- akt1 %>% select(-contains("SVT"), -RECTANGLE,-RECTANGLE_TYPE, -LATITUDE, -LONGITUDE, -C_SQUARE, -KALASTUSAIKAHH, -FROM, -TO, -metier6_orig, -METIER5)

j_all <- rbind(j_sas, j)

#Aggregating all passive gears to PG

j_all <- j_all %>% mutate(FISHING_TECH = case_when(
  FISHING_TECH == "FPO" |  FISHING_TECH == "DFN" | FISHING_TECH == "HOK" ~ "PG",
  TRUE ~ FISHING_TECH
))



#try to aggregate to trip level inside a subregion ####
j_all_trip <- j_all %>% group_by(COUNTRY, YEAR,VESSEL_LENGTH, FISHING_TECH, SUPRA_REGION,GEO_INDICATOR,FT_REF, ULKOINENTUNNUS, SUB_REGION) %>% summarise(KALASTUSPAIVAT=max(KALASTUSPAIVAT), MERIPAIVAT=max(KALASTUSPAIVAT))

# Select variables needed for J table from metier_sas (2013-2015 active vessels 2013-2015):

# from capacity table some needed variables
kap <- kapasiteetti %>% filter(VUOSI %in% years, REKISTERISSAVUODENAIKANA == 1) %>% select(
  YEAR = VUOSI,
  ULKOINENTUNNUS, 
  KOKPITUUS, 
  RAKENNUSPVM,
  PAAKONETEHO,
  VETOISUUS,
  VESSEL_LENGTH = VLENGTH_AER_NEW) %>% mutate(
  rakennusvuosi = as.numeric(format(RAKENNUSPVM,"%Y")),
  AGE = YEAR -rakennusvuosi
) %>% select(-RAKENNUSPVM, -rakennusvuosi) %>% distinct()

# removing duplicates and checking there are none left
dup<-kap %>% 
  group_by(YEAR, ULKOINENTUNNUS) %>% 
  filter(n()>1)

kap2 <- kap %>% arrange(YEAR, ULKOINENTUNNUS, KOKPITUUS, VESSEL_LENGTH, AGE) %>% 
  group_by(YEAR, ULKOINENTUNNUS) %>% 
  summarise(KOKPITUUS = first(KOKPITUUS), VESSEL_LENGTH = first(VESSEL_LENGTH), PAAKONETEHO=first(PAAKONETEHO), VETOISUUS=first(VETOISUUS) ,AGE = first(AGE),.groups = "drop")


# join the capacity to the j table
j2 <- left_join(kap2, j_all_trip, by = c("YEAR", "ULKOINENTUNNUS", "VESSEL_LENGTH"))

# replacing missing fishing tech's in the vessels that are not active
# Convert the factor to a character vector
j2 <- j2 %>% mutate(FISHING_TECH = as.character(FISHING_TECH))
# Replace NA values with "PG"
j3 <- j2 %>% mutate(FISHING_TECH = replace_na(FISHING_TECH, "PG"))
# convert the character vector back to a factor
j3 <- j3 %>% mutate(FISHING_TECH = as.factor(FISHING_TECH))


# Calculate the principal sub region for each vessel
j4 <- j3 %>% group_by(YEAR, ULKOINENTUNNUS, SUB_REGION) %>% summarise(
  fishingdays = n_distinct(KALASTUSPAIVAT, na.rm = TRUE),
  fishingtrips = n_distinct(FT_REF, na.rm = TRUE),
  .groups = 'drop'
  )
  
# Identify the principal sub region for each vessel
principal_sub_region <- j4 %>%
  group_by(YEAR, ULKOINENTUNNUS) %>%
  filter(fishingdays == max(fishingdays, na.rm = TRUE)) %>%
  # Handle ties by randomly selecting one sub region
  slice_sample(n = 1) %>%
  ungroup() %>% rename(PRINCIPAL_SUB_REGION = SUB_REGION)

# join the principal sub region to the j table
j5 <- left_join(j3, principal_sub_region, by = c("YEAR","ULKOINENTUNNUS"))
  
# Calculate the number of fishing trips for each vessel in sd
vessel_fishing_trips <- j5 %>% 
  group_by(COUNTRY, YEAR, VESSEL_LENGTH, FISHING_TECH, SUPRA_REGION, GEO_INDICATOR, PRINCIPAL_SUB_REGION, ULKOINENTUNNUS) %>% 
  summarise(
    total_fishingtrips = n_distinct(FT_REF, na.rm = TRUE),
    total_fishingdays = sum(KALASTUSPAIVAT, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate the average number of fishing days for the top 10 vessels with the highest number of fishing trips
maxseadays <- vessel_fishing_trips %>% 
  group_by(COUNTRY, YEAR, VESSEL_LENGTH, FISHING_TECH, SUPRA_REGION, GEO_INDICATOR, PRINCIPAL_SUB_REGION, ULKOINENTUNNUS) %>% 
  arrange(desc(total_fishingdays)) %>%
  # Sort vessels by days at sea in descending order and take the top 10
  slice(1:10) %>% 
  summarise(
    MAXSEADAYS = mean(total_fishingdays, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(MAXSEADAYS = replace_na(MAXSEADAYS, 'NK'))

maxseadays_2 <- maxseadays %>% 
  group_by(COUNTRY, YEAR, VESSEL_LENGTH, FISHING_TECH, SUPRA_REGION, GEO_INDICATOR, PRINCIPAL_SUB_REGION) %>% 
  summarise(
    MAXSEADAYS = as.integer(mean(MAXSEADAYS, na.rm = TRUE))
    )

# Calculate the other sums and averages
j6 <- j5 %>% 
  group_by(COUNTRY, YEAR, VESSEL_LENGTH, FISHING_TECH, SUPRA_REGION, GEO_INDICATOR, PRINCIPAL_SUB_REGION) %>% 
  summarise(
    TOTTRIPS = n_distinct(FT_REF), # changed table aggregation to fishingtrips, changed from variable fishingtrips
    TOTKW = sum(PAAKONETEHO, na.rm = TRUE),
    TOTGT = sum(VETOISUUS, na.rm = TRUE),
    TOTVES = n_distinct(ULKOINENTUNNUS, na.rm = TRUE),
    AVGAGE = mean(AGE, na.rm = TRUE),
    AVGLOA = mean(KOKPITUUS, na.rm = TRUE),
    .groups = 'drop'
  )

# join the maximum days at sea with j table
j7 <- left_join(j6, maxseadays_2, by = c("COUNTRY", "YEAR", "VESSEL_LENGTH", "FISHING_TECH", "SUPRA_REGION", "GEO_INDICATOR", "PRINCIPAL_SUB_REGION"))

# replace missing values and arrange by year
j8 <- j7 %>% mutate(
  COUNTRY = replace_na(COUNTRY, "FIN"),
  SUPRA_REGION = replace_na(SUPRA_REGION, "NAO"),
  GEO_INDICATOR = replace_na(GEO_INDICATOR, "NGI"),
  PRINCIPAL_SUB_REGION = replace_na(PRINCIPAL_SUB_REGION, "NK")
) %>% arrange(by = YEAR)


# SET TOTTRIPS to 0 for inactive vessel ranges

j9 <- j8 %>% mutate(TOTTRIPS = case_when(
  TOTTRIPS == 1 & PRINCIPAL_SUB_REGION == "NK" ~ 0,
  TRUE ~ TOTTRIPS
))
# mutate the fishing technique to INACTIVE if TOTTRIPS ==0

j10 <- j9 %>% mutate(FISHING_TECH = case_when(
  TOTTRIPS == 0 ~"INACTIVE",
  TRUE ~ FISHING_TECH
))



# Put the variables in the correct order:
table_J <- j10 %>% select(COUNTRY, YEAR, VESSEL_LENGTH, FISHING_TECH, SUPRA_REGION, GEO_INDICATOR, PRINCIPAL_SUB_REGION, TOTTRIPS, TOTKW, TOTGT, TOTVES, AVGAGE, AVGLOA, MAXSEADAYS)


# Write the resulting table J
write.xlsx(table_J, paste0(path_out,.Platform$file.sep,"FIN_TABLE_J_CAPACITY.xlsx"), 
           sheetName = "TABLE_J", colNames = TRUE, rowNames = FALSE)





