#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE A
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa, Antti Sykkö
#
# Creation Date: JUL-2019
# Updated: JUN 2021 (Perttu)
# Updated: JUN 2022 (Perttu)
# Updated: MAY 2023 (Antti)
# Updated: JUN 2024 Mira, Perttu, Petri
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for preparing FDI data tables from Table A from statistical DEP
#-------------------------------------------------------------------------------


# install.packages("RPostgreSQL")
# install.packages("dplyr")
# install.packages("xlsx")


# Add extension _sas to the csv's in orig-folder 

#- Clear workspace
rm(list=ls())

# needed libraries .. testi
library(dplyr)
library(openxlsx)
library(icesVocab)
library(openxlsx)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------

# Output folder
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2024")

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                       1. Import and modify table A                      
#-------------------------------------------------------------------------------

source("db.r")

## Read in data
# alter based on the date of the postgres schema date
schemadate <- "2024-06-14"


# Postgres
aktiviteetti <- read.dbTable(schema=paste(schemadate, "-dcprod", sep = ""), table='kalastusaktiviteetti', dbname = "kake_siirto")

# Read in data from the correct year
aktiviteetti_2023 <- aktiviteetti %>% filter(KALASTUSVUOSI == 2023) %>% 
  mutate(kalastus_kk = format(PALUUPVM, "%m"), #get the correct month in order to make quarters later on
         kalastus_vuosi = format(PALUUPVM,"%Y"),
         kalastus_kk = case_when(
           kalastus_vuosi == "2022" ~ "01", #set the months right on the beginning and end of the year
           kalastus_vuosi == "2024" ~ "12",
           TRUE ~ kalastus_kk
         ))

# Select and mutate all the needed variables for table A
a <- aktiviteetti_2023 %>% 
  select(YEAR = KALASTUSVUOSI,
         ULKOINENTUNNUS,
         KALASTUSPAIVAT,
         MONTH = kalastus_kk,
         VESSEL_LENGTH = VLENGTH_AER_NEW,
         FISHING_TECH = FT,
         GEAR_TYPE = GEAR,
         TARGET_ASSEMBLAGE = LEVEL5,
         MESH_SIZE_RANGE = SILMAKOKO,
         METIER,
         ICES,
         contains("SVT_KG_"),
         contains("SVT_VALUE_"),
         -contains("SVT_KG_IND_"),
         -contains("SVT_KG_HUC_"),
         -contains("SVT_VALUE_HUC_"),
         -contains("SVT_VALUE_IND_"))%>% 
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
    MESH_SIZE_RANGE = case_when(
      FISHING_TECH == "TM" & MESH_SIZE_RANGE < 16 ~ "00D16",
      FISHING_TECH == "TM" & 16 <= MESH_SIZE_RANGE & MESH_SIZE_RANGE < 32 ~ "16D32",
      FISHING_TECH == "TM" & 32 <= MESH_SIZE_RANGE & MESH_SIZE_RANGE < 90 ~ "32D90",
      FISHING_TECH == "TM" & 90 <= MESH_SIZE_RANGE & MESH_SIZE_RANGE < 105 ~ "90D105",
      FISHING_TECH == "TM" & 105 <= MESH_SIZE_RANGE & MESH_SIZE_RANGE < 110 ~ "105D110",
      FISHING_TECH == "TM" & 110 <= MESH_SIZE_RANGE ~ "110DXX",
      FISHING_TECH == "PG" & MESH_SIZE_RANGE < 16 ~ "00D16",
      FISHING_TECH == "PG" & 16 <= MESH_SIZE_RANGE & MESH_SIZE_RANGE < 32 ~ "16D32",
      FISHING_TECH == "PG" & 32 <= MESH_SIZE_RANGE & MESH_SIZE_RANGE < 90 ~ "32D90",
      FISHING_TECH == "PG" & 90 <= MESH_SIZE_RANGE & MESH_SIZE_RANGE < 110 ~ "90D110",
      FISHING_TECH == "PG" & 110 <= MESH_SIZE_RANGE & MESH_SIZE_RANGE < 157 ~ "110D157",
      FISHING_TECH == "PG" & 157 <= MESH_SIZE_RANGE ~ "157DXX",
      TRUE ~ "NK"
    ),
    METIER_7 = "NA",
    SUPRA_REGION = "NAO",
    SUB_REGION = paste("27.3.d.", ICES, sep = ""),
    EEZ_INDICATOR = "NA",
    GEO_INDICATOR = "NGI",
    NEP_SUB_REGION = "NA",
    SPECON_TECH = "NA",
    DEEP = "NA",) %>% 
  select(-MONTH, -ICES) %>% mutate(
    SUB_REGION = case_when(
      SUB_REGION == "27.3.d.28" ~ "27.3.d.28.2", #Vaihtoehtoinen 28.1 ja 28.2 ja jälkimmäinen pääallas, jolla Suomi kalastaa
      TRUE ~ SUB_REGION
    )) #tehdään tarkaste myöhemmin!


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


# Put the variables in the correct order:

table_H <- h7 %>% select(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, METIER_7, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP, RECTANGLE_TYPE, LATITUDE, LONGITUDE, C_SQUARE, SPECIES, TOTWGHTLANDG,TOTVALLANDG, CONFIDENTIAL)


# make a key variable to match table A key (domain_discards or domain_landings)

# first make individually all the parts that form the key
country_code <- "FIN"
quarter <- landing$q
subregion <- paste("27.3.D.", landing$ices_osa_alue, sep = "")
#Stat dep uses FPO instead of FPN so change
landing <- landing %>% mutate(metier = replace(metier,metier=="FPN_FWS_>0_0_0","FPO_FWS_>0_0_0"))
landing <- landing %>% mutate(metier = replace(metier,metier=="FPN_SPF_>0_0_0","FPO_SPF_>0_0_0"))
gear_type <- landing$metier
unique(gear_type)

# codes for vessel length from appendix 2:
landing$vessel_length_code[landing$laivan_pituus_cm < 1000] <- "VL0010"
landing$vessel_length_code[landing$laivan_pituus_cm >= 1000 & landing$laivan_pituus_cm < 1200] <- "VL1012"
landing$vessel_length_code[landing$laivan_pituus_cm >= 1200 & landing$laivan_pituus_cm < 1800] <- "VL1218"
landing$vessel_length_code[landing$laivan_pituus_cm >= 1800 & landing$laivan_pituus_cm < 2400] <- "VL1824"
landing$vessel_length_code[landing$laivan_pituus_cm >= 2400 & landing$laivan_pituus_cm < 4000] <- "VL2440"
landing$vessel_length_code[landing$laivan_pituus_cm >= 4000] <- "VL40XX"
landing$vessel_length_code[is.na(landing$laivan_pituus_cm)] <- "NK"

vessel_length <- landing$vessel_length_code

species <- landing$fao
commercial_cat <- "NA"

# then combine them as a single key, identical to that from table A
landing$domain_landings <- paste(country_code, quarter, subregion, gear_type, vessel_length, species, commercial_cat, sep = "_")


# ... write delivery to xlsx file using openxlsx

openxlsx::write.xlsx(table_A, paste0(path_out,.Platform$file.sep,"FIN_TABLE_A_CATCH.xlsx"), sheetName = "TABLE_A", colNames = TRUE, rowNames = FALSE)


#-------------------------------------------------------------------------------
