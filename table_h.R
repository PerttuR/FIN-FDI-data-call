#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE H
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa, Anna-Kaisa Ylitalo, Antti Sykkö
#
# Date: JUN-2018
# Updated: JUN 2021 by Perttu
#          JUN-2022 by Anna-Kaisa and Perttu
#          MAY-2023 by Antti
#          JUN-2024 by Mira, Petri and Perttu
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is to prepare FDI data tables H and I
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

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------

# Output folder
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2024")

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                       1 Import data for 2023 H table needs                    
#-------------------------------------------------------------------------------

source("db.r")

## Read in data
# alter based on the date of the postgres schema date
schemadate <- "2024-06-14"


# Postgres
aktiviteetti <- read.dbTable(schema=paste(schemadate, "-dcprod", sep = ""), table='kalastusaktiviteetti', dbname = "kake_siirto")

aktiviteetti_2023 <- aktiviteetti %>% filter(KALASTUSVUOSI == "2023") %>% 
  mutate(kalastus_kk = format(PALUUPVM, "%m"),
         kalastus_vuosi = format(PALUUPVM,"%Y"),
         kalastus_kk = case_when(
           kalastus_vuosi == "2022" ~ "01",
           kalastus_vuosi == "2024" ~ "12",
           TRUE ~ kalastus_kk
         ))

# Select and mutate needed variables for tables H and I
akt1 <- aktiviteetti_2023 %>% 
  select(YEAR = KALASTUSVUOSI,
         ULKOINENTUNNUS,
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
         -contains("SVT_VALUE_IND_"),
         RECTANGLE)%>% 
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
    SPECON_TECH = "NA",
    DEEP = "NA",
    RECTANGLE_TYPE = "05*1",
    C_SQUARE = "NA") %>% 
  select(-MONTH, -ICES) %>% mutate(
    SUB_REGION = case_when(
      SUB_REGION == "27.3.d.28" ~ "27.3.d.28.2.", #Vaihtoehtoine 28.1 ja 28.2 ja jälkimmäinen pääallas, jolla Suomi kalastaa
      TRUE ~ SUB_REGION
    ))

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

# Change the needed metiers into new format
akt1 <- akt1 %>%  mutate(METIER = case_when(
  METIER == "GNS_ANA_0_0_0" ~ "GNS_ANA_>0_0_0",
  METIER == "GNS_SPF_16-109_0_0" ~ "GNS_SPF_>=16_0_0",
  METIER == "OTM_SPF_16-104_0_0" ~ "OTM_SPF_>0_0_0",
  METIER == "PTM_SPF_16-104_0_0" ~ "PTM_SPF_>0_0_0",
  TRUE ~ METIER))  # All other values remain unchanged


# Pivot to longer format
akt2 <- akt1 %>%
  pivot_longer(cols = starts_with("SVT"), 
               names_to = c("type", "SPECIES"), 
               names_pattern = "SVT_(.+)_(.+)", 
               values_to = "value")%>%
  mutate(value = as.numeric(value),  # Ensure numeric type
         value = ifelse(is.na(value), as.numeric(0), value)) # Replace NAs with 0


# Handle duplicates by summarising
akt3 <- akt2 %>%
  #mutate(non_zero_flag = ifelse(value > 0, 1, 0)) %>%
  group_by(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, METIER_7, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP, RECTANGLE_TYPE, LATITUDE, LONGITUDE, C_SQUARE, type, SPECIES) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    n = n(),
    n2 = n_distinct(ULKOINENTUNNUS),
    #count_non_zero = sum(non_zero_flag), # the number of observations summed together
    .groups = 'drop'
  )


# Pivot to wider format and separate VALUE and KG
akt4 <- akt3 %>%
  pivot_wider(names_from = type, values_from = value)

# Rename columns and replace NA's with 0s
akt5 <- akt4 %>%
  rename(TOTVALLANDG = VALUE,
         TOTWGHTLANDG = KG)



# Transform the total weights into tonnes, add missing values and add the remaining variables

akt6 <- akt5 %>% mutate(
  TOTWGHTLANDG = TOTWGHTLANDG/1000,
  TOTWGHTLANDG = case_when(is.na(TOTWGHTLANDG) ~ 0,
                          TRUE ~ TOTWGHTLANDG),
  TOTVALLANDG = case_when(as.character(TOTVALLANDG) == 0 ~ "NK",
                          is.na(as.character(TOTVALLANDG)) ~ "NK",
                          TRUE ~ as.character(TOTVALLANDG))
  )


# Remove the zero rows (no catch) and create the confidential column based on the number of observations
akt7 <- akt6 %>% filter(TOTWGHTLANDG > 0) %>% mutate(
  CONFIDENTIAL = case_when(
    n2 < 3 ~ "A",
    n2 >= 3 ~ "N"
  )) %>% select(-n2, -n)


# Put the variables in the correct order:

table_H <- akt7 %>% select(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, METIER_7, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP, RECTANGLE_TYPE, LATITUDE, LONGITUDE, C_SQUARE, SPECIES, TOTWGHTLANDG,TOTVALLANDG, CONFIDENTIAL)



openxlsx::write.xlsx(table_H, paste0(path_out,.Platform$file.sep,"FIN_TABLE_H_LANDINGS_BY_RECTANGLE.xlsx"), sheetName = "TABLE_H", colNames = TRUE, rowNames = FALSE)



# Testausta


test <- akt5 %>% filter(VESSEL_LENGTH == "VL40XX", MESH_SIZE_RANGE == "16D32", METIER == "OTM_SPF_>0_0_0", LATITUDE == 60.75, LONGITUDE == 20.5, QUARTER == 4)

test1 <- table_H %>% filter(VESSEL_LENGTH == "VL40XX", MESH_SIZE_RANGE == "16D32", METIER == "OTM_SPF_>0_0_0", LATITUDE == 60.75, LONGITUDE == 20.5, QUARTER == 4)

test2 <- akt1 %>% filter(VESSEL_LENGTH == "VL40XX", MESH_SIZE_RANGE == "16D32", METIER == "OTM_SPF_>0_0_0", LATITUDE == 60.75, LONGITUDE == 20.5, QUARTER == 4)



test3 <- table_H %>% filter(VESSEL_LENGTH == "VL2440", MESH_SIZE_RANGE == "16D32", METIER == "OTM_SPF_>0_0_0", LATITUDE == 60.75, LONGITUDE == 20.5, QUARTER == 2)

test4 <- akt1 %>% filter(VESSEL_LENGTH == "VL2440", MESH_SIZE_RANGE == "16D32", METIER == "OTM_SPF_>0_0_0", LATITUDE == 60.75, LONGITUDE == 20.5, QUARTER == 2)
