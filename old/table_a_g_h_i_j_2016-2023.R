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
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is to prepare FDI data tables A, G, H, I and J
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

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------

# Output folder
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2025")
path_der <- paste0(getwd(), .Platform$file.sep, "der/2024/")
path_orig <- paste0(getwd(), .Platform$file.sep, "orig/")
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                       1. Import data for 2016-2023 A, G, H, I and J table needs                    
#-------------------------------------------------------------------------------

#----------- set years ---------------

years <- c(2016:2023)


#----------- get data ---------------

source("db.r")

## Read in data
# alter based on the date of the postgres schema date
schemadate <- "2025-04-10"


# Postgres (used with A, G, H and I)
aktiviteetti <- read.dbTable(schema=paste(schemadate, "-dcprod", sep = ""), table='kalastusaktiviteetti_2025_05_06', dbname = "kake_siirto")

# (used with J)
kapasiteetti <- read.dbTable(schema=paste(schemadate, "-dcprod", sep = ""), table='kapasiteetti_2025_05_06', dbname = "kake_siirto")

# Discards excel
discards <- read.xlsx(paste0(path_orig, "Vaurioitetut lohet 2016-2023.xlsx"))
names(discards) <- toupper(names(discards))


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

# Select and mutate all the needed variables for tables H and I
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

# Change the needed metiers into new format
akt1 <- akt1 %>%  mutate(METIER = case_when(
  METIER == "GNS_ANA_0_0_0" ~ "GNS_ANA_>0_0_0",
  METIER == "GNS_SPF_16-109_0_0" ~ "GNS_SPF_>=16_0_0",
  METIER == "OTM_SPF_16-104_0_0" ~ "OTM_SPF_>0_0_0",
  METIER == "PTM_SPF_16-104_0_0" ~ "PTM_SPF_>0_0_0",
  METIER == "OTB_DEF_>=105_1_120" ~ "OTB_DEF_100-119_0_0",
  METIER == "OTM_DEF_>=105_1_120" ~ "OTM_DEF_100-119_0_0",
  TRUE ~ METIER))  # All other values remain unchanged


#-------------------------------------------------------------------------------
#                   2. TABLE A (Catch summary)                       
#-------------------------------------------------------------------------------

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

# Add the discards data
a10 <- a9 %>% left_join(discards, by = c("YEAR", "QUARTER", "VESSEL_LENGTH", "FISHING_TECH" = "FT", "GEAR_TYPE" = "GEAR", "SUB_REGION", "TARGET_ASSEMBLAGE", "MESH_SIZE_RANGE", "SPECIES"))

a11 <- a10 %>% rename(DISCARDS = DISCARDS_KG) %>% mutate(DISCARDS = case_when(
  is.na(DISCARDS) ~ 0,
  TRUE ~ DISCARDS))

# Put the variables in the correct oDISCARDS_KG# Put the variables in the correct order:
table_A <- a11 %>% select(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, METIER_7, DOMAIN_DISCARDS, DOMAIN_LANDINGS, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, NEP_SUB_REGION, SPECON_TECH, DEEP, SPECIES, TOTWGHTLANDG,TOTVALLANDG, DISCARDS, CONFIDENTIAL)









# Write the resulting table A into der folder as rds file and into results folder
saveRDS(table_A, file = paste0(path_der,.Platform$file.sep,"table_A.rds"))

# Write the resulting table A
openxlsx::write.xlsx(table_A, paste0(path_out,.Platform$file.sep,"FIN_TABLE_A_CATCH.xlsx"), sheetName = "TABLE_A", colNames = TRUE, rowNames = FALSE)



#-------------------------------------------------------------------------------
#                   3. TABLE G (Effort summary)                       
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


# Put the variables in the correct order:
table_G <- g2 %>% select(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, METIER_7, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP, TOTSEADAYS, TOTKWDAYSATSEA, TOTGTDAYSATSEA, TOTFISHDAYS, TOTKWFISHDAYS, TOTGTFISHDAYS, HRSEA, KWHRSEA, GTHRSEA, TOTVES, CONFIDENTIAL)


# Write the resulting table G
write.xlsx(table_G,paste0(path_out,.Platform$file.sep,"FIN_TABLE_G_EFFORT.xlsx"), sheetName = "TABLE_G", colNames = TRUE, rowNames = FALSE)


#-------------------------------------------------------------------------------
#                   4. TABLE H (Landings by rectangle)                       
#-------------------------------------------------------------------------------


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


# Put the variables in the correct order:
table_H <- h7 %>% select(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, METIER_7, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP, RECTANGLE_TYPE, LATITUDE, LONGITUDE, C_SQUARE, SPECIES, TOTWGHTLANDG,TOTVALLANDG, CONFIDENTIAL)


# Write the resulting table H
openxlsx::write.xlsx(table_H, paste0(path_out,.Platform$file.sep,"FIN_TABLE_H_LANDINGS_BY_RECTANGLE.xlsx"), sheetName = "TABLE_H", colNames = TRUE, rowNames = FALSE)

#-------------------------------------------------------------------------------
#                   5. TABLE I (Effort by rectangle)                       
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
#                   6. TABLE J (Capacity and fleet segment effort)                       
#-------------------------------------------------------------------------------

# Select variables needed for J table
j <- akt1 %>% select(-contains("SVT"), -RECTANGLE,-RECTANGLE_TYPE, -LATITUDE, -LONGITUDE, -C_SQUARE)

# from capacity table some needed variables
kap <- kapasiteetti %>% filter(VUOSI %in% years, REKISTERISSAVUODENAIKANA == 1) %>% select(
  YEAR = VUOSI,
  ULKOINENTUNNUS, 
  KOKPITUUS, 
  RAKENNUSPVM,
  VESSEL_LENGTH = VLENGTH_AER_NEW) %>% mutate(
  rakennusvuosi = as.numeric(format(RAKENNUSPVM,"%Y")),
  AGE = max(years)-rakennusvuosi
) %>% select(-RAKENNUSPVM, -rakennusvuosi) %>% distinct()

# removing duplicates and checking there are none left
dup<-kap %>% 
  group_by(YEAR, ULKOINENTUNNUS) %>% 
  filter(n()>1)

kap2 <- kap %>% arrange(YEAR, ULKOINENTUNNUS, KOKPITUUS, VESSEL_LENGTH, AGE) %>% group_by(YEAR, ULKOINENTUNNUS) %>% summarise(KOKPITUUS = first(KOKPITUUS), VESSEL_LENGTH = first(VESSEL_LENGTH), AGE = first(AGE),.groups = "drop")


# join the capacity to the j table
j2 <- left_join(kap2, j, by = c("YEAR", "ULKOINENTUNNUS", "VESSEL_LENGTH"))

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
  
# Calculate the number of fishing trips for each vessel
vessel_fishing_trips <- j5 %>% group_by(COUNTRY, YEAR, VESSEL_LENGTH, FISHING_TECH, SUPRA_REGION, GEO_INDICATOR, PRINCIPAL_SUB_REGION, ULKOINENTUNNUS) %>% summarise(
    total_fishingtrips = n_distinct(FT_REF, na.rm = TRUE),
    total_fishingdays = sum(fishingdays, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate the average number of fishing days for the top 10 vessels with the highest number of fishing trips
maxseadays <- vessel_fishing_trips %>% group_by(COUNTRY, YEAR, VESSEL_LENGTH, FISHING_TECH, SUPRA_REGION, GEO_INDICATOR, PRINCIPAL_SUB_REGION) %>% arrange(desc(total_fishingdays)) %>%
  # Sort vessels by days at sea in descending order and take the top 10
  slice(1:10) %>% summarise(
    MAXSEADAYS = mean(total_fishingdays, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(MAXSEADAYS = replace_na(MAXSEADAYS, 'NK'))
  

# Calculate the other sums and averages
j6 <- j5 %>% group_by(COUNTRY, YEAR, VESSEL_LENGTH, FISHING_TECH, SUPRA_REGION, GEO_INDICATOR, PRINCIPAL_SUB_REGION) %>% summarise(
  TOTTRIPS = sum(fishingtrips, na.rm = TRUE),
  TOTKW = sum(PAAKONETEHO, na.rm = TRUE),
  TOTGT = sum(VETOISUUS, na.rm = TRUE),
  TOTVES = n_distinct(ULKOINENTUNNUS, na.rm = TRUE),
  AVGAGE = mean(AGE, na.rm = TRUE),
  AVGLOA = mean(KOKPITUUS, na.rm = TRUE),
  .groups = 'drop'
)

# join the maximum days at sea with j table
j7 <- left_join(j6, maxseadays, by = c("COUNTRY", "YEAR", "VESSEL_LENGTH", "FISHING_TECH", "SUPRA_REGION", "GEO_INDICATOR", "PRINCIPAL_SUB_REGION"))

# replace missing values and arrange by year
j8 <- j7 %>% mutate(
  COUNTRY = replace_na(COUNTRY, "FIN"),
  SUPRA_REGION = replace_na(SUPRA_REGION, "NAO"),
  GEO_INDICATOR = replace_na(GEO_INDICATOR, "NGI"),
  PRINCIPAL_SUB_REGION = replace_na(PRINCIPAL_SUB_REGION, "NK")
) %>% arrange(by = YEAR)


# Put the variables in the correct order:
table_J <- j8 %>% select(COUNTRY, YEAR, VESSEL_LENGTH, FISHING_TECH, SUPRA_REGION, GEO_INDICATOR, PRINCIPAL_SUB_REGION, TOTTRIPS, TOTKW, TOTGT, TOTVES, AVGAGE, AVGLOA, MAXSEADAYS)


# Write the resulting table J
write.xlsx(table_J, paste0(path_out,.Platform$file.sep,"FIN_TABLE_J_CAPACITY.xlsx"), 
           sheetName = "TABLE_J", colNames = TRUE, rowNames = FALSE)





