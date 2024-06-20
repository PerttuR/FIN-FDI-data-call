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
library(RPostgreSQL)
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

# Select and mutate needed variables for tables H and I
akt1 <- aktiviteetti %>% filter(KALASTUSVUOSI == "2023") %>% 
  select(YEAR = KALASTUSVUOSI,
         KK,
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
         LE_SLAT = KOORDELASKU, #log event start position latitude 
         LE_SLON = KOORDNLASKU, #log event start position longitude 
         LE_ELAT = KOORDENOSTO, #log event end position latitude
         LE_ELON = KOORDNNOSTO, #log event end position longitude
         RECTANGLE)%>% 
  separate(LE_SLAT, paste("lat",c("d","m","s"), sep="_") ) %>% # coordinates into the right format (decimals)!
  separate(LE_SLON, paste("long",c("d","m","s"), sep="_" ) ) %>%
  separate(LE_ELAT, paste("lat2",c("d","m","s"), sep="_") ) %>%
  separate(LE_ELON, paste("long2",c("d","m","s"), sep="_" ) ) %>%
  mutate(
    COUNTRY = "FIN",
    QUARTER = case_when(
      1 <= KK & KK <= 3 ~ 1,
      4 <= KK & KK <= 6 ~ 2,
      7 <= KK & KK <= 9 ~ 3,
      10 <= KK & KK <= 12 ~ 4),
    TARGET_ASSEMBLAGE = case_when(
      TARGET_ASSEMBLAGE == "Small pelagic" ~ "SPF",
      TARGET_ASSEMBLAGE == "Activity missing" ~ "NK",
      TARGET_ASSEMBLAGE == "Freshwater" ~ "FWS",
      TARGET_ASSEMBLAGE == "Anadromous" ~ "ANA",
      TARGET_ASSEMBLAGE == "Finfish" ~ "FIF"),
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
    SUB_REGION = paste("27.3.c.", ICES, sep = ""),
    EEZ_INDICATOR = "NA",
    GEO_INDICATOR = "NGI",
    SPECON_TECH = "NA",
    DEEP = "NA",
    RECTANGLE_TYPE = "05*1",
    lat_d = as.numeric(lat_d),
    lat_m = as.numeric(lat_m),
    lat_s = as.numeric(lat_s),
    long_d = as.numeric(long_d),
    long_m = as.numeric(long_m),
    long_s = as.numeric(long_s),
    lat2_d = as.numeric(lat2_d),
    lat2_m = as.numeric(lat2_m),
    lat2_s = as.numeric(lat2_s),
    long2_d = as.numeric(long2_d),
    long2_m = as.numeric(long2_m),
    long2_s = as.numeric(long2_s),
    LE_SLAT=lat_d + lat_m/60 + lat_s/60^2,
    LE_SLON=long_d + long_m/60 + long_s/60^2,
    LE_ELAT=lat2_d + lat2_m/60 + lat2_s/60^2,
    LE_ELON=long2_d + long2_m/60 + long2_s/60^2,
    LATITUDE = (LE_SLAT + LE_ELAT) / 2,
    LONGITUDE = (LE_SLON + LE_ELON) / 2
  ) %>% 
  select(-KK, -ICES, -lat_d, -lat_m, -lat_s, -long_d, -long_m, -long_s, -lat2_d, -lat2_m, -lat2_s, -long2_d, -long2_m, -long2_s, -LE_SLAT, -LE_SLON, -LE_ELAT, -LE_ELON)

# Create variables LATITUDE and LONGITUDE:

celtic <- get_csquare(ecoregion = "Baltic Sea")

# Apply the function to each rectangle in the data frame
midpoints <- do.call(rbind, lapply(df$RECTANGLE, get_midpoint))

# Merge the midpoints with the original data frame
akthh <- merge(akt1, midpoints, by = "RECTANGLE")


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
               names_to = c("type", "species"), 
               names_pattern = "SVT_(.+)_(.+)", 
               values_to = "value")


# Handle duplicates by summarising
akt3 <- akt2 %>%
  group_by(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, METIER_7, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP, type, species) %>%
  summarise(value = sum(value),
            count_non_zero = sum(value > 0), 
            .groups = 'drop')

# Pivot to wider format and separate VALUE and KG
akt4 <- akt3 %>%
  pivot_wider(names_from = type, values_from = value)

# Rename columns
akt5 <- akt4 %>%
  rename(TOTVALLANDG = VALUE,
         TOTWGHTLANDG = KG)

# Transform the total weights into tonnes, add missing values and add the remaining variables

akt6 <- akt5 %>% mutate(
  TOTWGHTLANDG = TOTWGHTLANDG/1000,
  TOTWGHTLANDG = case_when(is.na(TOTWGHTLANDG) ~ 0,
                          TRUE ~ TOTWGHTLANDG),
  TOTVALLANDG = case_when(is.na(as.character(TOTVALLANDG)) ~ "NK",
                          TRUE ~ as.character(TOTVALLANDG)),
  DISCARDS = 0
  )










unique(is.na(akt1$GEAR_TYPE))
unique(aktiviteetti$LEVEL5)
p1 %>% 
  mutate(NewCol = case_when(Value == 1 ~ "Test1Yes",
                            is.na(Value) ~ "TestYes",
                            Value == 0 ~ "Test0Yes",
                            TRUE ~ "No"))


metier7 <- read.xlsx(paste0("C:/Users/03080096/OneDrive - Valtion/2024/FIN-FDI-data-call/metier_level7.xlsx"))
metakt <- aktiviteetti %>% distinct(METIER)
as.data.frame(metakt)
metier_check <-  metier7 %>% left_join(metakt, by = c("METIER_7" = "METIER"), keep = T)
#-------------------------------------------------------------------------------









eflalo <- aktiviteetti %>% filter(KALASTUSVUOSI == 2023)%>% select(VE_REF = ULKOINENTUNNUS, # vessel ID
                                                                   VE_FLT = FT_TRIP, # fleet
                                                                   VE_COU = MAA_LIPPU, # home country
                                                                   VE_LEN = KOKONAISPITUUS, # vessel length
                                                                   VE_KW = PAAKONETEHO, # vessel power
                                                                   VE_TON = VETOISUUS, # vessel tonnage
                                                                   
                                                                   FT_REF, # fishing trip reference number
                                                                   FT_DCOU = MAAKOODI_LAHTO, # departure country
                                                                   FT_DHAR = LAHTOSATAMA_EUKOODI, # departure harbour
                                                                   FT_DDAT = LAHTOPVM, # departure date DD/MM/YYYY
                                                                   FT_DTIME = LAHTOAIKA, # departure time HH:MM
                                                                   FT_LCOU = MAAKOODI_PALUU, # landing country 
                                                                   FT_LHAR = PALUUSATAMA_EUKOODI, # landing harbour
                                                                   FT_LDAT = PALUUPVM, # arrival date DD/MM/YYYY
                                                                   FT_LTIME = PALUUAIKA, # arrival time HH:MM
                                                                   
                                                                   LE_ID, # log event ID 
                                                                   LE_CDAT = KALASTUSPVM, # catch date DD/MM/YYYY
                                                                   LE_STIME = LASKUAIKA, # log event start time *OPTIONAL*
                                                                   LE_ETIME = NOSTOAIKA, # log event end time *OPTIONAL*
                                                                   LE_SLAT = KOORDELASKU, #log event start position latitude *OPTIONAL*
                                                                   LE_SLON = KOORDNLASKU, #log event start position longitude *OPTIONAL*
                                                                   LE_ELAT = KOORDENOSTO, #log event end position latitude *OPTIONAL*
                                                                   LE_ELON = KOORDNNOSTO, #log event end position longitude *OPTIONAL*
                                                                   LE_GEAR = GEAR, # gear
                                                                   LE_MSZ = SILMAKOKO, # mesh size
                                                                   LE_RECT = RECTANGLE, # ICES rectangle
                                                                   LE_DIV = ICES, # ICES division
                                                                   LE_MET =  METIER,# fishing activity (metier)
                                                                   contains("SVT_KG_"),
                                                                   contains("SVT_VALUE_"),
                                                                   -contains("SVT_KG_IND_"),
                                                                   -contains("SVT_KG_HUC_"),
                                                                   -contains("SVT_VALUE_HUC_"),
                                                                   -contains("SVT_VALUE_IND_")
)%>% 
  separate(LE_SLAT, paste("lat",c("d","m","s"), sep="_") ) %>% # coordinates into the right format (decimals)!
  separate(LE_SLON, paste("long",c("d","m","s"), sep="_" ) ) %>%
  separate(LE_ELAT, paste("lat2",c("d","m","s"), sep="_") ) %>%
  separate(LE_ELON, paste("long2",c("d","m","s"), sep="_" ) ) 



%>%
  mutate(FT_DDAT = format(FT_DDAT, "%d/%m/%Y"), # departure date DD/MM/YYYY)
         FT_LDAT = format(FT_LDAT, "%d/%m/%Y"), # arrival date DD/MM/YYY
         lat_d = as.numeric(lat_d),
         lat_m = as.numeric(lat_m),
         lat_s = as.numeric(lat_s),
         long_d = as.numeric(long_d),
         long_m = as.numeric(long_m),
         long_s = as.numeric(long_s),
         lat2_d = as.numeric(lat2_d),
         lat2_m = as.numeric(lat2_m),
         lat2_s = as.numeric(lat2_s),
         long2_d = as.numeric(long2_d),
         long2_m = as.numeric(long2_m),
         long2_s = as.numeric(long2_s),
         LE_SLAT=lat_d + lat_m/60 + lat_s/60^2,
         LE_SLON=long_d + long_m/60 + long_s/60^2,
         LE_ELAT=lat2_d + lat2_m/60 + lat2_s/60^2,
         LE_ELON=long2_d + long2_m/60 + long2_s/60^2) %>% 
  select(-starts_with("lat"), -starts_with("long"))
















#-------------------------------------------------------------------------------
#                       2. Modify table H                     
#-------------------------------------------------------------------------------


# .. define coordinates 
source("spatial.R")

midpoints <- latlon(table_H$RECTANGLE,midpoint=TRUE)

table_H <- tibble::rowid_to_column(table_H, "ID")
midpoints <- tibble::rowid_to_column(midpoints, "ID")

table_H <- left_join(table_H, midpoints,copy = TRUE)

table_H <- table_H %>% rename(RECTANGLE_LAT = SI_LATI, RECTANGLE_LON = SI_LONG)

# .. add empty col for new metier
table_H$METIER_7 <- 'NA'



# .. Invalid code: GNS_SPF_16-109_0_0
table_H$METIER <- ifelse(table_H$METIER == "GNS_SPF_16-109_0_0", "GNS_SPF_16-31_0_0", table_H$METIER)
# .. Invalid code: OTM_DEF_>=105_1_120
table_H$METIER <- ifelse(table_H$METIER == "OTM_DEF_>=105_1_120", "OTM_DEF_105-115_1_120", table_H$METIER)
# .. Invalid code: OTM_SPF_16-104_0_0
table_H$METIER <- ifelse(table_H$METIER == "OTM_SPF_16-104_0_0", "OTM_SPF_16-31_0_0", table_H$METIER)
# .. Invalid code: PTM_SPF_16-104_0_0
table_H$METIER <- ifelse(table_H$METIER == "PTM_SPF_16-104_0_0", "PTM_SPF_16-31_0_0", table_H$METIER)
# .. Invalid code: OTB_DEF_>=105_1_120
table_H$METIER <- ifelse(table_H$METIER == "OTB_DEF_>=105_1_120", "OTB_DEF_115-120_0_0", table_H$METIER)



#-------------------------------------------------------------------------------
#                       3. Validate table H                       
#-------------------------------------------------------------------------------

# .. METIER ..
source("validateMetierOverall.R")
# ... import codelist from IcesVocab 
Metier6FishingActivity <- getCodeList("Metier6_FishingActivity", date = NULL)
# .. validate metier in table G 
validateMetierOverall(table_H, Metier6FishingActivity)

#                               @TODO  


#-------------------------------------------------------------------------------
#                       4. Write table H                       
#-------------------------------------------------------------------------------

# ... preparation for writing 
colnames(table_H) <- toupper(colnames(table_H))

table_H$RECTANGLE_TYPE <- "05*1"

table_H$C_SQUARE <- "NA"

# ... convert quarters to char
table_H$QUARTER <- as.character(table_H$QUARTER)

# ... changes in colnames in 2023 
table_H$LATITUDE <- table_H$RECTANGLE_LAT
table_H$LONGITUDE <- table_H$RECTANGLE_LON


# ... select & order cols 
table_H <- table_H %>% select(COUNTRY,	YEAR,	QUARTER,	VESSEL_LENGTH,	FISHING_TECH,	GEAR_TYPE,	TARGET_ASSEMBLAGE,
                                MESH_SIZE_RANGE,	METIER,	METIER_7,	SUPRA_REGION,	SUB_REGION,	EEZ_INDICATOR,	GEO_INDICATOR,
                                SPECON_TECH,	DEEP,	RECTANGLE_TYPE,	LATITUDE,	LONGITUDE,	C_SQUARE,	SPECIES,	TOTWGHTLANDG,	
                                TOTVALLANDG,	CONFIDENTIAL)


# NOTE 2023: a missing value in MESH_SIZE_RANGE is not anymore allowed if gear is GNS 
#            The problem -> NA values are refering to trap code 25 = Net, unknown


# .. modify 
table_H$MESH_SIZE_RANGE <- ifelse(is.na(table_H$MESH_SIZE_RANGE) & table_H$METIER == "GNS_ANA_>=157_0_0", 
                                  '157DXX', table_H$MESH_SIZE_RANGE)


table_H$MESH_SIZE_RANGE <- ifelse(is.na(table_H$MESH_SIZE_RANGE) & table_H$METIER == "GNS_FWS_>0_0_0", 
                                  '16D32', table_H$MESH_SIZE_RANGE)

table(table_H$MESH_SIZE_RANGE, useNA = 'always')


# ... write delivery to csv (orig-folder)
write.csv(table_H, paste0(path_tableh,.Platform$file.sep,"H_table_2013_2022.csv"), row.names = FALSE)

# save table H
#write.xlsx(table_H, paste0(path_out,.Platform$file.sep,"FIN_TABLE_H_LANDINGS_BY_RECTANGLE.xlsx"), sheetName = "TABLE_H", col.names = TRUE, row.names = FALSE)

## AKY: the line above did not work for me (some java related memory issue)
## Antti: As for me -> we could fix this later 

openxlsx::write.xlsx(table_H, paste0(path_out,.Platform$file.sep,"FIN_TABLE_H_LANDINGS_BY_RECTANGLE.xlsx"), sheetName = "TABLE_H", colNames = TRUE, rowNames = FALSE)
