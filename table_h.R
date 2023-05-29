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
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for futher prepare FDI data table H from partial Table H from statistical DEP (Pirkko)
#-------------------------------------------------------------------------------


# install.packages("RPostgreSQL")
# install.packages("dplyr")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)
library(magrittr)
library(xlsx)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------
# Common paths & 2022 folder:
path_tableh <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where TABLE H is
# Output folder
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2023")


#-------------------------------------------------------------------------------
#                       1. Import table H                    
#-------------------------------------------------------------------------------

# import table H
table_H <- read.csv2(paste0(path_tableh,"H_table_2013_2022.csv"), sep = "," ,na.strings="")


#-------------------------------------------------------------------------------



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


# save table H
#write.xlsx(table_H, paste0(path_out,.Platform$file.sep,"FIN_TABLE_H_LANDINGS_BY_RECTANGLE.xlsx"), sheetName = "TABLE_H", col.names = TRUE, row.names = FALSE)

## AKY: the line above did not work for me (some java related memory issue)
## Antti: As for me -> we could fix this later 
library(openxlsx)
openxlsx::write.xlsx(table_H, paste0(path_out,.Platform$file.sep,"FIN_TABLE_H_LANDINGS_BY_RECTANGLE.xlsx"), sheetName = "TABLE_H", colNames = TRUE, rowNames = FALSE)
