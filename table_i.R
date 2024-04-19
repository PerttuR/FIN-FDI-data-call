#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE I
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa, Anna-Kaisa Ylitalo, Antti Sykkö
#
# Date: JUN-2018
# Updated: JUN 2021 by Perttu
#          JUN-2022 by Anna-Kaisa, Perttu
#          JUN-2023 by Antti
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for futher prepare FDI data table I from partial Table H from statistical DEP (Pirkko)
#-------------------------------------------------------------------------------


# install.packages("RPostgreSQL")
# install.packages("dplyr")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)
# library(vmstools) not available for R 4.1.2
library(magrittr)
library(openxlsx)
library(icesVocab)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------
# Common paths & 2022 folder:
path_tablei <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where TABLE I is 
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2023")

#-------------------------------------------------------------------------------
#                       1. Import table I                        
#-------------------------------------------------------------------------------

# .. import table I
table_I <- read.csv2(paste0(path_tablei,"I_table_2013_2022.csv"), sep = "," ,na.strings="")

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#                       2. Modify table I                  
#-------------------------------------------------------------------------------


# .. add empty col for new metier 
table_I$METIER_7 <- 'NA'

# ... def coordinates 
source("spatial.R")

midpoints <- latlon(table_I$RECTANGLE,midpoint=TRUE)

table_I <- tibble::rowid_to_column(table_I, "ID")
midpoints <- tibble::rowid_to_column(midpoints, "ID")

table_I <- left_join(table_I, midpoints,copy = TRUE, by="ID")
table_I <- table_I %>% rename(LATITUDE = SI_LATI, LONGITUDE = SI_LONG)



# .. Invalid code: GNS_SPF_16-109_0_0
table_I$METIER <- ifelse(table_I$METIER == "GNS_SPF_16-109_0_0", "GNS_SPF_16-31_0_0", table_I$METIER)
# .. Invalid code: OTM_DEF_>=105_1_120
table_I$METIER <- ifelse(table_I$METIER == "OTM_DEF_>=105_1_120", "OTM_DEF_105-115_1_120", table_I$METIER)
# .. Invalid code: OTM_SPF_16-104_0_0
table_I$METIER <- ifelse(table_I$METIER == "OTM_SPF_16-104_0_0", "OTM_SPF_16-31_0_0", table_I$METIER)
# .. Invalid code: PTM_SPF_16-104_0_0
table_I$METIER <- ifelse(table_I$METIER == "PTM_SPF_16-104_0_0", "PTM_SPF_16-31_0_0", table_I$METIER)
# .. Invalid code: OTB_DEF_>=105_1_120
table_I$METIER <- ifelse(table_I$METIER == "OTB_DEF_>=105_1_120", "OTB_DEF_115-120_0_0", table_I$METIER)



#-------------------------------------------------------------------------------
#                       3. Validate table I                       
#-------------------------------------------------------------------------------
# .. METIER ..
source("validateMetierOverall.R")
# ... import codelist from IcesVocab 
Metier6FishingActivity <- getCodeList("Metier6_FishingActivity", date = NULL)
# .. validate metier in table G 
validateMetierOverall(table_I, Metier6FishingActivity)


#-------------------------------------------------------------------------------
#                       4. Write table I                       
#-------------------------------------------------------------------------------


names(table_I) %<>% toupper
table_I$RECTANGLE_TYPE <- "05*1"
table_I$C_SQUARE <- "NA"

# .. quarters to string
table_I$QUARTER <- as.character(table_I$QUARTER)


table_I <- table_I %>% select(COUNTRY,YEAR,QUARTER,VESSEL_LENGTH,FISHING_TECH,GEAR_TYPE,
                              TARGET_ASSEMBLAGE,MESH_SIZE_RANGE,METIER,METIER_7,SUPRA_REGION,
                              SUB_REGION,EEZ_INDICATOR,GEO_INDICATOR, SPECON_TECH,DEEP,
                              RECTANGLE_TYPE,LATITUDE,LONGITUDE,C_SQUARE,TOTFISHDAYS,CONFIDENTIAL)



table_I <-  table_I %>% mutate(VESSEL_LENGTH = replace(VESSEL_LENGTH, is.na(VESSEL_LENGTH), "NK"))

# ... write delivery to csv (orig-folder)
write.csv(table_I, paste0(path_tablei,.Platform$file.sep,"I_table_2013_2022.csv"), row.names = FALSE)

# .. save table I
openxlsx::write.xlsx(table_I, paste0(path_out,.Platform$file.sep,"FIN_TABLE_I_EFFORT_BY_RECTANGLE.xlsx"), sheetName = "TABLE_I", colNames = TRUE, rowNames = FALSE)


