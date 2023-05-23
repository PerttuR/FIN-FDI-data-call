#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE H
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa, Anna-Kaisa Ylitalo
#
# Date: JUN-2018
# Updated: JUN 2021 by Perttu
#          JUN-2022 by Anna-Kaisa and Perttu
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
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2022")


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
table_H$METIER_7 <- NA



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

# ... select & order cols 
table_H <- table_H %>% select(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, 
                              TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, METIER_7, SUPRA_REGION, SUB_REGION, 
                              EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP, RECTANGLE_TYPE, RECTANGLE_LAT, 
                              RECTANGLE_LON, C_SQUARE, SPECIES, TOTWGHTLANDG, TOTVALLANDG, CONFIDENTIAL)

# save table H
write.xlsx(table_H, paste0(path_out,.Platform$file.sep,"TABLE_H_LANDINGS_BY_RECTANGLE.xlsx"), sheetName = "TABLE_H", col.names = TRUE, row.names = FALSE)

## AKY: the line above did not work for me (some java related memory issue)
#library(openxlsx)
#openxlsx::write.xlsx(table_H, paste0(path_out,.Platform$file.sep,"TABLE_H_LANDINGS_BY_RECTANGLE.xlsx"), sheetName = "TABLE_H", colNames = TRUE, rowNames = FALSE)
