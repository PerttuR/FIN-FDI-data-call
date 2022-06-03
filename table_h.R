#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE H
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa, Anna-Kaisa Ylitalo
#
# Date: JUN-2018
# Updated: JUN 2021 by Perttu
#          JUN-2022 by Team
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
library(vmstools)
library(magrittr)
library(xlsx)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------
# Common paths & 2022 folder:
path_tablea <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_salmon <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where salmon data lies (salmon.csv)
path_rproject <- getwd() # folder where the r project is (and the source file db.R!)
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2022")


#-------------------------------------------------------------------------------
#                       1. read table H to R                       
#-------------------------------------------------------------------------------

# import table H
table_H <- read.csv2(paste0(path_tablea,"H_table_2013_2021.csv"), sep = "," ,na.strings="")

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                       2. define coordinates                       
#-------------------------------------------------------------------------------

source("spatial.R")

midpoints <- latlon(table_H$RECTANGLE,midpoint=TRUE)

table_H <- tibble::rowid_to_column(table_H, "ID")
midpoints <- tibble::rowid_to_column(midpoints, "ID")

table_H <- left_join(table_H, midpoints,copy = TRUE)

table_H <- table_H %>% rename(RECTANGLE_LAT = SI_LATI, RECTANGLE_LON = SI_LONG)



#-------------------------------------------------------------------------------
#                       3. prepare file for upload and save to path                       
#-------------------------------------------------------------------------------


names(table_H) %<>% toupper
table_H$RECTANGLE_TYPE <- "05*1"

table_H$C_SQUARE <- "NA"
#quartes to string
table_H$QUARTER <- as.character(table_H$QUARTER)

table_H <- table_H %>% select(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP, RECTANGLE_TYPE, RECTANGLE_LAT, RECTANGLE_LON, C_SQUARE, SPECIES, TOTWGHTLANDG, TOTVALLANDG, CONFIDENTIAL)

# save table H
write.xlsx(table_H, paste0(path_out,.Platform$file.sep,"TABLE_H_LANDINGS_BY_RECTANGLE.xlsx"), sheetName = "TABLE_H", col.names = TRUE, row.names = FALSE)

## AKY: the line above did not work for me (some java related memory issue)
library(openxlsx)
openxlsx::write.xlsx(table_H, paste0(path_out,.Platform$file.sep,"TABLE_H_LANDINGS_BY_RECTANGLE.xlsx"), sheetName = "TABLE_H", colNames = TRUE, rowNames = FALSE)
