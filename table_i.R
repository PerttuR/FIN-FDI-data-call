#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE I
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
# library(vmstools) not available for R 4.1.2
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
#                       1. read table I to R                       
#-------------------------------------------------------------------------------

# import table I
table_I <- read.csv2(paste0(path_tablea,"I_table_2013_2021.csv"), sep = "," ,na.strings="")

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#                       2. define coordinates                       
#-------------------------------------------------------------------------------

source("spatial.R")


midpoints <- latlon(table_I$RECTANGLE,midpoint=TRUE)


table_I <- tibble::rowid_to_column(table_I, "ID")
midpoints <- tibble::rowid_to_column(midpoints, "ID")


table_I <- left_join(table_I, midpoints,copy = TRUE, by="ID")

table_I <- table_I %>% rename(RECTANGLE_LAT = SI_LATI, RECTANGLE_LON = SI_LONG)


#-------------------------------------------------------------------------------
#                       3. prepare file for upload and save to path                       
#-------------------------------------------------------------------------------


names(table_I) %<>% toupper
table_I$RECTANGLE_TYPE <- "05*1"
table_I$C_SQUARE <- "NA"

#quartes to string
table_I$QUARTER <- as.character(table_I$QUARTER)


table_I <- table_I %>% select(COUNTRY, YEAR, QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, METIER, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP, RECTANGLE_TYPE, RECTANGLE_LAT, RECTANGLE_LON, C_SQUARE, TOTFISHDAYS, CONFIDENTIAL)

table_I <-  table_I %>% mutate(VESSEL_LENGTH = replace(VESSEL_LENGTH, is.na(VESSEL_LENGTH), "NK"))

# save table I

write.xlsx(table_I, paste0(path_out,.Platform$file.sep,"TABLE_I_EFFORT_BY_RECTANGLE.xlsx"), sheetName = "TABLE_I", col.names = TRUE, row.names = FALSE)


