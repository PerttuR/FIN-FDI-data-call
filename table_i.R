#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE I
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
#
# Date: JUN-2018
# Updated: JUN 2021 by Perttu
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
# Mira:
path_tablea <- "C:/2018/FDI/work/data/orig/" # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- "C:/2018/FDI/work/prog/FIN-FDI-data-call/" # folder where the r project is (and the source file db.R!)
path_out <- "C:/2018/FDI/work/data/der/" # folder where the output is saved

# Perttu:
path_tablea <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- getwd() # folder where the r project is (and the source file db.R!)
path_salmon <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where salmon data lies (salmon.csv)
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep, "results", .Platform$file.sep, "2021")

# create directories if missing, but ignore warnings in case they already exist
dir.create(path_tablea, showWarnings = FALSE)
dir.create(path_out, showWarnings = FALSE)
#-------------------------------------------------------------------------------
#                       1. read table I to R                       
#-------------------------------------------------------------------------------
setwd(path_tablea)

# import table I
table_I <- read.csv2("I_table_2014_and_2020.csv", sep = ",",na.strings=""   )
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#                       2. define coordinates                       
#-------------------------------------------------------------------------------
setwd(path_rproject)
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

# set working directory to save table H
setwd(path_out)
write.xlsx(table_I, "TABLE_I_EFFORT_BY_RECTANGLE.xlsx", sheetName = "TABLE_I", col.names = TRUE, row.names = FALSE)


