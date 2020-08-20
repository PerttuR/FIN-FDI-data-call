#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE H
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
#
# Date: JUN-2018
# Updated: JUL 2019 by Perttu
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
path_tablea <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call/orig" # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call" # folder where the r project is (and the source file db.R!)
path_out <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call/results" # folder where the output is saved

#-------------------------------------------------------------------------------
#                       1. read table H to R                       
#-------------------------------------------------------------------------------
setwd(path_tablea)

# import table H
table_H <- read.csv2("TABLE_H_LANDINGS.csv", sep = "," ,na.strings=""  )
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                       2. define coordinates                       
#-------------------------------------------------------------------------------

setwd(path_rproject)
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

# set working directory to save table H
setwd(path_out)
write.xlsx(table_H, "TABLE_H_LANDINGS_BY_RECTANGLE.xlsx", sheetName = "TABLE_H", col.names = TRUE, row.names = FALSE)
