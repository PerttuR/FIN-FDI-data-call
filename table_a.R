#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE A
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
#
# Date: JUL-2019
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for preparing FDI data tables from Table A from statistical DEP (Pirkko)
#-------------------------------------------------------------------------------


# install.packages("RPostgreSQL")
# install.packages("dplyr")
# install.packages("xlsx")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)
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
#                       1. import TABLE A and save it as .xlsx                       
#-------------------------------------------------------------------------------
setwd(path_tablea)

# import table A
table_A <- read.csv2("FIN_TABLE_A_CATCH.csv", sep = "," )

# rename columns to UPPER and save as .xlsx
setwd(path_out)
colnames(table_A)    <- c("COUNTRY", "YEAR", "QUARTER", "VESSEL_LENGTH", "FISHING_TECH", "GEAR_TYPE", "TARGET_ASSEMBLAGE", "MESH_SIZE_RANGE", "METIER", "DOMAIN_DISCARDS", "DOMAIN_LANDINGS", "SUPRA_REGION", "SUB_REGION", "EEZ_INDICATOR", "GEO_INDICATOR", "SPECON_TECH", "DEEP", "SPECIES", "TOTWGHTLANDG", "TOTVALLANDG", "DISCARDS", "CONFIDENTIAL")
write.xlsx(table_A, "TABLE_A_CATCH.xlsx", sheetName = "TABLE_A", col.names = TRUE, row.names = FALSE)

#-------------------------------------------------------------------------------
