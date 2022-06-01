#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE A
#
# Coded: Antti Sykkö, Perttu Rantanen, Mira Sustar, Petri Sarvamaa
#
# Creation Date: JUN-2022 Antti, Anna-Kaisa, Perttu
# 
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for preparing FDI data tables from Table A from statistical DEP (Pirkko)
#-------------------------------------------------------------------------------



#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)
library(xlsx)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------


# Common paths & 2022 folder:
path_tablea <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- getwd() # folder where the r project is (and the source file db.R!)
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2022")

# import table G
table_G <- read.csv2(paste0(path_tablea,.Platform$file.sep,"G_table_2013_2021.csv"), sep = "," , na.strings = "")

#select order of columns
table_G <- table_G %>% select("COUNTRY", "YEAR", "QUARTER", "VESSEL_LENGTH", "FISHING_TECH", "GEAR_TYPE", "TARGET_ASSEMBLAGE", "MESH_SIZE_RANGE", "METIER", "SUPRA_REGION", "SUB_REGION", "EEZ_INDICATOR", "GEO_INDICATOR", "SPECON_TECH", "DEEP", "TOTSEADAYS", "TOTKWDAYSATSEA", "TOTGTDAYSATSEA", "TOTFISHDAYS", "TOTKWFISHDAYS", "TOTGTFISHDAYS", "HRSEA", "KWHRSEA", "GTHRSEA", "TOTVES", "CONFIDENTIAL")

# rename columns to UPPER and save as .xlsx
colnames(table_A)    <- c("COUNTRY", "YEAR", "QUARTER", "VESSEL_LENGTH", "FISHING_TECH", "GEAR_TYPE", "TARGET_ASSEMBLAGE", "MESH_SIZE_RANGE", "METIER", "SUPRA_REGION", "SUB_REGION", "EEZ_INDICATOR", "GEO_INDICATOR", "SPECON_TECH", "DEEP", "TOTSEADAYS", "TOTKWDAYSATSEA", "TOTGTDAYSATSEA", "TOTFISHDAYS", "TOTKWFISHDAYS", "TOTGTFISHDAYS", "HRSEA", "KWHRSEA", "GTHRSEA", "TOTVES", "CONFIDENTIAL")

write.xlsx(table_A,paste0(path_out,.Platform$file.sep,"TABLE_G_EFFORT.xlsx"), sheetName = "TABLE_G", col.names = TRUE, row.names = FALSE)

