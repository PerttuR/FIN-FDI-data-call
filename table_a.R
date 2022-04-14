#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE A
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
#
# Creation Date: JUL-2019
# Updated: JUN 2021 (Perttu)
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


# Common paths & 2022 folder:
path_tablea <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- getwd() # folder where the r project is (and the source file db.R!)
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2022")

#-------------------------------------------------------------------------------
#                       1. import TABLE A and save it as .xlsx                       
#-------------------------------------------------------------------------------

# import table A
table_A <- read.csv2(paste0(path_tablea,.Platform$file.sep,"A_table_2014_2020.csv"), sep = "," , na.strings = "")

#select order of columns
table_A <- table_A %>% select(COUNTRY,	YEAR, QUARTER, VESSEL_LENGTH,	FISHING_TECH,	GEAR_TYPE,	TARGET_ASSEMBLAGE,	MESH_SIZE_RANGE,	METIER,	DOMAIN_DISCARDS,	DOMAIN_LANDINGS,	SUPRA_REGION,	SUB_REGION,	EEZ_INDICATOR,	GEO_INDICATOR,	NEP_SUB_REGION,	SPECON_TECH,	DEEP,	SPECIES,	TOTWGHTLANDG,	TOTVALLANDG,	DISCARDS,	CONFIDENTIAL)

# rename columns to UPPER and save as .xlsx
colnames(table_A)    <- c("COUNTRY",	"YEAR",	"QUARTER",	"VESSEL_LENGTH",	"FISHING_TECH",	"GEAR_TYPE",	"TARGET_ASSEMBLAGE",	"MESH_SIZE_RANGE",	"METIER",	"DOMAIN_DISCARDS",	"DOMAIN_LANDINGS",	"SUPRA_REGION",	"SUB_REGION",	"EEZ_INDICATOR",	"GEO_INDICATOR",	"NEP_SUB_REGION",	"SPECON_TECH",	"DEEP",	"SPECIES",	"TOTWGHTLANDG",	"TOTVALLANDG",	"DISCARDS",	"CONFIDENTIAL")



# rounding the number to three digits precision

table_A$TOTWGHTLANDG <- round(as.numeric(as.character(table_A$TOTWGHTLANDG)), digits = 3)
table_A$TOTVALLANDG <- round(as.numeric(as.character(table_A$TOTVALLANDG)), digits = 3)
table_A$DISCARDS <- round(as.numeric(as.character(table_A$DISCARDS)), digits = 3)

#quartes to string
table_A$QUARTER <- as.character(table_A$QUARTER)

#FDI database did not allow "=" in DOMAINS.. now fixed
#library(stringr)
#table_A <- table_A %>% filter(!str_detect(DOMAIN_DISCARDS, "=") )
#table_A <- table_A %>% filter(!str_detect(DOMAIN_LANDINGS, "=") )

#METIER set NK when TARGET_ASSEMBLAGE == NK
#table_A <- table_A %>% mutate(METIER = replace(as.character(METIER), which(as.character(METIER)=="MIS_MIS_0_0_0" & as.character(TARGET_ASSEMBLAGE) == "NK") , "NK"))

write.xlsx(table_A,paste0(path_out,.Platform$file.sep,"TABLE_A_CATCH.xlsx"), sheetName = "TABLE_A", col.names = TRUE, row.names = FALSE)

#-------------------------------------------------------------------------------
