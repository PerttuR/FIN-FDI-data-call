#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE A
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa, Antti Sykkö
#
# Creation Date: JUL-2019
# Updated: JUN 2021 (Perttu)
# Updated: JUN 2022 (Perttu)
# Updated: MAY 2023 (Antti)
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for preparing FDI data tables from Table A from statistical DEP
#-------------------------------------------------------------------------------


# install.packages("RPostgreSQL")
# install.packages("dplyr")
# install.packages("xlsx")


# Add extension _sas to the csv's in orig-folder 

#- Clear workspace
rm(list=ls())

# needed libraries .. testi
library(dplyr)
library(xlsx)
library(icesVocab)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------


# Common paths & 2022 folder:
path_tablea <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- getwd() # folder where the r project is (and the source file db.R!)
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2023")
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                       1. Import table A                      
#-------------------------------------------------------------------------------

# import table A
table_A <- read.csv2(paste0(path_tablea,.Platform$file.sep,"A_table_2013_2022_SAS.csv"), sep = "," , na.strings = "")

#select order of columns
table_A <- table_A %>% select(COUNTRY,	YEAR, QUARTER, VESSEL_LENGTH,	FISHING_TECH,	GEAR_TYPE,	TARGET_ASSEMBLAGE,	MESH_SIZE_RANGE,	METIER,	DOMAIN_DISCARDS,	DOMAIN_LANDINGS,	SUPRA_REGION,	SUB_REGION,	EEZ_INDICATOR,	GEO_INDICATOR,	NEP_SUB_REGION,	SPECON_TECH,	DEEP,	SPECIES,	TOTWGHTLANDG,	TOTVALLANDG,	DISCARDS,	CONFIDENTIAL)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#                       2. Modify table A                      
#-------------------------------------------------------------------------------

# rename columns to UPPER and save as .xlsx
colnames(table_A)    <- c("COUNTRY",	"YEAR",	"QUARTER",	"VESSEL_LENGTH",	"FISHING_TECH",	"GEAR_TYPE",	"TARGET_ASSEMBLAGE",	"MESH_SIZE_RANGE",	"METIER",	"DOMAIN_DISCARDS",	"DOMAIN_LANDINGS",	"SUPRA_REGION",	"SUB_REGION",	"EEZ_INDICATOR",	"GEO_INDICATOR",	"NEP_SUB_REGION",	"SPECON_TECH",	"DEEP",	"SPECIES",	"TOTWGHTLANDG",	"TOTVALLANDG",	"DISCARDS",	"CONFIDENTIAL")



# ... rounding the number to three digits precision
table_A$TOTWGHTLANDG <- round(as.numeric(as.character(table_A$TOTWGHTLANDG)), digits = 3)
table_A$TOTVALLANDG <- round(as.numeric(as.character(table_A$TOTVALLANDG)), digits = 3)
table_A$DISCARDS <- round(as.numeric(as.character(table_A$DISCARDS)), digits = 3)

# ... quartes to string
table_A$QUARTER <- as.character(table_A$QUARTER)


# METIER
# .. Invalid code: GNS_SPF_16-109_0_0
table_A$METIER <- ifelse(table_A$METIER == "GNS_SPF_16-109_0_0", "GNS_SPF_16-31_0_0", table_A$METIER)
# .. Invalid code: OTM_DEF_>=105_1_120
table_A$METIER <- ifelse(table_A$METIER == "OTM_DEF_>=105_1_120", "OTM_DEF_105-115_1_120", table_A$METIER)
# .. Invalid code: OTM_SPF_16-104_0_0
table_A$METIER <- ifelse(table_A$METIER == "OTM_SPF_16-104_0_0", "OTM_SPF_16-31_0_0", table_A$METIER)
# .. Invalid code: PTM_SPF_16-104_0_0
table_A$METIER <- ifelse(table_A$METIER == "PTM_SPF_16-104_0_0", "PTM_SPF_16-31_0_0", table_A$METIER)
# .. Invalid code: OTB_DEF_>=105_1_120
table_A$METIER <- ifelse(table_A$METIER == "OTB_DEF_>=105_1_120", "OTB_DEF_115-120_0_0", table_A$METIER)


# ... add new field for METIER_7
table_A$METIER_7 <- 'NA'




#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                       3. Validate table A                      
#-------------------------------------------------------------------------------

# .. METIER ..
source("validateMetierOverall.R")
# ... import codelist from IcesVocab 
Metier6FishingActivity <- getCodeList("Metier6_FishingActivity", date = NULL)
# .. validate metier in table G 
validateMetierOverall(table_A, Metier6FishingActivity)


#FDI database did not allow "=" in DOMAINS.. now fixed
#library(stringr)
#table_A <- table_A %>% filter(!str_detect(DOMAIN_DISCARDS, "=") )
#table_A <- table_A %>% filter(!str_detect(DOMAIN_LANDINGS, "=") )

#METIER set NK when TARGET_ASSEMBLAGE == NK
#table_A <- table_A %>% mutate(METIER = replace(as.character(METIER), which(as.character(METIER)=="MIS_MIS_0_0_0" & as.character(TARGET_ASSEMBLAGE) == "NK") , "NK"))


# ... order columns 
table_A <- table_A[, c("COUNTRY","YEAR","QUARTER","VESSEL_LENGTH","FISHING_TECH",
                       "GEAR_TYPE","TARGET_ASSEMBLAGE","MESH_SIZE_RANGE","METIER",
                       "METIER_7","DOMAIN_DISCARDS","DOMAIN_LANDINGS","SUPRA_REGION",
                       "SUB_REGION","EEZ_INDICATOR","GEO_INDICATOR","NEP_SUB_REGION",
                       "SPECON_TECH","DEEP","SPECIES","TOTWGHTLANDG","TOTVALLANDG",
                       "DISCARDS","CONFIDENTIAL")]


#-------------------------------------------------------------------------------
#                       4. Write table A                      
#-------------------------------------------------------------------------------

# ... write delivery to csv (orig-folder)
write.csv(table_A, paste0(path_tablea,.Platform$file.sep,"A_table_2013_2022.csv"), row.names = FALSE)


# ... write delivery to xlsx file 
write.xlsx(table_A,paste0(path_out,.Platform$file.sep,"FIN_TABLE_A_CATCH.xlsx"), 
           sheetName = "TABLE_A", col.names = TRUE, row.names = FALSE)


#-------------------------------------------------------------------------------
