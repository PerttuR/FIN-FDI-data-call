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

# libraries
library(dplyr)
library(xlsx)
library(icesVocab)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------


# Common paths & 2022 folder:
path_tableg <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where csv tables located 


path_rproject <- getwd() # folder where the r project is (and the source file db.R!)
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2023")


path_out

#-------------------------------------------------------------------------------
#                   1. Import table G                       
#-------------------------------------------------------------------------------

# import table G
table_G <- read.csv2(paste0(path_tableg,.Platform$file.sep,"G_table_2013_2022_SAS.csv"), sep = "," , na.strings = "")
 
# .. add empty col for new metier 
table_G$METIER_7 <- 'NA'

# ... order cols 
table_G <- table_G[, c("COUNTRY", "YEAR", "QUARTER", "VESSEL_LENGTH", "FISHING_TECH", 
                              "GEAR_TYPE", "TARGET_ASSEMBLAGE", "MESH_SIZE_RANGE", "METIER", "METIER_7" ,"SUPRA_REGION", 
                              "SUB_REGION", "EEZ_INDICATOR", "GEO_INDICATOR", "SPECON_TECH", "DEEP", "TOTSEADAYS", 
                              "TOTKWDAYSATSEA", "TOTGTDAYSATSEA", "TOTFISHDAYS", "TOTKWFISHDAYS", "TOTGTFISHDAYS", 
                              "HRSEA", "KWHRSEA", "GTHRSEA", "TOTVES", "CONFIDENTIAL")]
 


#-------------------------------------------------------------------------------
#                   2. Modify table G                       
#-------------------------------------------------------------------------------

# table(table_G$METIER, useNA = 'always', table_G$YEAR)

# .... modify invalid metier codes 
# View(Metier6FishingActivity[grep(paste0("^", "OTM_DEF_"), Metier6FishingActivity$Key), ])


# .. Invalid code: GNS_SPF_16-109_0_0
table_G$METIER <- ifelse(table_G$METIER == "GNS_SPF_16-109_0_0", "GNS_SPF_16-31_0_0", table_G$METIER)
# .. Invalid code: OTM_DEF_>=105_1_120
table_G$METIER <- ifelse(table_G$METIER == "OTM_DEF_>=105_1_120", "OTM_DEF_105-115_1_120", table_G$METIER)
# .. Invalid code: OTM_SPF_16-104_0_0
table_G$METIER <- ifelse(table_G$METIER == "OTM_SPF_16-104_0_0", "OTM_SPF_16-31_0_0", table_G$METIER)
# .. Invalid code: PTM_SPF_16-104_0_0
table_G$METIER <- ifelse(table_G$METIER == "PTM_SPF_16-104_0_0", "PTM_SPF_16-31_0_0", table_G$METIER)
# .. Invalid code: OTB_DEF_>=105_1_120
table_G$METIER <- ifelse(table_G$METIER == "OTB_DEF_>=105_1_120", "OTB_DEF_115-120_0_0", table_G$METIER)


#-------------------------------------------------------------------------------
#                   3. Validate table G                       
#-------------------------------------------------------------------------------

# .. METIER ..
source("validateMetierOverall.R")
# ... import codelist from IcesVocab 
Metier6FishingActivity <- getCodeList("Metier6_FishingActivity", date = NULL)
# .. validate metier in table G 
validateMetierOverall(table_G, Metier6FishingActivity)




#-------------------------------------------------------------------------------
#                   X. Write table G                       
#-------------------------------------------------------------------------------

# ... write delivery to csv (orig-folder)
write.csv(table_G, paste0(path_tableg,.Platform$file.sep,"G_table_2013_2022.csv"), row.names = FALSE)

# ... write del as xlsx to results folder 
write.xlsx(table_G,paste0(path_out,.Platform$file.sep,"TABLE_G_EFFORT.xlsx"), sheetName = "TABLE_G", col.names = TRUE, row.names = FALSE)

