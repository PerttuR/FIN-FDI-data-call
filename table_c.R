#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE C
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa, Anna-Kaisa Ylitalo
#
# Create Date: JUN-2018
# Updated: MAY 2022 by Team
# Updated: MAY 2023 by Perttu
# Updated: JUN 2024 by Mira
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for preparing FDI data tables from Table A from statistical DEP (Pirkko)
# Table C:includes EU-DCF samples from Suomu DB. No samples from LohiDB, since discards do not exist there
#-------------------------------------------------------------------------------


# install.packages("RPostgreSQL")
# install.packages("dplyr")
#install.packages("xlsx")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)
library(openxlsx)
library(icesVocab)


#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------

# Output folder
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2024")
path_der <- paste0(getwd(), .Platform$file.sep, "der/2024/")
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                       1. Import data for 2023 C table needs                    
#-------------------------------------------------------------------------------

# import table A
table_A <- readRDS(paste0(path_der,.Platform$file.sep,"table_A.rds"))


#-------------------------------------------------------------------------------
#                   2. TABLE C (NAO OFR. Discards age data)                       
#-------------------------------------------------------------------------------

table_C <- table_A %>% select(COUNTRY, YEAR, DOMAIN_DISCARDS, NEP_SUB_REGION, SPECIES, TOTWGHTLANDG) %>% distinct() %>% 
  mutate(
    DISCARDS = 0,
    DISCARD_CV = "NK",
    DISCARD_CI_UPPER = "NK",
    DISCARD_CI_LOWER = "NK",
    TOTAL_TRIPS = "NK",
    TOTAL_SAMPLED_TRIPS = "NK",
    NO_AGE_MEASUREMENTS = "NK",
    AGE_MEASUREMENTS_PROP = "NA",
    MIN_AGE = "NK",
    MAX_AGE = "NK",
    AGE = "NK",
    NO_AGE = "NK",
    MEAN_WEIGHT = "NK",
    WEIGHT_UNIT = "NK",
    MEAN_LENGTH = "NK",
    LENGTH_UNIT = "NK"
  ) %>% filter(SPECIES == c("HER", "SPR"))



openxlsx::write.xlsx(table_C, paste0(path_out,.Platform$file.sep,"TABLE_C_NAO_OFR_DISCARDS_AGE.xlsx"), sheetName = "TABLE_C", colNames = TRUE, rowNames = FALSE)


