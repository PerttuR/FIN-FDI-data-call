#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE J
#
# Coded: Antti Sykkö
#
# Date: MAY 2023
# Updated: JUNE 2024 (Mira Sustar)
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is to prepare FDI data table J 
#-------------------------------------------------------------------------------


# install.packages("RPostgreSQL")
# install.packages("dplyr")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)
library(magrittr)
library(openxlsx)
library(icesVocab)
library(RPostgres)
library(tidyr)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------
# Output folder
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2024")
path_der <- paste0(getwd(), .Platform$file.sep, "der/2024/")

#-------------------------------------------------------------------------------
#                       1. Import and modify table J                    
#-------------------------------------------------------------------------------
source("db.r")

## Read in data
# alter based on the date of the postgres schema date
schemadate <- "2024-06-14"


# Postgres
kapasiteetti <- read.dbTable(schema=paste(schemadate, "-dcprod", sep = ""), table='kapasiteetti', dbname = "kake_siirto")

Table J. Capacity and fleet segment effort
1. COUNTRY: According to the code list provided in Appendix 1; missing values not allowed.
2. YEAR: Four digits (e.g. 2019); missing values not allowed.
3. VESSEL_LENGTH: According to the code list provided in Appendix 2; ‘NK’ if not known.
4. FISHING_TECH: According to the code list provided in Appendix 3; missing values not allowed.
5. SUPRA_REGION: According to the code list in Appendix 9; missing values not allowed.
6. GEO_INDICATOR: According to the code list in Appendix 10; ‘NK’ if not known.
7. PRINCIPAL_SUB_REGION: Sub-region where the vessel has its majority fishing activity during the year. Principal sub-region has to be calculated vessel by vessel (i.e. calculated at the vessel level) and, in order to define the principal sub-region of a vessel, the metric number of fishing days should be used. According to the code list in Appendix 9; ‘NK’ if not known.
8. TOTTRIPS: [integer] Number of fishing trips by the vessels in the fleet segment from a land location to a landing place, excluding non-fishing trips; ‘NK’ if not known.
9. TOTKW: Fishing capacity in kW of the vessels in the fleet segment; if not available use ‘NK’.
10. TOTGT: Fishing capacity in gross tonnage of the vessels in the fleet segment; if not available use ‘NK’.
11. TOTVES: [integer] Number of vessels in the fleet segment11; if not available use ‘NK’.
12. AVGAGE: Average age [in years] of the vessels in the fleet segment; if not available use ‘NK’.
13. AVGLOA: Average length over all [in metres] of the vessels in the fleet segment; if not available use ‘NK’.
14. MAXSEADAYS: Average number of days at sea of the top 10 most active vessels in the fleet segment; if not available use ‘NK’.



#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#                       4. Write table J                       
#-------------------------------------------------------------------------------


# save table H
write.xlsx(table_J, paste0(path_out,.Platform$file.sep,"FIN_TABLE_J_CAPACITY.xlsx"), 
           sheetName = "TABLE_J", colNames = TRUE, rowNames = FALSE)

