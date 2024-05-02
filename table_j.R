

#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE J
#
# Coded: Antti Sykkö
#
# Date: MAY 2023
# Updated: 
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is ... 
#-------------------------------------------------------------------------------


# install.packages("RPostgreSQL")
# install.packages("dplyr")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)
#library(magrittr)
library(openxlsx)
library(icesVocab)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------
# Common paths & 2022 folder:
path_tablej <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where TABLE J is
# Output folder
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2023")


#-------------------------------------------------------------------------------
#                       1. Import table J                    
#-------------------------------------------------------------------------------

# ... import table J

table_J <- read.csv2(paste0(path_tablej,"J_table_2013_2022.csv"), sep = "," ,na.strings="")


#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
#                       2. Modify table J                    
#-------------------------------------------------------------------------------


#                         @TODO if needed 



#-------------------------------------------------------------------------------
#                       3. Validate table J                       
#-------------------------------------------------------------------------------


# .. check that correct cols exist & order 
table_J <- table_J[, c("COUNTRY","YEAR","VESSEL_LENGTH","FISHING_TECH",
                       "SUPRA_REGION","GEO_INDICATOR","PRINCIPAL_SUB_REGION",
                       "TOTTRIPS","TOTKW","TOTGT","TOTVES",
                       "AVGAGE","AVGLOA","MAXSEADAYS")]


#-------------------------------------------------------------------------------
#                       4. Write table J                       
#-------------------------------------------------------------------------------

# ... preparation for writing 
colnames(table_J) <- toupper(colnames(table_J))

# ... write delivery to csv (orig-folder)
write.csv(table_J, paste0(path_tablej,.Platform$file.sep,"J_table_2013_2022.csv"), row.names = FALSE)

# save table H
write.xlsx(table_J, paste0(path_out,.Platform$file.sep,"FIN_TABLE_J_CAPACITY.xlsx"), 
           sheetName = "TABLE_J", colNames = TRUE, rowNames = FALSE)

