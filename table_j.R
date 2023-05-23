

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
#library(dplyr)
#library(magrittr)
#library(xlsx)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------
# Common paths & 2022 folder:
path_tablej <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where TABLE J is
# Output folder
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2022")


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

View(table_J)


#-------------------------------------------------------------------------------
#                       3. Validate table J                       
#-------------------------------------------------------------------------------


#                               @TODO  


#-------------------------------------------------------------------------------
#                       4. Write table J                       
#-------------------------------------------------------------------------------

# ... preparation for writing 
colnames(table_J) <- toupper(colnames(table_J))

# save table H
write.xlsx(table_J, paste0(path_out,.Platform$file.sep,"TABLE_J_CAPACITY.xlsx"), 
           sheetName = "TABLE_J", col.names = TRUE, row.names = FALSE)

