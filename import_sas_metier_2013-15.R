# import SAS metier tables for 2013 to 2015
# Authors: J. Demmler, Perttu
# Date: 10/06/2025
# Last revision: 11/06/2025

#- Clear workspace
rm(list=ls())

library(tidyverse)
library(haven)
library(openxlsx)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------
# Common paths data call folders:

run.year = 2025

# Output folder
path_der <- paste0(getwd(), .Platform$file.sep, "der/", run.year,"/")


# get data from G drive ####
          
for (i in 2013:2015){
  
  tmp <- read_sas(paste0("G:/Luke2/Stat_kala_merikalastus/Metier/Data/pvkarvo",i,"_metier.sas7bdat")) |>
                  mutate(KALASTUSVUOSI = i)
  
  assign(paste0("metier_",i), tmp)
  
} 


# alternative path ####

for (i in 2013:2015){
  
  tmp <- read_sas(paste0("orig/pvkarvo",i,"_metier.sas7bdat")) |>
    mutate(KALASTUSVUOSI = i)
  
  assign(paste0("metier_",i), tmp)
  
} 

# combine into 1 table

metier_2013_15 <- bind_rows(metier_2013, metier_2014, metier_2015)

# filter is wanted
# metier_2013_15_FIN <- metier_2013_15 |> filter(grepl("FIN",alus))

# remove temporary files
rm(metier_2013, metier_2014, metier_2015, tmp)
invisible(gc())

#save to der folder
saveRDS(metier_2013_15, file = paste0(path_der,"metier_2013_15.rds"))

# connection and personal parameters to PG-database:
source("db.R")

# ... time stamp to latest Logbook DB source: YYYY-MM-DD
kakeTimeStamp <- "2025-04-10"

# Write combined metier datafiles 2013-2015 output data to Luke LOGBOOK database
dcprodschema <- paste0(kakeTimeStamp, "-dcprod")
invisible(write.dbTable(dcprodschema, "metier_sas_files_2013_15", metier_2013_15, overwrite = TRUE))


# check column names
# compare with base tables from table_a_g_h_i_j_2013-2024.R

#First read in aktiviteetti and akt1 if not loaded:


lookup1 <- data.frame(COL_NO = seq(1:length(names(aktiviteetti))),
                     COLUMN_NAME = names(aktiviteetti),
                     DESCRIPTON = NA)

lookup2 <- data.frame(COL_NO = seq(1:length(names(akt1))),
                      COLUMN_NAME = names(akt1),
                      DESCRIPTON = NA)

lookup3 <- data.frame(COL_NO= seq(1:length(names(metier_2013_15))),
                      COLUMN_NAME = names(metier_2013_15),
                      DESCRIPTON = NA)

# save to Excel
wb <- createWorkbook()
addWorksheet(wb, sheetName = "aktiviteetti")
addWorksheet(wb, sheetName = "akt1")
addWorksheet(wb, sheetName = "SAS_metier")
writeDataTable(wb, sheet = 1, lookup1)
writeDataTable(wb, sheet = 2, lookup2)
writeDataTable(wb, sheet = 3, lookup3)
saveWorkbook(wb, "orig/lookup.xlsx")
