# import SAS metier tables for 2013 to 2015
# Authors: J. Demmler
# Date: 10/06/2025
# Last revision: 10/06/2025

library(tidyverse)
library(haven)
library(openxls)

# get data from G drive ####
          
for (i in 2013:2015){
  
  tmp <- read_sas(paste0("G:/Luke2/Stat_kala_tiedonkeruu/FDI/Data/pvkarvo",i,"_metier.sas7bdat")) |>
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

# remove
rm(metier_2013, metier_2014, metier_2015)
invisible(gc())

# check column names
# compare with base tables from table_a_g_h_i_j_2013-2024.R
lookup <- data.frame(COL_NO = seq(1:length(names(aktiviteetti))),
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
writeDataTable(wb, sheet = 1, lookup)
writeDataTable(wb, sheet = 2, lookup2)
writeDataTable(wb, sheet = 3, lookup3)
saveWorkbook(wb, "orig/lookup.xlsx")
