# import SAS metier tables for 2013 to 2015
# Authors: J. Demmler
# Date: 10/06/2025
# Last revision: 10/06/2025

library(tidyverse)
library(haven)

# get data from G drive ####
          
for (i in 2013:2015){
  
  tmp <- read_sas(paste0("G:/Luke2/Stat_kala_tiedonkeruu/FDI/Data/pvkarvo",i,"_metier.sas7bdat")) |>
                  filter(KALVYOH == "FI") |>
                  mutate(KALASTUSVUOSI = i)
  
  assign(paste0("metier_",i), tmp)
  
} 


# alternative path ####
# for (i in 2013:2015){
#   
#   tmp <- read_sas(paste0("orig/pvkarvo",i,"_metier.sas7bdat")) |>
#     filter(KALVYOH == "FI") |>
#     mutate(KALASTUSVUOSI = i)
#   
#   assign(paste0("metier_",i), tmp)
#   
# } 

# combine into 1 table

metier_2013_15 <- bind_rows(metier_2013, metier_2014, metier_2015)

# remove
rm(metier_2013, metier_2014, metier_2015)
invisible(gc())

# check column names
names(metier_2013_15)





