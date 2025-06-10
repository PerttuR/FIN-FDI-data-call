# import SAS metier tables for 2013 to 2015
# Authors: J. Demmler
# Date: 10/06/2025
# Last revision: 10/06/2025

library(tidyverse)
library(haven)

# get data from G drive
metier_2013 <- read_sas("G:/Luke2/Stat_kala_tiedonkeruu/FDI/Data/pvkarvo2013_metier.sas7bdat")
                  filter(KALVYOH == "FI") |>
                  mutate(KALASTUSVUOSI = 2013)

metier_2014 <- read_sas("G:/Luke2/Stat_kala_tiedonkeruu/FDI/Data/pvkarvo2014_metier.sas7bdat") |> 
                  filter(KALVYOH == "FI") |>
                  mutate(KALASTUSVUOSI = 2014)

metier_2015 <- read_sas("G:/Luke2/Stat_kala_tiedonkeruu/FDI/Data/pvkarvo2015_metier.sas7bdat") |> 
                  filter(KALVYOH == "FI") |>
                  mutate(KALASTUSVUOSI = 2015)                        
          
              
# alterative path
# metier_2013 <- read_sas("orig/pvkarvo2013_metier.sas7bdat") |> 
#                   filter(KALVYOH == "FI") |>
#                   mutate(KALASTUSVUOSI = 2013)
# 
# metier_2014 <- read_sas("orig/pvkarvo2014_metier.sas7bdat") |> 
#                   filter(KALVYOH == "FI") |>
#                   mutate(KALASTUSVUOSI = 2014)
# 
# metier_2015 <- read_sas("orig/pvkarvo2015_metier.sas7bdat") |> 
#                   filter(KALVYOH == "FI") |>
#                 mutate(KALASTUSVUOSI = 2015)

#combine into 1

metier_2013_15 <- bind_rows(metier_2013, metier_2014, metier_2015)

# remove
rm(metier_2013, metier_2014, metier_2015)
invisible(gc())

# check column names
names(metier_2013_15)