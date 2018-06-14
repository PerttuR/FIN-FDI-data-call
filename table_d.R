<<<<<<< HEAD
#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE C
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
#
# Date: JUN-2018
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for preparing FDI data tables from Table A fron stat DEP (Pirkko)
#-------------------------------------------------------------------------------

#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)

# set working directory to read files from
setwd("C:/2018/FDI/work/data/orig/")

# import table A
table_A <- read.csv2("FIN_TABLE_A_CATCH.csv", sep = "," )


# unique keys
domain_discards_key = table_A %>% distinct(domain_discards)
domain_landings_key = table_A %>% distinct(domain_landings)



# import data from samples (Suomu), length classes
biological <- read.csv2("pituusluokkatiedot.csv", sep = ";")


# make a key variable to match table A key (domain_discards or domain_landings)
country_code <- "FIN"
quarter <- biological$q
subregion <- paste("27.3.D.", biological$ices_osa_alue, sep = "")
gear_type <- biological$metiers_fk

# codes for vessel length from appendix 2:
biological$vessel_length_code[biological$laivan_pituus_cm < 1000] <- "VL0010"
biological$vessel_length_code[biological$laivan_pituus_cm >= 1000 & biological$laivan_pituus_cm < 1200] <- "VL1012"
biological$vessel_length_code[biological$laivan_pituus_cm >= 1200 & biological$laivan_pituus_cm < 1800] <- "VL1218"
biological$vessel_length_code[biological$laivan_pituus_cm >= 1800 & biological$laivan_pituus_cm < 2400] <- "VL1824"
biological$vessel_length_code[biological$laivan_pituus_cm >= 2400 & biological$laivan_pituus_cm < 4000] <- "VL2440"
biological$vessel_length_code[biological$laivan_pituus_cm >= 4000] <- "VL40XX"

biological$vessel_length_code[is.na(biological$laivan_pituus_cm)] <- "NONE"
vessel_length <- biological$vessel_length_code
  
species <- biological$fao
commercial_cat <- "NA"

biological$domain_discards <- paste(country_code, quarter, subregion, gear_type, vessel_length, species, commercial_cat, sep = "_")



=======
#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE C
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
#
# Date: JUN-2018
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for preparing FDI data tables from Table A fron stat DEP (Pirkko)
#-------------------------------------------------------------------------------

#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)

# set working directory to read files from
setwd("C:/2018/FDI/work/data/orig/")

# import table A
table_A <- read.csv2("FIN_TABLE_A_CATCH.csv", sep = "," )


# unique keys
domain_discards_key = table_A %>% distinct(domain_discards)
domain_landings_key = table_A %>% distinct(domain_landings)



# import data from samples (Suomu), length classes
biological <- read.csv2("pituusluokkatiedot.csv", sep = ";")


# make a key variable to match table A key (domain_discards or domain_landings)
country_code <- "FIN"
quarter <- biological$q
subregion <- paste("27.3.D.", biological$ices_osa_alue, sep = "")
gear_type <- biological$metiers_fk

# codes for vessel length from appendix 2:
biological$vessel_length_code[biological$laivan_pituus_cm < 1000] <- "VL0010"
biological$vessel_length_code[biological$laivan_pituus_cm >= 1000 & biological$laivan_pituus_cm < 1200] <- "VL1012"
biological$vessel_length_code[biological$laivan_pituus_cm >= 1200 & biological$laivan_pituus_cm < 1800] <- "VL1218"
biological$vessel_length_code[biological$laivan_pituus_cm >= 1800 & biological$laivan_pituus_cm < 2400] <- "VL1824"
biological$vessel_length_code[biological$laivan_pituus_cm >= 2400 & biological$laivan_pituus_cm < 4000] <- "VL2440"
biological$vessel_length_code[biological$laivan_pituus_cm >= 4000] <- "VL40XX"

biological$vessel_length_code[is.na(biological$laivan_pituus_cm)] <- "NONE"
vessel_length <- biological$vessel_length_code
  
species <- biological$fao
commercial_cat <- "NA"

biological$domain_discards <- paste(country_code, quarter, subregion, gear_type, vessel_length, species, commercial_cat, sep = "_")



>>>>>>> 44b552f6219d2eadf161f9a320e95b4d4074fa47
