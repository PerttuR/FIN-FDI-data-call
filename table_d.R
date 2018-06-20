
#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE D
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


# install.packages("RPostgreSQL")
# install.packages("dplyr")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)

# set working directory to read files from
#perttu´s adderesses:
# setwd("~/R/FIN-FDI-data-call")
#setwd("C:/perttu/eu-tike/STECF/FIN-FDI-data-call")
#mira´s wd:
setwd("C:/2018/FDI/work/data/orig/")

#-------------------------------------------------------------------------------
#                       1. aggregate TABLE A for merging                       
#-------------------------------------------------------------------------------

# import table A
table_A <- read.csv2("FIN_TABLE_A_CATCH.csv", sep = "," )
#-------------------------------------------------------------------------------

# sum totwghtlandg and unwanted_catch BY year, domain_discards and species from TABLE A
table_A_sum <- table_A %>% group_by(country, year, domain_discards, species) %>% summarise(totwghtlandg = sum(as.numeric(as.character(totwghtlandg))), unwanted_catch = sum(as.numeric(as.character(unwanted_catch))))

# rounding the number to three digits precision
table_A_sum$totwghtlandg <- round(table_A_sum$totwghtlandg, digits = 3)
table_A_sum$unwanted_catch <- round(table_A_sum$unwanted_catch, digits = 3)

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#                       2. aggregate SAMPLED DATA for merging                       
#-------------------------------------------------------------------------------

# import data from samples (Suomu), length classes
#setwd("C:/2018/FDI/work/prog/FIN-FDI-data-call/")

source("db.R")

lengthdata <- read.dbTable("suomu","report_lengthclassrecords")
#-------------------------------------------------------------------------------
# choose commercial DISCARD samples only, from years 2015-2017

unwanted <- filter(lengthdata, saalisluokka == "DISCARD", projekti == "EU-tike(CS, kaupalliset näytteet)", vuosi >= 2015 & vuosi <= 2017)

#-------------------------------------------------------------------------------
# make a key variable to match table A key (domain_discards or domain_landings)

# first make individually all the parts that form the key
country_code <- "FIN"
quarter <- unwanted$q
subregion <- paste("27.3.D.", unwanted$ices_osa_alue, sep = "")
gear_type <- unwanted$metiers_fk

# codes for vessel length from appendix 2:
unwanted$vessel_length_code[unwanted$laivan_pituus_cm < 1000] <- "VL0010"
unwanted$vessel_length_code[unwanted$laivan_pituus_cm >= 1000 & unwanted$laivan_pituus_cm < 1200] <- "VL1012"
unwanted$vessel_length_code[unwanted$laivan_pituus_cm >= 1200 & unwanted$laivan_pituus_cm < 1800] <- "VL1218"
unwanted$vessel_length_code[unwanted$laivan_pituus_cm >= 1800 & unwanted$laivan_pituus_cm < 2400] <- "VL1824"
unwanted$vessel_length_code[unwanted$laivan_pituus_cm >= 2400 & unwanted$laivan_pituus_cm < 4000] <- "VL2440"
unwanted$vessel_length_code[unwanted$laivan_pituus_cm >= 4000] <- "VL40XX"
unwanted$vessel_length_code[is.na(unwanted$laivan_pituus_cm)] <- "NK"

vessel_length <- unwanted$vessel_length_code
  
species <- unwanted$fao
commercial_cat <- "NA"

# then combine them as a single key, identical to that from table A
unwanted$domain_discards <- paste(country_code, quarter, subregion, gear_type, vessel_length, species, commercial_cat, sep = "_")
#-------------------------------------------------------------------------------
# aggregate data on different levels according to Annex D instructions from the Official Letter

#number of samples (number of TRIPS) 
d_7 <- unwanted %>% group_by(vuosi, domain_discards) %>% summarise(no_samples_uc = n_distinct(nayteno)) 

#number of length measurements 
d_8 <- unwanted %>% group_by(vuosi, domain_discards) %>% summarise(no_length_measurements_uc = sum(as.numeric(pituusluokan_kpl_maara)))

# minimum and maximum lengths (notice! this is done by trip as well)
d10_11 <- unwanted %>% group_by(vuosi, domain_discards, nayteno) %>% summarise(min_length = sum(min(pituusluokka)), max_length = sum(max(pituusluokka)))

#-------------------------------------------------------------------------------
# merge the aggregated datas (above) to unwanted catch data 

unwanted2 <- merge(unwanted, d_7, by = c("vuosi", "domain_discards"))

unwanted3 <- merge(unwanted2, d_8, by = c("vuosi", "domain_discards"))

unwanted4 <- merge(unwanted3, d10_11, by = c("vuosi", "domain_discards", "nayteno"))



# add length_unit and country variables
unwanted4$length_unit <- "mm"
unwanted4$country = "FIN"

# this is just crude renaming
unwanted4$length <- unwanted4$pituusluokka
unwanted4$no_length_uc <- unwanted4$pituusluokan_kpl_maara
unwanted4$year <- unwanted$vuosi

# select only those variables important to merging with table A
unwanted5 <- select(unwanted4, country, year, domain_discards, no_samples_uc, no_length_measurements_uc, min_length, max_length, length_unit, length, no_length_uc)


#-------------------------------------------------------------------------------
#                       3. Merge SAMPLED DATA with TABLE A                       
#-------------------------------------------------------------------------------

# merge unwanted catch data with TABLE A
table_d_pre <- merge(unwanted5, table_A_sum, by = c("country", "year", "domain_discards"), all.x = T)


# some keys might not match, check how many there might be
missing_domains <- table_d_pre[is.na(table_d_pre$totwghtlandg),]
missing_domains2 = missing_domains %>% distinct(domain_discards, .keep_all = T)

length(missing_domains2$domain_discards)


# delete the missmatch values
table_d_pre2 <- filter(table_d_pre, !is.na(totwghtlandg))



# aggregate data on a level of YEAR, DOMAIN and LENGTH!

#table_d_pre3 <- table_d_pre2 %>% group_by(year, domain_discards, length) %>% summarise(no_length_uc2 = sum(no_length_uc))





# arrange the variables in proper order and put them to upper case
table_D <- table_d_pre2 %>% select(country, year, domain_discards, species, totwghtlandg, unwanted_catch, no_samples_uc, no_length_measurements_uc, length_unit, min_length, max_length, length, no_length_uc) %>% rename_all(toupper)


# set working directory to save table D and table of deleted observations
#setwd("C:/2018/FDI/work/data/der/")
write.csv(table_D, "FIN_TABLE_D_UNWANTED_CATCH_AT_LENGTH.csv", row.names = F)
write.csv(missing_domains2, "DELETED_TABLE_D.csv", row.names = F)

