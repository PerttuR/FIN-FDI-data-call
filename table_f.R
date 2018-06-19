
#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE F
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
#perttu´s mac wd:
# setwd("~/R/FIN-FDI-data-call")
#mira´s wd:
setwd("C:/2018/FDI/work/data/orig/")

#-------------------------------------------------------------------------------
#                       1. aggregate TABLE A for merging                       
#-------------------------------------------------------------------------------

# import table A
table_A <- read.csv2("FIN_TABLE_A_CATCH.csv", sep = "," )
#-------------------------------------------------------------------------------

# sum totwghtlandg BY year, domain_landings and species from TABLE A
table_A_sum <- table_A %>% group_by(country, year, domain_landings, species) %>% summarise(totwghtlandg = sum(as.numeric(as.character(totwghtlandg))))

# rounding the number to three digits precision
table_A_sum$totwghtlandg <- round(table_A_sum$totwghtlandg, digits = 3)

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#                       2. aggregate SAMPLED DATA for merging                       
#-------------------------------------------------------------------------------

# import data from samples (Suomu), length classes
setwd("C:/2018/FDI/work/prog/FIN-FDI-data-call/")

source("db.R")

spec <- read.dbTable("suomu","species")
lengthdata <- read.dbTable("suomu","report_lengthclassrecords")
#-------------------------------------------------------------------------------
# choose commercial LANDING samples only, from years 2015-2017

landing <- filter(lengthdata, saalisluokka == "LANDING", projekti == "EU-tike(CS, kaupalliset näytteet)", vuosi >= 2015 & vuosi <= 2017)

#-------------------------------------------------------------------------------
# make a key variable to match table A key (domain_discards or domain_landings)

# first make individually all the parts that form the key
country_code <- "FIN"
quarter <- landing$q
subregion <- paste("27.3.D.", landing$ices_osa_alue, sep = "")
gear_type <- landing$metiers_fk

# codes for vessel length from appendix 2:
landing$vessel_length_code[landing$laivan_pituus_cm < 1000] <- "VL0010"
landing$vessel_length_code[landing$laivan_pituus_cm >= 1000 & landing$laivan_pituus_cm < 1200] <- "VL1012"
landing$vessel_length_code[landing$laivan_pituus_cm >= 1200 & landing$laivan_pituus_cm < 1800] <- "VL1218"
landing$vessel_length_code[landing$laivan_pituus_cm >= 1800 & landing$laivan_pituus_cm < 2400] <- "VL1824"
landing$vessel_length_code[landing$laivan_pituus_cm >= 2400 & landing$laivan_pituus_cm < 4000] <- "VL2440"
landing$vessel_length_code[landing$laivan_pituus_cm >= 4000] <- "VL40XX"
landing$vessel_length_code[is.na(landing$laivan_pituus_cm)] <- "NK"

vessel_length <- landing$vessel_length_code

species <- landing$fao
commercial_cat <- "NA"

# then combine them as a single key, identical to that from table A
landing$domain_landings <- paste(country_code, quarter, subregion, gear_type, vessel_length, species, commercial_cat, sep = "_")
#-------------------------------------------------------------------------------
# aggregate data on different levels according to Annex D instructions from the Official Letter

#number of samples (number of TRIPS) 
d_6 <- landing %>% group_by(vuosi, domain_landings) %>% summarise(no_samples_landg = n_distinct(nayteno)) 

#number of length measurements 
d_7 <- landing %>% group_by(vuosi, domain_landings) %>% summarise(no_length_measurements_landg = sum(as.numeric(pituusluokan_kpl_maara)))

# minimum and maximum lengths (notice! this is done by trip as well)
d9_10 <- landing %>% group_by(vuosi, domain_landings, nayteno) %>% summarise(min_length = sum(min(pituusluokka)), max_length = sum(max(pituusluokka)))

#-------------------------------------------------------------------------------
# merge the aggregated datas (above) to landing catch data 

landing2 <- merge(landing, d_6, by = c("vuosi", "domain_landings"))

landing3 <- merge(landing2, d_7, by = c("vuosi", "domain_landings"))

landing4 <- merge(landing3, d9_10, by = c("vuosi", "domain_landings", "nayteno"))

# add length_unit and country variables
landing4$length_unit <- "mm"
landing4$country = "FIN"

# this is just crude renaming
landing4$length <- landing4$pituusluokka
landing4$no_length_landg <- landing4$pituusluokan_kpl_maara
landing4$year <- landing$vuosi

# select only those variables important to merging with table A
landing5 <- select(landing4, country, year, domain_landings, no_samples_landg, no_length_measurements_landg, min_length, max_length, length_unit, length, no_length_landg)


#-------------------------------------------------------------------------------
#                       3. Merge SAMPLED DATA with TABLE A                       
#-------------------------------------------------------------------------------

# merge landing catch data with TABLE A
table_f_pre <- merge(landing5, table_A_sum, by = c("country", "year", "domain_landings"), all.x = T)

# some keys might not match, check how many there might be
missing_domains <- table_f_pre[is.na(table_f_pre$totwghtlandg),]
missing_domains2 = missing_domains %>% distinct(domain_landings, .keep_all = T)

length(missing_domains2$domain_landings)


# delete the missmatch values
table_f_pre2 <- filter(table_f_pre, !is.na(totwghtlandg))

# arrange the variables in proper order and put them to upper case
table_F <- table_f_pre2 %>% select(country, year, domain_landings, species, totwghtlandg, no_samples_landg, no_length_measurements_landg, length_unit, min_length, max_length, length, no_length_landg) %>% rename_all(toupper)


# set working directory to save table D and table of deleted observations
setwd("C:/2018/FDI/work/data/der/")
write.csv(table_F, "FIN_TABLE_F_LANDINGS_AT_LENGTH.csv", row.names = F)
write.csv(missing_domains2, "DELETED_TABLE_F.csv", row.names = F)

