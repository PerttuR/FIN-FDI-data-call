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
# The following script is for preparing FDI data tables from Table A from statistical DEP (Pirkko)
#-------------------------------------------------------------------------------


# install.packages("RPostgreSQL")
# install.packages("dplyr")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------
# Mira:
path_tablea <- "C:/2018/FDI/work/data/orig/" # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- "C:/2018/FDI/work/prog/FIN-FDI-data-call/" # folder where the r project is (and the source file db.R!)
path_out <- "C:/2018/FDI/work/data/der/" # folder where the output is saved

# Perttu:
path_tablea <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call/orig" # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call" # folder where the r project is (and the source file db.R!)
path_out <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call/results" # folder where the output is saved

#-------------------------------------------------------------------------------
#                       1. aggregate TABLE A for merging                       
#-------------------------------------------------------------------------------
setwd(path_tablea)

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
#                       2. aggregate AGE DATA for merging                       
#-------------------------------------------------------------------------------

setwd(path_rproject)

source("db.R")

agedata <- read.dbTable("suomu","report_individual")



#-------------------------------------------------------------------------------
# choose commercial DISCARD samples only, from years 2015-2017

unwanted <- filter(agedata, saalisluokka == "DISCARD", name == "EU-tike(CS, kaupalliset näytteet)", vuosi >= 2015 & vuosi <= 2017, !is.na(ika))

# age data covers only part of the individual data (individual data is collected for the use of other biological parametres as well)
unwanted_missing_age <- filter(agedata, saalisluokka == "DISCARD", name == "EU-tike(CS, kaupalliset näytteet)", vuosi >= 2015 & vuosi <= 2017, is.na(ika))

#-------------------------------------------------------------------------------
# make a key variable to match table A key (domain_discards or domain_landings)

# first make individually all the parts that form the key
country_code <- "FIN"
quarter <- unwanted$q
subregion <- paste("27.3.D.", unwanted$ices_osa_alue, sep = "")
gear_type <- unwanted$metier

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
#this variable (commercial_cat) should be in the sampling db.. check later!(should also match with statistical logbook data so perhaps NA only works??)
commercial_cat <- "NA"

# then combine them as a single key, identical to that from table A
unwanted$domain_discards <- paste(country_code, quarter, subregion, gear_type, vessel_length, species, commercial_cat, sep = "_")
#-------------------------------------------------------------------------------

# select only important variables
unwanted2 <- select(unwanted, vuosi, nayteno, paino, pituus, ika, domain_discards)
unwanted2$dummy <- 1 # help variable to count observations (probably a better way in dplyr but no time for that...)

# aggregate data
d7_8 <- unwanted2 %>% group_by(vuosi, domain_discards) %>% summarise(no_samples_uc = n_distinct(nayteno), no_age_measurements_uc = sum(dummy))

d10_11 <- unwanted2 %>% group_by(vuosi, domain_discards) %>% summarise(min_age = min(ika), max_age = max(ika)) 

d12_13_14_15 <- unwanted2 %>% group_by(vuosi, domain_discards, ika) %>% summarise(no_age_uc = sum(dummy), mean_weight_uc = round(mean(paino), digits = 3), mean_length_uc = round(mean(pituus), digits = 1))


#-------------------------------------------------------------------------------
# merge the aggregated datas (above) to unwanted catch data 

unwanted3 <- merge(d12_13_14_15, d10_11, by = c("vuosi", "domain_discards"))

unwanted4 <- merge(unwanted3, d7_8, by = c("vuosi", "domain_discards"))

# add variables
unwanted4$country <- "FIN"
unwanted4$age_measurements_prop <- "NK"

# select only those variables important to merging with table A
unwanted5 <- unwanted4 %>% select(country, vuosi, domain_discards, no_samples_uc, no_age_measurements_uc, age_measurements_prop, min_age, max_age, ika, no_age_uc, mean_weight_uc, mean_length_uc) %>% rename(year = vuosi, age = ika)


#-------------------------------------------------------------------------------
#                       3. Merge SAMPLED DATA with TABLE A                       
#-------------------------------------------------------------------------------

# merge unwanted catch data with TABLE A
table_c_pre <- merge(unwanted5, table_A_sum, by = c("country", "year", "domain_discards"), all.x = T)

# some keys might not match, check how many there might be
missing_domains <- table_c_pre[is.na(table_c_pre$totwghtlandg),]
missing_domains2 = missing_domains %>% distinct(domain_discards, .keep_all = T)

length(missing_domains2$domain_discards)


# delete the missmatch values
table_c_pre2 <- filter(table_c_pre, !is.na(totwghtlandg))

# arrange the variables in proper order and put them to upper case
table_C <- table_c_pre2  %>% select(country, year, domain_discards, species, totwghtlandg, unwanted_catch, no_samples_uc, no_age_measurements_uc, age_measurements_prop, min_age, max_age, age, no_age_uc, mean_weight_uc, mean_length_uc) %>% rename_all(toupper)


# set working directory to save table D and table of deleted observations
setwd(path_out)
write.csv(table_C, "FIN_TABLE_C_UNWANTED_CATCH_AT_AGE.csv", row.names = F)
write.csv(missing_domains2, "DELETED_TABLE_C.csv", row.names = F)



