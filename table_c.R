#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE C
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa, Anna-Kaisa Ylitalo
#
# Create Date: JUN-2018
# Updated: APR 2022 by Team
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for preparing FDI data tables from Table A from statistical DEP (Pirkko)
# Table C:includes EU-DCF samples from Suomu DB. No samples from LohiDB, since discards do not exist there
#-------------------------------------------------------------------------------


# install.packages("RPostgreSQL")
# install.packages("dplyr")
#install.packages("xlsx")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)
library(xlsx)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------

# Common paths & 2022 folder:
path_tablea <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- getwd() # folder where the r project is (and the source file db.R!)
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2022")

#-------------------------------------------------------------------------------
#                       1. aggregate TABLE A for merging                       
#-------------------------------------------------------------------------------

# import table A
table_A_2014_2021 <- read.csv2(paste0(path_tablea,.Platform$file.sep,"A_table_2014_2020.csv"), sep = "," , na.strings = "")
table_A_2013 <- read.csv2(paste0(path_tablea,.Platform$file.sep,"A_table_2013.csv"), sep = "," , na.strings = "")
table_A_2015_2020 <- read.csv2(paste0(path_tablea,.Platform$file.sep,"A_table_2015_2019.csv"), sep = "," , na.strings = "")

new_matrix <- rbind(table_A_2014_2021, table_A_2013)
table_A <- rbind(new_matrix, table_A_2015_2020)

#select order of columns
table_A <- table_A %>% select(COUNTRY,	YEAR, QUARTER, VESSEL_LENGTH,	FISHING_TECH,	GEAR_TYPE,	TARGET_ASSEMBLAGE,	MESH_SIZE_RANGE,	METIER,	DOMAIN_DISCARDS,	DOMAIN_LANDINGS,	SUPRA_REGION,	SUB_REGION,	EEZ_INDICATOR,	GEO_INDICATOR,	NEP_SUB_REGION,	SPECON_TECH,	DEEP,	SPECIES,	TOTWGHTLANDG,	TOTVALLANDG,	DISCARDS,	CONFIDENTIAL)

table_A <- table_A %>% rename_all(tolower)

#colnames(table_A)    <- c("country", "year", "quarter", "vessel_length", "fishing_tech", "gear_type", "target_assemblage", "mesh_size_range", "metier", "domain_discards", "domain_landings", "supra_region", "sub_region", "eez_indicator", "geo_indicator", "specon_tech", "deep", "species", "totwghtlandg", "totvallandg", "discards", "confidential")

#-------------------------------------------------------------------------------

# sum totwghtlandg and unwanted_catch BY year, domain_discards and species from TABLE A
table_A_sum <- table_A %>% group_by(country, year, domain_discards, species) %>% summarise(totwghtlandg = sum(as.numeric(as.character(totwghtlandg))), discards = sum(as.numeric(as.character(discards))))

# rounding the number to three digits precision
table_A_sum$totwghtlandg <- round(table_A_sum$totwghtlandg, digits = 3)
table_A_sum$discards <- round(table_A_sum$discards, digits = 3)

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#                       2. aggregate AGE DATA for merging                       
#-------------------------------------------------------------------------------


## CHANGES 2022: in tables C, D, E and F in 2022 the variable NO_SAMPLES was replaced with TOTAL_SAMPLED_TRIPS.
## CHANGES 2022: in 2022 for tables C and D additional columns were added; TOTAL_TRIPS, DISCARD_CV, DISCARD_CI_UPPER,
##                  DISCARD_CI_LOWER to add information on the coverage rate of discard estimates.


source("db.R")

agedata <- read.dbTable("suomu","report_individual")



#-------------------------------------------------------------------------------
# choose commercial DISCARD INDIV samples only, from years 2013 to 2021

unwanted <- filter(agedata, saalisluokka == "DISCARD", name == "EU-tike(CS, kaupalliset näytteet)", vuosi >= 2013 & vuosi <= 2021, !is.na(ika))

# CHECK missing age data numbers: age data covers only part of the individual data (individual data is collected for the use of other biological parametres as well)
unwanted_without_age <- filter(agedata, saalisluokka == "DISCARD", name == "EU-tike(CS, kaupalliset näytteet)", vuosi >= 2013 & vuosi <= 2021, is.na(ika))

#-------------------------------------------------------------------------------
# make a key variable to match table A key (domain_discards or domain_landings)

# first make individually all the parts that form the "DOMAIN_DISCARDS" key 
country_code <- "FIN"
quarter <- unwanted$q
subregion <- paste("27.3.D.", unwanted$ices_osa_alue, sep = "")
#Stat dep in A TAble uses different gear code -> FPO instead of FPN so change the metier accordingly
unwanted <- unwanted %>% mutate(metier = replace(metier,metier=="FPN_FWS_>0_0_0","FPO_FWS_>0_0_0"))
gear_type <- unwanted$metier


# codes for vessel length from appendix 2 as defined in 2022 datacall annex:
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

#unwanted weight from g -> to kg
unwanted$paino <- unwanted$paino/1000

#unwanted length from mm -> to cm
unwanted$pituus <- unwanted$pituus/10

#-------------------------------------------------------------------------------

# select only important variables
unwanted2 <- select(unwanted, vuosi, nayteno, paino, pituus, ika, domain_discards)

# AKY: aggregate data
d7_8 <- unwanted2 %>% group_by(vuosi, domain_discards) %>% summarise(total_sampled_trips = n_distinct(nayteno), no_age_measurements = n())

d10_11 <- unwanted2 %>% group_by(vuosi, domain_discards) %>% summarise(min_age = min(ika), max_age = max(ika)) 

d12_13_14_15 <- unwanted2 %>% group_by(vuosi, domain_discards, ika) %>% summarise(no_age = n(), mean_weight = round(mean(paino), digits = 3), mean_length = round(mean(pituus), digits = 1))

#-------------------------------------------------------------------------------
# merge the aggregated datas (above) to unwanted catch data 

unwanted3 <- merge(d12_13_14_15, d10_11, by = c("vuosi", "domain_discards"))

unwanted4 <- merge(unwanted3, d7_8, by = c("vuosi", "domain_discards"))

# add variables
unwanted4$country <- "FIN"
unwanted4$age_measurements_prop <- "NA"
unwanted4$nep_sub_region <-"NA"

# add variables included 2022
unwanted4$discard_cv <-"NK"
unwanted4$discard_ci_upper <-"NK"
unwanted4$discard_ci_lower <-"NK"

# select only those variables important to merging with table A
unwanted5 <- unwanted4 %>% select(country, vuosi, domain_discards, nep_sub_region, total_sampled_trips, no_age_measurements, age_measurements_prop, min_age, max_age, ika, no_age, mean_weight, mean_length, discard_cv, discard_ci_upper, discard_ci_lower) %>% rename(year = vuosi, age = ika)


#-------------------------------------------------------------------------------
#                       3. Merge SAMPLE DATA with TABLE A                       
#-------------------------------------------------------------------------------


# merge unwanted catch data with TABLE A. All biological data is included (all.x = T)
table_c_pre <- merge(unwanted5, table_A_sum, by = c("country", "year", "domain_discards"), all.x = T)

# some keys might not match, check how many there might be
missing_domains <- table_c_pre[is.na(table_c_pre$totwghtlandg),]
missing_domains2 <- missing_domains %>% distinct(domain_discards, .keep_all = T)

length(missing_domains2$domain_discards)

# delete the missmatch values
table_c_pre2 <- filter(table_c_pre, !is.na(totwghtlandg))


#units
table_c_pre2$weight_unit <- "kg"
table_c_pre2$length_unit <- "cm"

#2020 DATACALL MUUTOS: ikäkohtaisten mittauslukumäärä domainkohtaisen lukumäärä tilalle..
table_c_pre2$no_age_measurements <- table_c_pre2$no_age
#2020 DATACALL MUUTOS: ja samalla oletettu laajennettu ikämäärä (estimaatti) NK:ksi..
table_c_pre2$no_age <- "NK"

#2022 TOTAL NUMBER OF TRIPS should come from logbook database KAKE. Now dummy NK value used:
table_c_pre2$total_trips <- "NK"

#test, delete when fdi db ready:
table_c_pre2$no_samples <- table_c_pre2$total_trips

# arrange the variables in proper order and put them to upper case 2021 version:
table_c <- table_c_pre2  %>% select(country,	year,	domain_discards, nep_sub_region,	species,	totwghtlandg,	discards, no_samples,	no_age_measurements,	age_measurements_prop,	min_age,	max_age,	age,	no_age,	mean_weight,	weight_unit, mean_length, length_unit) %>% rename_all(toupper)

#2022 select:
#table_c <- table_c_pre2  %>% select(country,	year,	domain_discards, nep_sub_region,	species,	totwghtlandg,	discards, discard_cv, discard_ci_upper, discard_ci_lower,	total_trips, total_sampled_trips,	no_age_measurements,	age_measurements_prop,	min_age,	max_age,	age,	no_age,	mean_weight,	weight_unit, mean_length, length_unit) %>% rename_all(toupper)
  
#  country, year, domain_discards, species, totwghtlandg, unwanted_catch, no_samples_uc, no_age_measurements_uc, age_measurements_prop, min_age, max_age, age, no_age_uc, mean_weight_uc, mean_length_uc) %>% rename_all(toupper)


# save table C and table of deleted observations

write.xlsx(table_c,paste0(path_out,.Platform$file.sep,"TABLE_C_NAO_OFR_DISCARDS_AGE.xlsx"), sheetName = "TABLE_C", col.names = TRUE, row.names = FALSE)
write.xlsx(table_c_pre,paste0(path_out,.Platform$file.sep,"TABLE_C_NAO_OFR_DISCARDS_AGE_RAW_MERGED.xlsx"), sheetName = "TABLE_C_all_merged", col.names = TRUE, row.names = FALSE)
write.xlsx(missing_domains2,paste0(path_out,.Platform$file.sep,"DELETED_DOMAINS_TABLE_C.xlsx"), sheetName = "TABLE_A_puuttuvat_poisheitto_domainit", col.names = TRUE, row.names = FALSE)



