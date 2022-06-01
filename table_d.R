#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE D
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa, Anna-Kaisa Ylitalo
#
# Date: JUN-2018
# Updated: JUN-2022 by Team
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for preparing FDI data tables from Table A from stat DEP (Pirkko)
#-------------------------------------------------------------------------------


# install.packages("RPostgreSQL")
# install.packages("dplyr")


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
table_A <- read.csv2(paste0(path_tablea,.Platform$file.sep,"A_table_2013_2021.csv"), sep = "," , na.strings = "")
#select order of columns
table_A <- table_A %>% select(COUNTRY,	YEAR, QUARTER, VESSEL_LENGTH,	FISHING_TECH,	GEAR_TYPE,	TARGET_ASSEMBLAGE,	MESH_SIZE_RANGE,	METIER,	DOMAIN_DISCARDS,	DOMAIN_LANDINGS,	SUPRA_REGION,	SUB_REGION,	EEZ_INDICATOR,	GEO_INDICATOR,	NEP_SUB_REGION,	SPECON_TECH,	DEEP,	SPECIES,	TOTWGHTLANDG,	TOTVALLANDG,	DISCARDS,	CONFIDENTIAL)

table_A <- table_A %>% rename_all(tolower)

#colnames(table_A)    <- c("country", "year", "quarter", "vessel_length", "fishing_tech", "gear_type", "target_assemblage", "mesh_size_range", "metier", "domain_discards", "domain_landings", "supra_region", "sub_region", "eez_indicator", "geo_indicator", "specon_tech", "deep", "species", "totwghtlandg", "totvallandg", "discards", "confidential")

#-------------------------------------------------------------------------------

# sum totwghtlandg and unwanted_catch BY year, domain_discards and species from TABLE A
table_A_sum <- table_A %>%
  group_by(country, year, domain_discards, species) %>% 
  summarise(totwghtlandg = sum(as.numeric(totwghtlandg)), discards = sum(as.numeric(discards)))

# rounding the number to three digits precision
table_A_sum$totwghtlandg <- round(table_A_sum$totwghtlandg, digits = 3)
table_A_sum$discards <- round(table_A_sum$discards, digits = 3)

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#                       2. aggregate SAMPLED DATA for merging                       
#-------------------------------------------------------------------------------



## CHANGES 2022: in tables C, D, E and F in 2022 the variable NO_SAMPLES was replaced with TOTAL_SAMPLED_TRIPS.
## CHANGES 2022: in 2022 for tables C and D additional columns were added; TOTAL_TRIPS, DISCARD_CV, DISCARD_CI_UPPER,
##                  DISCARD_CI_LOWER to add information on the coverage rate of discard estimates.

# import data from samples (Suomu), length classes

source("db.R")

lengthdata <- read.dbTable("suomu","report_lengthclassrecords")
#-------------------------------------------------------------------------------
# choose commercial DISCARD samples only, from years 2013-2021 

unwanted <- filter(lengthdata, saalisluokka == "DISCARD", projekti == "EU-tike(CS, kaupalliset näytteet)", vuosi == 2013 | vuosi == 2021)

#-------------------------------------------------------------------------------
# make a key variable to match table A key (domain_discards or domain_landings)

# first make individually all the parts that form the key
country_code <- "FIN"
quarter <- unwanted$q
subregion <- paste("27.3.D.", unwanted$ices_osa_alue, sep = "")
#Stat dep uses FPO instead of FPN so change
unwanted <- unwanted %>% mutate(metiers_fk = replace(metiers_fk,metiers_fk=="FPN_FWS_>0_0_0","FPO_FWS_>0_0_0"))
unwanted <- unwanted %>% mutate(metiers_fk = replace(metiers_fk,metiers_fk=="FPN_SPF_>0_0_0","FPO_SPF_>0_0_0"))
gear_type <- unwanted$metiers_fk
unique(gear_type)

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

# choose only the important variables
unwanted2 <- unwanted %>% select(vuosi, domain_discards, nayteno, pituusluokka, pituusluokan_kpl_maara, pituusluokan_kokpaino, saalislaji)

#-------------------------------------------------------------------------------
# aggregate data on different levels according to Annex D instructions from the Official Letter

# #number of samples and length measurements 
# d7_8 <- unwanted %>% group_by(vuosi, domain_discards, saalislaji) %>% summarise(no_samples = n_distinct(nayteno), no_length_measurements = sum(pituusluokan_kpl_maara)) 
# 
# # minimum and maximum lengths
# d10_11 <- unwanted %>% group_by(vuosi, domain_discards) %>% summarise(min_length = sum(min(pituusluokka)), max_length = sum(max(pituusluokka)))
# 
# d12_13 <- unwanted2 %>% group_by(vuosi, domain_discards, pituusluokka) %>% summarise(no_length = sum(pituusluokan_kpl_maara), mean_weight_at_length = mean(pituusluokan_kokpaino/pituusluokan_kpl_maara, na.rm = TRUE))
# d12_13$mean_weight_at_length <- round(d12_13$mean_weight_at_length, digits = 0)


# number of samples (2022: total_sampled_trips), no_length_measurements, min_lenght, max_length
d12_13_15_16 <- unwanted %>% 
  group_by(vuosi, domain_discards, saalislaji) %>% 
  summarise(total_sampled_trips = n_distinct(nayteno), no_length_measurements = sum(pituusluokan_kpl_maara), min_length = sum(min(pituusluokka)), max_length = sum(max(pituusluokka))) 

# no_length, mean_weight_at_length
d18_19 <- unwanted2 %>% 
  group_by(vuosi, domain_discards, pituusluokka) %>% 
  summarise(no_length = sum(pituusluokan_kpl_maara), mean_weight_at_length = mean(pituusluokan_kokpaino/pituusluokan_kpl_maara, na.rm = TRUE))

d18_19$mean_weight_at_length <- round(d18_19$mean_weight_at_length, digits = 0)


#-------------------------------------------------------------------------------
# merge the aggregated datas (above) to unwanted catch data 

# unwanted3 <- merge(d12_13, d10_11, by = c("vuosi", "domain_discards"))
# 
# unwanted4 <- merge(unwanted3, d7_8, by = c("vuosi", "domain_discards"))

unwanted3 <- merge(d18_19, d12_13_15_16, by = c("vuosi", "domain_discards"))


# add length_unit and country variables
unwanted3$length_unit <- "mm"
unwanted3$country = "FIN"

# add variables included 2022
unwanted3$discard_cv <-"NK"
unwanted3$discard_ci_upper <-"NK"
unwanted3$discard_ci_lower <-"NK"


# select only those variables important to merging with table A
unwanted4 <- unwanted3 %>% 
  select(country, vuosi, domain_discards,saalislaji, total_sampled_trips, no_length_measurements, min_length, max_length, length_unit, pituusluokka, no_length, mean_weight_at_length, discard_cv, discard_ci_upper, discard_ci_lower) %>% 
  rename(year = vuosi, length = pituusluokka)

#2020 changes definition of length measurements Count
unwanted4$no_length_measurements <- unwanted4$no_length
unwanted4$no_length <- "NK"



#-------------------------------------------------------------------------------
#                       3. Merge SAMPLED DATA with TABLE A                       
#-------------------------------------------------------------------------------

# merge unwanted catch data with TABLE A
table_d_pre <- merge(unwanted4, table_A_sum, by = c("country", "year", "domain_discards"), all.x = T)

# some keys might not match, check how many there might be
missing_domains <- table_d_pre[is.na(table_d_pre$totwghtlandg),]
missing_domains2 = missing_domains %>% distinct(domain_discards, .keep_all = T)

length(missing_domains2$domain_discards)


# delete the missmatch values
table_d_pre2 <- filter(table_d_pre, !is.na(totwghtlandg))
missing_Discard_kilos_table_d_pre2 <- filter(table_d_pre, is.na(totwghtlandg))

#add new variables:
table_d_pre2$nep_sub_region <-"NA"
#table_d_pre2$mean_weight_at_length <-"NK"
table_d_pre2$weight_unit <-"g"

#2022 TOTAL NUMBER OF TRIPS should come from logbook database KAKE. Now dummy NK value used:
table_d_pre2$total_trips <- "NK"


# arrange the variables in proper order and put them to upper case
table_D <- table_d_pre2 %>% select(country,	year,	domain_discards, nep_sub_region, species,	totwghtlandg,	discards,	discard_cv, discard_ci_upper, discard_ci_lower, total_trips, total_sampled_trips,	no_length_measurements,	length_unit,	min_length,	max_length,	length,	no_length, mean_weight_at_length, weight_unit) %>%
  rename_all(toupper)


# save table D and table of deleted observations
write.xlsx(table_D, paste0(path_out,.Platform$file.sep,"TABLE_D_NAO_OFR_DISCARDS_LENGTH.xlsx"), sheetName = "TABLE_D", col.names = TRUE, row.names = FALSE)
write.xlsx(missing_domains2, paste0(path_out,.Platform$file.sep,"DELETED_DOMAINS_TABLE_D.xlsx"), col.names = TRUE, row.names = FALSE)
write.xlsx(missing_Discard_kilos_table_d_pre2, paste0(path_out,.Platform$file.sep,"DELETED_TABLE_D_no_kilos.xlsx"), col.names = TRUE, row.names = FALSE)

















