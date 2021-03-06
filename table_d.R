#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE D
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
#
# Date: JUN-2018
# Updated: JUL 2021 by Perttu
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
# Mira:
path_tablea <- "C:/2018/FDI/work/data/orig/" # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- "C:/2018/FDI/work/prog/FIN-FDI-data-call/" # folder where the r project is (and the source file db.R!)
path_out <- "C:/2018/FDI/work/data/der/" # folder where the output is saved

# Perttu:
path_tablea <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call/orig" # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call" # folder where the r project is (and the source file db.R!)
path_out <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call/results/2021" # folder where the output is saved

#-------------------------------------------------------------------------------
#                       1. aggregate TABLE A for merging                       
#-------------------------------------------------------------------------------
setwd(path_tablea)

# import table A
table_A <- read.csv2("A_table_2014_2020.csv", sep = "," )
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
#                       2. aggregate SAMPLED DATA for merging                       
#-------------------------------------------------------------------------------

# import data from samples (Suomu), length classes
setwd(path_rproject)

source("db.R")

lengthdata <- read.dbTable("suomu","report_lengthclassrecords")
#-------------------------------------------------------------------------------
# choose commercial DISCARD samples only, from years 2015-2018 

unwanted <- filter(lengthdata, saalisluokka == "DISCARD", projekti == "EU-tike(CS, kaupalliset näytteet)", vuosi == 2014 | vuosi == 2020)

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

#number of samples and length measurements 
d7_8 <- unwanted %>% group_by(vuosi, domain_discards, saalislaji) %>% summarise(no_samples = n_distinct(nayteno), no_length_measurements = sum(pituusluokan_kpl_maara)) 

# minimum and maximum lengths
d10_11 <- unwanted %>% group_by(vuosi, domain_discards) %>% summarise(min_length = sum(min(pituusluokka)), max_length = sum(max(pituusluokka)))

d12_13 <- unwanted2 %>% group_by(vuosi, domain_discards, pituusluokka) %>% summarise(no_length = sum(pituusluokan_kpl_maara), mean_weight_at_length = mean(pituusluokan_kokpaino/pituusluokan_kpl_maara, na.rm = TRUE))
d12_13$mean_weight_at_length <- round(d12_13$mean_weight_at_length, digits = 0)

#-------------------------------------------------------------------------------
# merge the aggregated datas (above) to unwanted catch data 

unwanted3 <- merge(d12_13, d10_11, by = c("vuosi", "domain_discards"))

unwanted4 <- merge(unwanted3, d7_8, by = c("vuosi", "domain_discards"))

# add length_unit and country variables
unwanted4$length_unit <- "mm"
unwanted4$country = "FIN"

# select only those variables important to merging with table A
unwanted5 <- unwanted4 %>% select(country, vuosi, domain_discards,saalislaji, no_samples, no_length_measurements, min_length, max_length, length_unit, pituusluokka, no_length, mean_weight_at_length) %>% rename(year = vuosi, length = pituusluokka)

#2020 changes definition of length measurements Count
unwanted5$no_length_measurements <- unwanted5$no_length
unwanted5$no_length <- "NK"



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
missing_Discard_kilos_table_d_pre2 <- filter(table_d_pre, is.na(totwghtlandg))

#add new variables:
table_d_pre2$nep_sub_region <-"NA"
#table_d_pre2$mean_weight_at_length <-"NK"
table_d_pre2$weight_unit <-"g"


# arrange the variables in proper order and put them to upper case
table_D <- table_d_pre2 %>% select(country,	year,	domain_discards, nep_sub_region,	species,	totwghtlandg,	discards,	no_samples,	no_length_measurements,	length_unit,	min_length,	max_length,	length,	no_length, mean_weight_at_length, weight_unit) %>% rename_all(toupper)


# set working directory to save table D and table of deleted observations
setwd(path_out)
write.xlsx(table_D, "TABLE_D_NAO_OFR_DISCARDS_LENGTH.xlsx", sheetName = "TABLE_D", col.names = TRUE, row.names = FALSE)
write.csv(missing_domains2, "DELETED_TABLE_D_no_DOMAIN.csv", row.names = F)
write.csv(missing_Discard_kilos_table_d_pre2, "DELETED_TABLE_D_no_kilos.csv", row.names = F)

















