
#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE E
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
#
# Date: JUN-2018
# Updated: JUN 2021 by Perttu
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for preparing FDI data tables from Table A from stat DEP (Pirkko)
#-------------------------------------------------------------------------------


#install.packages("RPostgreSQL")
# install.packages("dplyr")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)
library(RPostgreSQL)
library(xlsx)


#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------
# Mira:
path_tablea <- "C:/2018/FDI/work/data/orig/" # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- "C:/2018/FDI/work/prog/FIN-FDI-data-call/" # folder where the r project is (and the source file db.R!)
path_salmon <- "C:/2018/FDI/work/data/orig/" # folder where the salmon data is (stecf.csv)
path_out <- "C:/2018/FDI/work/data/der/" # folder where the output is saved

# Perttu:
path_tablea <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- getwd() # folder where the r project is (and the source file db.R!)
path_salmon <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where salmon data lies (salmon.csv)
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep, "results", .Platform$file.sep, "2021")

# create directories if missing, but ignore warnings in case they already exist
dir.create(path_tablea, showWarnings = FALSE)
dir.create(path_out, showWarnings = FALSE)

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

# sum totwghtlandg BY year, domain_landings and species from TABLE A
table_A_sum <- table_A %>% group_by(country, year, domain_landings, species) %>% summarise(totwghtlandg = sum(as.numeric(as.character(totwghtlandg))))

# rounding the number to three digits precision
table_A_sum$totwghtlandg <- round(table_A_sum$totwghtlandg, digits = 3)

table_A_sum_SAL <-  filter(table_A_sum, species=="SAL")

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#                       2. aggregate AGE DATA for merging                       
#-------------------------------------------------------------------------------

setwd(path_rproject)

source("db.R")

agedata <- read.dbTable(schema="suomu",table="report_individual", where=paste0("vuosi IN(2014, 2020)"))

#-------------------------------------------------------------------------------
# choose commercial DISCARD samples only, from years 2015-2017

landing <- filter(agedata, saalisluokka == "LANDING", name == "EU-tike(CS, kaupalliset näytteet)", vuosi == 2014 | vuosi == 2020, !is.na(ika))

# a lot of ages are missing
landing_missing_age <- filter(agedata, saalisluokka == "LANDING", name == "EU-tike(CS, kaupalliset näytteet)", vuosi == 2014 | vuosi == 2020, is.na(ika))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# make a key variable to match table A key (domain_discards or domain_landings)

# first make individually all the parts that form the key
country_code <- "FIN"
quarter <- landing$q
subregion <- paste("27.3.D.", landing$ices_osa_alue, sep = "")
#Stat dep uses FPO instead of FPN so change
landing <- landing %>% mutate(metier = replace(metier,metier=="FPN_FWS_>0_0_0","FPO_FWS_>0_0_0"))
landing <- landing %>% mutate(metier = replace(metier,metier=="FPN_SPF_>0_0_0","FPO_SPF_>0_0_0"))
gear_type <- landing$metier
unique(gear_type)

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

# select only important variables
landing2 <- landing %>% select(vuosi, nayteno, paino, pituus, ika, domain_landings)

#landing2 weight from g -> to kg
landing2$paino <- landing2$paino/1000

#landing2 length from mm -> to cm
landing2$pituus <- landing2$pituus/10

#--------------------------------------------------------------------------------------------
#       3. aggregate SALMON data to length classes and merge it with LANDING data                       
#--------------------------------------------------------------------------------------------

# download salmon data from : http://suomu.rktl.fi/lohi/Report/stecf?format=csv
# and save it to workflow orig folder
# import data from salmon samples
setwd(path_salmon)
salmon <- read.csv("salmon.csv", sep = ";", header = T, stringsAsFactors=FALSE)

#2021 data call filter 2014 and 2020
salmon <- salmon %>% filter( YEAR == 2014 | YEAR == 2020)

#rename metier to correct
salmon <- salmon %>% mutate(METIER=replace(METIER, METIER=="FYK_ANA_0_0_0", "FYK_ANA_>0_0_0")) %>% as.data.frame()


# make a key variable to match table A key (domain_discards or domain_landings)

# first make individually all the parts that form the key
country_code <- "FIN"
quarter <- salmon$QUARTER
subregion <- paste("27.3.D.", salmon$ICES_OA, sep = "")
gear_type <- salmon$METIER
vessel_length <- "VL0010"
species <- salmon$FAO
commercial_cat <- "NA"



# then combine them as a single key, identical to that from table A
salmon$domain_landings <- paste(country_code, quarter, subregion, gear_type, vessel_length, species, commercial_cat, sep = "_")


# aggregate data into length classes 
salmon$pituusluokka[salmon$PITUUS >= 300 & salmon$PITUUS < 350] <- 300
salmon$pituusluokka[salmon$PITUUS >= 350 & salmon$PITUUS < 400] <- 350
salmon$pituusluokka[salmon$PITUUS >= 400 & salmon$PITUUS < 450] <- 400
salmon$pituusluokka[salmon$PITUUS >= 450 & salmon$PITUUS < 500] <- 450
salmon$pituusluokka[salmon$PITUUS >= 500 & salmon$PITUUS < 550] <- 500
salmon$pituusluokka[salmon$PITUUS >= 550 & salmon$PITUUS < 600] <- 550
salmon$pituusluokka[salmon$PITUUS >= 600 & salmon$PITUUS < 650] <- 600
salmon$pituusluokka[salmon$PITUUS >= 650 & salmon$PITUUS < 700] <- 650
salmon$pituusluokka[salmon$PITUUS >= 700 & salmon$PITUUS < 750] <- 700
salmon$pituusluokka[salmon$PITUUS >= 750 & salmon$PITUUS < 800] <- 750
salmon$pituusluokka[salmon$PITUUS >= 800 & salmon$PITUUS < 850] <- 800
salmon$pituusluokka[salmon$PITUUS >= 850 & salmon$PITUUS < 900] <- 850
salmon$pituusluokka[salmon$PITUUS >= 900 & salmon$PITUUS < 950] <- 900
salmon$pituusluokka[salmon$PITUUS >= 950 & salmon$PITUUS < 1000] <- 950
salmon$pituusluokka[salmon$PITUUS >= 1000 & salmon$PITUUS < 1050] <- 1000
salmon$pituusluokka[salmon$PITUUS >= 1050 & salmon$PITUUS < 1100] <- 1050
salmon$pituusluokka[salmon$PITUUS >= 1100 & salmon$PITUUS < 1150] <- 1100
salmon$pituusluokka[salmon$PITUUS >= 1150 & salmon$PITUUS < 1200] <- 1150
salmon$pituusluokka[salmon$PITUUS >= 1200 & salmon$PITUUS < 1250] <- 1200



# select important variables and rename them to match landing2 data
salmon2 <- salmon %>% select(YEAR, DB_TRIP_ID, PITUUS, PAINO_GRAMMOINA, IKA, domain_landings) %>% rename(vuosi = YEAR, nayteno = DB_TRIP_ID, pituus = PITUUS, paino = PAINO_GRAMMOINA, ika = IKA)

#salmon2 weight from g -> to kg
salmon2$paino <- salmon2$paino/1000

#salmon2 length from mm -> to cm
salmon2$pituus <- salmon2$pituus/10


# remove missing age values
salmon3 <- filter(salmon2, !is.na(ika))
salmon3 <- filter(salmon3, !is.na(paino))
salmon3 <- filter(salmon3, !is.na(pituus))
# merge landing and salmon data

landing3 <- merge(landing2, salmon3, all = T)
landing3$dummy <- 1 # help variable to count observations (probably a better way in dplyr but no time for that...)


#-------------------------------------------------------------------------------
#                   4. aggregate AGE DATA for merging with TABLE A                     
#-------------------------------------------------------------------------------

# aggregate data
d6_7 <- landing3 %>% group_by(vuosi, domain_landings) %>% summarise(no_samples = n_distinct(nayteno), no_age_measurements = sum(dummy))

d9_10 <- landing3 %>% group_by(vuosi, domain_landings) %>% summarise(min_age = min(ika), max_age = max(ika)) 

d11_12_13_14 <- landing3 %>% group_by(vuosi, domain_landings, ika) %>% summarise(no_age = sum(dummy), mean_weight = round(mean(paino), digits = 3), mean_length = round(mean(pituus), digits = 1))


#-------------------------------------------------------------------------------
# merge the aggregated datas (above) to landing catch data 

landing3 <- merge(d11_12_13_14, d9_10, by = c("vuosi", "domain_landings"))

landing4 <- merge(landing3, d6_7, by = c("vuosi", "domain_landings"))

# add variables
landing4$country <- "FIN"
landing4$age_measurements_prop <- "NA"

# select only those variables important to merging with table A
landing5 <- landing4 %>% select(country, vuosi, domain_landings, no_samples, no_age_measurements, age_measurements_prop, min_age, max_age, ika, no_age, mean_weight, mean_length) %>% rename(year = vuosi, age = ika)


#-------------------------------------------------------------------------------
#                       3. Merge SAMPLED DATA with TABLE A                       
#-------------------------------------------------------------------------------

# merge landing catch data with TABLE A
table_e_pre <- merge(landing5, table_A_sum, by = c("country", "year", "domain_landings"), all.x = T)

# some keys might not match, check how many there might be
missing_domains <- table_e_pre[is.na(table_e_pre$totwghtlandg),]
missing_domains2 = missing_domains %>% distinct(domain_landings, .keep_all = T)

length(missing_domains2$domain_landings)


# delete the missmatch values
table_e_pre2 <- filter(table_e_pre, !is.na(totwghtlandg))

table_e_pre2$nep_sub_region <-"NA"
#units
table_e_pre2$weight_unit <- "kg"
table_e_pre2$length_unit <- "cm"

#2020 testi: ikäkohtaisten mittauslukumäärä domainkohtaisen lukumäärä tilalle..
table_e_pre2$no_age_measurements <- table_e_pre2$no_age
#2020 testi: ja samalla oletettu laajennettu ikämäärä (estimaatti) NK:ksi..
table_e_pre2$no_age <- "NK"

# arrange the variables in proper order and put them to upper case
#table_E <- table_e_pre2  %>% select(country, year, domain_landings, species, totwghtlandg, no_samples_landg, no_age_measurements_landg, age_measurements_prop, min_age, max_age, age, no_age_landg, mean_weight_landg, mean_length_landg) %>% rename_all(toupper)
table_E <- table_e_pre2  %>% select(country,	year,	domain_landings, nep_sub_region, species,	totwghtlandg,	no_samples,	no_age_measurements,	age_measurements_prop,	min_age,	max_age,	age,	no_age,	mean_weight, weight_unit,	mean_length, length_unit) %>% rename_all(toupper)


# set working directory to save table E and table of deleted observations
setwd(path_out)
write.xlsx(table_E, "TABLE_E_NAO_OFR_LANDINGS_AGE.xlsx", sheetName = "TABLE_E", col.names = TRUE, row.names = FALSE)
write.xlsx(missing_domains2, "DELETED_TABLE_E.xlsx", sheetName = "TABLE_E", col.names = TRUE, row.names = FALSE)
