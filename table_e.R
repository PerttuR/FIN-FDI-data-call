
#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE E
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa, Anna-Kaisa Ylitalo
#
# Date: JUN-2018
# Updated: MAY-2022
# Updated: JUN-2023 (Perttu)
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
library(openxlsx)
library(mongolite)
library(tidyr)
library(lubridate)


#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------
# Common paths & 2022 folder:

path_tablea <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_salmon <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where salmon data lies (salmon.csv)
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2023")


# create directories if missing, but ignore warnings in case they already exist
dir.create(path_tablea, showWarnings = FALSE)
dir.create(path_out, showWarnings = FALSE)

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

# sum totwghtlandg BY year, domain_landings and species from TABLE A
table_A_sum <- table_A %>% group_by(country, year, domain_landings, species) %>% summarise(totwghtlandg = sum(as.numeric(as.character(totwghtlandg))))

# rounding the number to three digits precision
table_A_sum$totwghtlandg <- round(table_A_sum$totwghtlandg, digits = 3)

#Just to check: table_A_sum_SAL <-  filter(table_A_sum, species=="SAL")
table_A_sum_ANA <-  filter(table_A_sum, species=="SAL"| species=="TRS") #SAL = Lohi/Merilohi(Atlantic salmon) TRS=Taimen/Meritaimen(Sea trout)

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#                       2. aggregate AGE DATA for merging                       
#-------------------------------------------------------------------------------


## CHANGES 2022: in tables C, D, E and F in 2022 the variable NO_SAMPLES was replaced with TOTAL_SAMPLED_TRIPS.

source("db.R")

agedata <- read.dbTable(schema="suomu",table="report_individual", where=paste0("vuosi >= 2013 AND vuosi <= 2022"))

#-------------------------------------------------------------------------------
# choose commercial LANDINGS samples only, from years 2013-2022

landing <- filter(agedata, saalisluokka == "LANDING", name == "EU-tike(CS, kaupalliset näytteet)", !is.na(ika))

# a lot of ages are missing
landing_missing_age <- filter(agedata, saalisluokka == "LANDING", name == "EU-tike(CS, kaupalliset näytteet)", is.na(ika))


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
#       3. Import SALMON data to length classes and merge it with LANDING data                       
#--------------------------------------------------------------------------------------------

# download anadromous species sampling data from : http://suomu.rktl.fi/lohi/Report/stecf?format=csv
# and save it to workflow orig folder
# import data from salmon samples
# -> The final data from http://suomu.rktl.fi/lohi/Report/stecf?format=csv is anadromous_samples_2013_2021.csv
# Database no longer in use !!
# New database to anadromous samples from 2022 onwards is:  

ana1 <- read.csv(paste0(path_salmon, "anadromous_samples_2013_2021.csv"), sep = ";", header = T, stringsAsFactors=FALSE)




#--------------------------------------------------------------------------------------------
#       3.1 Functions to use Mongo Individual data                      
#--------------------------------------------------------------------------------------------



get.rectangle <- function(individual) {
  rectangle <- ices_rectangle[ices_rectangle$rktl_name ==first(individual$ruutu),]
  if(nrow(rectangle) != 1) {
    stop("Missing rectangle")
  }
  return(rectangle)
}

get.stock <- function(batch, rectangle) {
  if(batch$aphia_id == 127186) {
    if(rectangle$rdb_area_code == "27.3.d.32") {
      return("sal-32")
    } else {
      return("sal-2431")
    }
  }
  return("")
}

get.age <- function(individual) {
  mevu <- as.integer(individual$me_vu)
  povu <- as.integer(individual$po_vu)
  povu <- ifelse(is.na(povu), 2, povu)
  return(ifelse(is.na(mevu), NA, mevu + povu))
}


#--------------------------------------------------------------------------------------------
#       3.2 Merge ANA1 (Oracle ana-samples 2013-2021) and ANA2 (MOngo 2022 onwards samples)                    
#--------------------------------------------------------------------------------------------

source("mongo.R")

ices_rectangle <- read.csv2("ices_rectangle.csv", sep=",", dec=".")

batch <- read.mongoCollectionToDataframe("batch")
individual <- read.mongoCollectionToDataframe("individual")
species <- read.mongoCollectionToDataframe("species")
species$laji <- as.character(species$laji)
batch <- batch %>% left_join(species, by = c("species_id" = "laji"))

# Resolve trip names

batch$trip_id <- paste(batch$year, batch$lajikv1, batch$batch_number, sep="_")

#join to individual

individual <- individual %>% left_join(batch, by = c("sample_id" = "batch_id"))

individual <- individual %>%
  filter(!is.na(as.numeric(pitcm))) %>%
  mutate(length = as.numeric(pitcm) * 10) %>%
  mutate(lengthclass = length - length %% 10) %>%
  arrange(lengthclass, length)

#Age to a single number from me_vu and po_vu variables
individual$ika <- get.age(individual)

individual$month <- format(as.Date(individual$pvm, format="%Y-%m-%d"),"%m")

#add quarters
Q1 <- c("01","02","03")
Q2 <- c("04","05","06")
Q3 <- c("07","08","09")
Q4 <- c("10","11","12")
individual$quarter [individual$month %in% Q1]<-1
individual$quarter [individual$month %in% Q3]<-3
individual$quarter [individual$month %in% Q4]<-4
individual$quarter [individual$month %in% Q2]<-2

#--------------------------------------------------------------------------------------------
#       3.3 aggregate SALMON data to length classes and merge it with LANDING data                       
#--------------------------------------------------------------------------------------------


# make a key variable to match table A key (domain_discards or domain_landings)

# first make individually all the parts that form the key
country_code <- "FIN"
quarter <- ana$QUARTER
subregion <- paste("27.3.D.", ana$ICES_OA, sep = "")
gear_type <- ana$METIER
vessel_length <- "VL0010"
species <- ana$FAO
commercial_cat <- "NA"



# then combine them as a single key, identical to that from table A
ana$domain_landings <- paste(country_code, quarter, subregion, gear_type, vessel_length, species, commercial_cat, sep = "_")

## aggregate data into length classes (assuming there are no fish under 300 or over 1250)
## OBS! This information is not used - WHY?
pit_bins <- seq(300,1250, by=50)
ana$pituusluokka <- pit_bins[findInterval(ana$PITUUS, pit_bins)]

# select important variables and rename them to match landing2 data
ana2 <- ana %>% select(YEAR, DB_TRIP_ID, PITUUS, PAINO_GRAMMOINA, IKA, domain_landings) %>% rename(vuosi = YEAR, nayteno = DB_TRIP_ID, pituus = PITUUS, paino = PAINO_GRAMMOINA, ika = IKA)

#salmon2 weight from g -> to kg
ana2$paino <- ana2$paino/1000

#salmon2 length from mm -> to cm
ana2$pituus <- ana2$pituus/10

# remove missing age values
ana3 <- filter(ana2, !is.na(ika) & !is.na(paino) & !is.na(pituus))

# check missing
ana3_missing_feno_data <- filter(ana2, is.na(ika) & is.na(paino) & is.na(pituus))

# Merge SUOMU DB landings data and anadromous sampling data to one dataframe

landing3 <- merge(landing2, ana3, all = T)

#-------------------------------------------------------------------------------
#                   4. aggregate AGE DATA for merging with TABLE A                     
#-------------------------------------------------------------------------------

# aggregate data
d6_7 <- landing3 %>% group_by(vuosi, domain_landings) %>% summarise(TOTAL_SAMPLED_TRIPS = n_distinct(nayteno), no_age_measurements = n())

d9_10 <- landing3 %>% group_by(vuosi, domain_landings) %>% summarise(min_age = min(ika), max_age = max(ika)) 

d11_12_13_14 <- landing3 %>% group_by(vuosi, domain_landings, ika) %>% summarise(no_age = n(), mean_weight = round(mean(paino), digits = 3), mean_length = round(mean(pituus), digits = 1))


#-------------------------------------------------------------------------------
# merge the aggregated datas (above) to landing catch data 

landing3 <- merge(d11_12_13_14, d9_10, by = c("vuosi", "domain_landings"))

landing4 <- merge(landing3, d6_7, by = c("vuosi", "domain_landings"))

# add variables
landing4$country <- "FIN"
landing4$age_measurements_prop <- "NA"

# select only those variables important to merging with table A
landing5 <- landing4 %>% select(country, vuosi, domain_landings, TOTAL_SAMPLED_TRIPS, no_age_measurements, age_measurements_prop, min_age, max_age, ika, no_age, mean_weight, mean_length) %>% rename(year = vuosi, age = ika)


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
table_E <- table_e_pre2  %>% select(country,	year,	domain_landings, nep_sub_region, species,	totwghtlandg,	TOTAL_SAMPLED_TRIPS,	no_age_measurements,	age_measurements_prop,	min_age,	max_age,	age,	no_age,	mean_weight, weight_unit,	mean_length, length_unit) %>% rename_all(toupper)


# set working directory to save table E and table of deleted observations
openxlsx::write.xlsx(table_E, paste0(path_out,.Platform$file.sep,"TABLE_E_NAO_OFR_LANDINGS_AGE.xlsx"), sheetName = "TABLE_E", colNames = TRUE, rowNames = FALSE)
openxlsx::write.xlsx(missing_domains2, paste0(path_out,.Platform$file.sep,"DELETED_TABLE_E.xlsx"), sheetName = "TABLE_E", colNames = TRUE, rowNames = FALSE)
