#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE F
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
#
# Date: JUN-2018 by Mira
# Updated: JUN 2021 by Perttu
# Updated: JUN 2022 by Perttu
# Updated: JUN 2023 by Perttu
# Updated: JUN 2023 by Perttu, Mira and Petri
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



#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------

path_der <- paste0(getwd(), .Platform$file.sep, "der/2024/")
path_rproject <- getwd() # folder where the r project is (and the source file db.R!)
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2024")

# create directories if missing, but ignore warnings in case they already exist
dir.create(path_out, showWarnings = FALSE)


#-------------------------------------------------------------------------------
#                       1. aggregate TABLE A for merging                       
#-------------------------------------------------------------------------------

table_A <- readRDS(paste0(path_der,.Platform$file.sep,"table_A.rds"))


# import table A
#table_A <- read.csv2(paste0(path_tablea,.Platform$file.sep,"A_table_2013_2022.csv"), sep = "," , na.strings = "")
#select order of columns
table_A <- table_A %>% select(COUNTRY,	YEAR, QUARTER, VESSEL_LENGTH,	FISHING_TECH,	GEAR_TYPE,	TARGET_ASSEMBLAGE,	MESH_SIZE_RANGE,	METIER,	DOMAIN_DISCARDS,	DOMAIN_LANDINGS,	SUPRA_REGION,	SUB_REGION,	EEZ_INDICATOR,	GEO_INDICATOR,	NEP_SUB_REGION,	SPECON_TECH,	DEEP,	SPECIES,	TOTWGHTLANDG,	TOTVALLANDG,	DISCARDS,	CONFIDENTIAL)

table_A <- table_A %>% rename_all(tolower)

#colnames(table_A)    <- c("country", "year", "quarter", "vessel_length", "fishing_tech", "gear_type", "target_assemblage", "mesh_size_range", "metier", "domain_discards", "domain_landings", "supra_region", "sub_region", "eez_indicator", "geo_indicator", "specon_tech", "deep", "species", "totwghtlandg", "totvallandg", "discards", "confidential")

#-------------------------------------------------------------------------------

# sum totwghtlandg BY year, domain_landings and species from TABLE A
table_A_sum <- table_A %>% group_by(country, year, domain_landings, species) %>% summarise(totwghtlandg = sum(as.numeric(as.character(totwghtlandg)))) %>% filter(species == 'HER' | species == 'SPR')

# rounding the number to three digits precision
table_A_sum$totwghtlandg <- round(table_A_sum$totwghtlandg, digits = 3)

#Just to check: table_A_sum_SAL <-  filter(table_A_sum, species=="SAL")
#ehkä Elokuussa tätä?
#table_A_sum_ANA <-  filter(table_A_sum, species=="SAL"| species=="TRS") #SAL = Lohi/Merilohi(Atlantic salmon) TRS=Taimen/Meritaimen(Sea trout)

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#                       2. SAMPLED DATA for merging                       
#-------------------------------------------------------------------------------


## CHANGES 2022: in tables C, D, E and F in 2022 the variable NO_SAMPLES was replaced with TOTAL_SAMPLED_TRIPS.


# import data from samples (Suomu), length classes from national DCF database

source("db.R")

lengthdata <- read.dbTable("suomu","report_lengthclassrecords")


#-------------------------------------------------------------------------------
# choose commercial LANDING samples only, from years 2013-2023 HUOM 2023 valittu kesäkuussa 2024 toimituksessa..
 
suomu <- filter(lengthdata, projekti == "EU-tike(CS, kaupalliset näytteet)", vuosi >= 2023 & vuosi <= 2023,fao %in% c("HER", "SPR"))

suomu$metier <- suomu$metiers_fk

#-------------------------------------------------------------------------------

# make a key variable to match table A key (domain_discards or domain_landings)

# first make individually all the parts that form the key
# make a key variable to match table A key (domain_discards or domain_landings)

# first make individually all the parts that form the key
country_code <- "FIN"
quarter <- suomu$q
subregion <- paste("27.3.d.", suomu$ices_osa_alue, sep = "")
#Stat dep uses FPO instead of FPN so change
suomu <- suomu %>% mutate(metier = replace(metier,metier=="FPN_FWS_>0_0_0","FPO_FWS_>0_0_0"))
suomu <- suomu %>% mutate(metier = replace(metier,metier=="FPN_SPF_>0_0_0","FPO_SPF_>0_0_0"))
gear_type <- suomu$fishframe
gear_type <- case_when(gear_type == "FPO" | gear_type == "FPN" | gear_type == "FYK" ~ "FPO-FPN-FYK",
                       gear_type == "OTM" | gear_type == "PTM" ~ "OTM-PTM",
                       TRUE ~ gear_type
)

unique(gear_type)

TARGET_ASSEMBLAGE <- substr(suomu$metier,5,7)#metieristä merkit 5-7
mesh_size <- "all"
selective_device <- "NA"
mesh_size_range_selective_device <- "NA"
vessel_length <- "all"
species <- suomu$fao
commercial_cat <- "all"

# then combine them as a single key, identical to that from table A
suomu$domain_landings <- paste(country_code, quarter, subregion, gear_type, TARGET_ASSEMBLAGE, mesh_size, selective_device, mesh_size_range_selective_device, vessel_length, species, commercial_cat, sep = "_")

suomu$species <- suomu$fao

suomu2 <- suomu %>% select(vuosi, domain_landings, species, nayteno, pituusluokka, pituusluokan_kpl_maara, pituusluokan_kokpaino)


#-----------------------------------------------------------------------------------
#                       4. aggregate SAMPLED DATA for merging with TABLE A                       
#-----------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# aggregate data on different levels according to Annex D instructions from the Official Letter

#number of samples (number of TRIPS) + sum class bumber + mean weight at length
d6_7 <- suomu2 %>% group_by(vuosi, domain_landings, species) %>% summarise(TOTAL_SAMPLED_TRIPS = n_distinct(nayteno), no_length_measurements = sum(pituusluokan_kpl_maara)) 

# minimum and maximum lengths (notice! this is done by trip as well)
d9_10 <- suomu2 %>% group_by(vuosi, domain_landings, species) %>% summarise(min_length = sum(min(pituusluokka)), max_length = sum(max(pituusluokka)))

#number of length measurements 
d11_12 <- suomu2 %>% group_by(vuosi, domain_landings,species, pituusluokka) %>% summarise(no_length = sum(pituusluokan_kpl_maara), mean_weight_at_length = mean(pituusluokan_kokpaino/pituusluokan_kpl_maara, na.rm = TRUE))
d11_12$mean_weight_at_length <- round(d11_12$mean_weight_at_length, digits = 0)



# merge the aggregated datas (above) to landing catch data 

landing4 <- merge(d11_12, d9_10, by = c("vuosi", "domain_landings","species"))

landing5 <- merge(landing4, d6_7, by = c("vuosi", "domain_landings","species"))

# add length_unit and country variables
landing5$length_unit <- "mm"
landing5$country = "FIN"

#-------------------------------------------------------------------------------

landing5$no_length <- "NK"
landing5$weight_unit <-"g"
landing5$nep_sub_region <-"NA"


# select only those variables important to merging with table A
landing6 <- landing5 %>% select(country, vuosi, domain_landings, nep_sub_region, TOTAL_SAMPLED_TRIPS, no_length_measurements, min_length, max_length, length_unit, pituusluokka, no_length, mean_weight_at_length, weight_unit) %>% rename(year = vuosi, length = pituusluokka)


#-------------------------------------------------------------------------------
#                       5. Merge SAMPLED DATA with TABLE A                       
#-------------------------------------------------------------------------------

# 2022 data has NaN values -> delete
table_A_sum <- table_A_sum

# merge landing catch data with TABLE A
table_f_pre <- merge(landing6, table_A_sum, by = c("country", "year", "domain_landings"), all.x = T)

# some keys might not match, check how many there might be
missing_domains <- table_f_pre[is.na(table_f_pre$totwghtlandg),]
missing_domains2 = missing_domains %>% distinct(domain_landings, .keep_all = T)

length(missing_domains2$domain_landings)


# delete the missmatch values
table_f_pre2 <- filter(table_f_pre, !is.na(totwghtlandg))
suomu_deleted_for_no_catch <- filter(table_f_pre, is.na(totwghtlandg))

table_f_pre3 <- na.omit(table_f_pre2)


# arrange the variables in proper order and put them to upper case
#table_F <- table_f_pre2 %>% select(country, year, domain_landings, species, totwghtlandg, no_samples_landg, no_length_measurements_landg, length_unit, min_length, max_length, length, no_length_landg) %>% rename_all(toupper)
table_F <- table_f_pre3 %>% select(country, year, domain_landings, nep_sub_region, species, totwghtlandg, TOTAL_SAMPLED_TRIPS, no_length_measurements, length_unit, min_length, max_length, length, no_length, mean_weight_at_length, weight_unit) %>% rename_all(toupper)

# set working directory to save table F and table of deleted observations
openxlsx::write.xlsx(table_F,  paste0(path_out,.Platform$file.sep,"TABLE_F_NAO_OFR_LANDINGS_LENGTH.xlsx"), sheetName = "TABLE_F", colNames = TRUE, rowNames = FALSE)
openxlsx::write.xlsx(missing_domains2,  paste0(path_out,.Platform$file.sep,"TABLE_F_NAO_OFR_LANDINGS_LENGTH_DELETED_TABLE_F.xlsx"), sheetName = "TABLE_F", colNames = TRUE, rowNames = FALSE)
openxlsx::write.xlsx(suomu_deleted_for_no_catch,  paste0(path_out,.Platform$file.sep,"TABLE_F_NAO_OFR_LANDINGS_LENGTH_DELETED_from_sampling_cause_no_catch.xlsx"), sheetName = "TABLE_F", colNames = TRUE, rowNames = FALSE)

