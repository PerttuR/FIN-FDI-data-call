
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


# install.packages("RPostgreSQL")
# install.packages("dplyr")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)

# set working directory to read files from
setwd("C:/2018/FDI/work/data/orig/")

#-------------------------------------------------------------------------------
# import table A
table_A <- read.csv2("FIN_TABLE_A_CATCH.csv", sep = "," )
#-------------------------------------------------------------------------------

# sum totwghtlandg and unwanted_catch BY year, domain_discards and species from TABLE A
test <- table_A %>% group_by(year, domain_discards, species) %>% summarise(totwghtlandg = sum(as.numeric(as.character(totwghtlandg))), unwanted_catch = sum(as.numeric(as.character(unwanted_catch))))

#-------------------------------------------------------------------------------

# import data from samples (Suomu), length classes
setwd("C:/2018/FDI/work/prog/FIN-FDI-data-call/")

source("db.R")

spec <- read.dbTable("suomu","species")
lengthdata <- read.dbTable("suomu","report_lengthclassrecords")
#-------------------------------------------------------------------------------
# choose commercial DISCARD samples only, from years 2015-2017

unwanted <- filter(lengthdata, saalisluokka == "DISCARD", projekti == "EU-tike(CS, kaupalliset näytteet)", vuosi >= 2015 & vuosi <= 2017)

#-------------------------------------------------------------------------------
# make a key variable to match table A key (domain_discards or domain_landings)
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

unwanted$vessel_length_code[is.na(unwanted$laivan_pituus_cm)] <- "NONE"
vessel_length <- unwanted$vessel_length_code
  
species <- unwanted$fao
commercial_cat <- "NA"

unwanted$domain_discards <- paste(country_code, quarter, subregion, gear_type, vessel_length, species, commercial_cat, sep = "_")
#-------------------------------------------------------------------------------
# sum  year, domain_discards and species from unwanted catch data
# aggregated on DOMAIN level (by domain_discards) and year


#number of samples (number of TRIPS) 
d_7 <- unwanted %>% group_by(domain_discards) %>% summarise(no_samples_uc = n_distinct(nayteno)) #### not yet finished

#number of length measurements 
d_8 <- unwanted %>% group_by(vuosi, domain_discards) %>% summarise(no_length_measurements_uc = sum(as.numeric(pituusluokan_kpl_maara)))

# minimum and maximum lengths (notice! this is done by trip as well)
d10_11 <- unwanted %>% group_by(vuosi, domain_discards, nayteno) %>% summarise(min_length = sum(min(pituusluokka)), max_length = sum(max(pituusluokka)))










# säläääää










t2 <- unwanted %>% group_by(vuosi, domain_discards, fao) %>% summarise(min_length = sum(min(pituusluokka)))



h2 <- filter(unwanted, vuosi == 2015, domain_discards == "FIN_4_27.3.D.30_GNS_FWS_>0_0_0_VL0010_FPP_NA")




t3 <- unwanted %>% summarise(min_length = sum(min(pituusluokka)), max_length = sum(max(pituusluokka)))


t4 <- unwanted %>% mutate(min_length = min())













