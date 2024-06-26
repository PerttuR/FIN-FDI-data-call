
#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE E
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa, Anna-Kaisa Ylitalo
#
# Date: JUN-2018
# Updated: MAY-2022
# Updated: JUN-2023 (Perttu)
# Updated: JUN-2024 (Perttu, Mira and Petri)

#
# Client: LUKE EU-DCF projects
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for preparing FDI data tables E,F,C,D merging table A content from DCPROD dataflow
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
library(icesVocab)


#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------
# Common paths & 2024 data call folders:

# Paths & 2024 folder:

path_der <- paste0(getwd(), .Platform$file.sep, "der/2024/")
path_rproject <- getwd() # folder where the r project is (and the source file db.R!)
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2024")


#TO DO?
#path_salmon <- paste0(getwd(), .Platform$file.sep, "orig/") # folder where salmon data lies (salmon.csv)
# folder where the output is saved

# create directories if missing, but ignore warnings in case they already exist
dir.create(path_out, showWarnings = FALSE)

#-------------------------------------------------------------------------------
#                       1. aggregate TABLE A for merging                       
#-------------------------------------------------------------------------------

# import IC data and table A

IC_2023 <- readRDS(paste0(path_der,.Platform$file.sep,"IC_2023_DATA.rds"))
table_A <- readRDS(paste0(path_der,.Platform$file.sep,"table_A.rds"))


#table_A <- read.csv2(paste0(path_tablea,.Platform$file.sep,"A_table_2013_2022.csv"), sep = "," , na.strings = "")
#select order of columns
#table_A <- table_A %>% select(COUNTRY,	YEAR, QUARTER, VESSEL_LENGTH,	FISHING_TECH,	GEAR_TYPE,	TARGET_ASSEMBLAGE,	MESH_SIZE_RANGE,	METIER,	DOMAIN_DISCARDS,	DOMAIN_LANDINGS,	SUPRA_REGION,	SUB_REGION,	EEZ_INDICATOR,	GEO_INDICATOR,	NEP_SUB_REGION,	SPECON_TECH,	DEEP,	SPECIES,	TOTWGHTLANDG,	TOTVALLANDG,	DISCARDS,	CONFIDENTIAL)

table_A <- table_A %>% rename_all(tolower)

#colnames(table_A)    <- c("country", "year", "quarter", "vessel_length", "fishing_tech", "gear_type", "target_assemblage", "mesh_size_range", "metier", "domain_discards", "domain_landings", "supra_region", "sub_region", "eez_indicator", "geo_indicator", "specon_tech", "deep", "species", "totwghtlandg", "totvallandg", "discards", "confidential")


#-------------------------------------------------------------------------------
#                       1.1 create DOMAIN landing to IC data                       
#-------------------------------------------------------------------------------

IC_2023$Country <- "FIN"
IC_2023$quarter <- IC_2023$Season
IC_2023$subregion <- IC_2023$FishingArea
IC_2023$gear_type <- case_when(IC_2023$Fleet == "Trapnet" ~ "FPO-FPN-FYK",
                               IC_2023$Fleet == "Pelagic trawl" ~ "OTM-PTM",
                               IC_2023$Fleet == "Gillnet" ~ "GNS",
                               IC_2023$Fleet == "Active" ~ "OTM-PTM",
                               IC_2023$Fleet == "Pelagic trawlers" ~ "OTM-PTM",
                               IC_2023$Fleet == "Passive" ~ "GNS-FYK"
                               )
IC_2023$vessel_length <- "all"
IC_2023$TARGET_ASSEMBLAGE <- case_when(IC_2023$Species == "HER"| IC_2023$Species == "SPR"~ "SPF")
                                       
e1 <- IC_2023 %>% mutate(DOMAIN_LANDINGS = paste0(
                          Country, "_", # country
                         quarter, "_", # quarter
                         subregion, "_", # region
                         gear_type, "_", # gear type
                         TARGET_ASSEMBLAGE, "_", # target assemblage
                         "all_", # mesh size range
                         "NA_", # selective device / metier
                         "NA_", # mesh size range of the selective device
                         "all_", # vessel length
                         Species, "_", # species
                         "all" # commercial category
                         )
                  )


#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#                       2. aggregate AGE DATA for merging                       
#-------------------------------------------------------------------------------


## CHANGES 2022: in tables C, D, E and F in 2022 the variable NO_SAMPLES was replaced with TOTAL_SAMPLED_TRIPS.

source("db.R")

agedata <- read.dbTable(schema="suomu",table="report_individual", where=paste0("vuosi >= 2023 AND vuosi <= 2023"))

#-------------------------------------------------------------------------------
# choose commercial LANDINGS samples only, from years 2013-2022

agedata_cs <- agedata |> filter(name == "EU-tike(CS, kaupalliset näytteet)")

#Filter ages, 99 is essentially NA
suomu <- agedata_cs |> filter(!is.na(age) & age != "99")

# a lot of ages are missing
suomu_missing_age <- agedata_cs |> filter(is.na(age) | age == "99")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# make a key variable to match table A key (domain_discards or domain_landings)

# first make individually all the parts that form the key
country_code <- "FIN"
quarter <- suomu$q
subregion <- paste("27.3.d.", suomu$ices_osa_alue, sep = "")
#Stat dep uses FPO instead of FPN so change
suomu <- suomu %>% mutate(metier = replace(metier,metier=="FPN_FWS_>0_0_0","FPO_FWS_>0_0_0"))
suomu <- suomu %>% mutate(metier = replace(metier,metier=="FPN_SPF_>0_0_0","FPO_SPF_>0_0_0"))
gear_type <- suomu$gear_fishframe
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

# select only important variables
suomu2 <- suomu %>% select(vuosi, nayteno, paino, pituus, age, domain_landings)

#landing2 length from mm -> to cm
suomu2$pituus <- suomu2$pituus/10


# Merge Logbook landings data and anadromous sampling data to one dataframe

#-------------------------------------------------------------------------------
#                   4. aggregate AGE DATA for merging with TABLE A     2024 alkaen:                
#-------------------------------------------------------------------------------

# aggregate data separately:
suomu2_year_domain <- suomu2 |>
  group_by(vuosi, domain_landings) |>
  summarise(
            total_sampled_trips = n_distinct(nayteno),
            no_age_measurements = n(),
            min_age = min(age),
            max_age = max(age))


suomu2_year_domain_age <- suomu2 |>
  group_by(vuosi, domain_landings, age) |>
  summarise(
            no_age = n(),
            mean_weight = round(mean(paino),digits = 3),
            mean_length = round(mean(pituus), digits = 1))

suomu2_year_domain <- suomu2_year_domain |> left_join(suomu2_year_domain_age, relationship="many-to-many")

suomu2_year_domain$country <- "FIN"
suomu2_year_domain$age_measurements_prop <- "NA"

suomu2_e1 <- suomu2_year_domain |> select(country, year=vuosi, domain_landings, total_sampled_trips, no_age_measurements, age_measurements_prop, min_age, max_age, age, no_age, mean_weight, mean_length)

#-------------------------------------------------------------------------------
#                       3. Merge SAMPLED DATA with TABLE A                       
#-------------------------------------------------------------------------------

#e1 to lower
e1 <- e1 %>% rename_all(tolower)

#poistetaan alusryhmittely ja gear type, jotke tekevät monia rivejä per DOMAIN
table_A_SUM <- table_A %>% group_by(country,year,domain_landings,species) %>% summarise(totwghtlandg=sum(totwghtlandg))

# merge IC age data with TABLE A
table_e_pre1 <- e1 %>% left_join(table_A_SUM, by=join_by("country", "year", "domain_landings", "species"))

# TEST some keys might not match, check how many there might be
missing_domains_IC <- table_e_pre1[is.na(table_e_pre1$totwghtlandg),]
missing_domains_IC_DISTINCT = missing_domains_IC %>% distinct(domain_landings, .keep_all = T)
length(missing_domains_IC_DISTINCT$domain_landings)
#Liittyneet:
domains_IC <- table_e_pre1[!is.na(table_e_pre1$totwghtlandg),]
domains_IC_DISTINCT <- domains_IC%>% distinct(domain_landings, .keep_all = T)
length(domains_IC_DISTINCT$domain_landings)

#Aggrekoi ensin Suomu yksilödata samaan sapluunaan ja tee sitten left joini :)
# merge SUOMU age data with TABLE A
table_e_suomu <- left_join(suomu2_e1, table_A, by = join_by(country, year, domain_landings))

# TEST some keys might not match, check how many there might be
missing_domains_SUOMU <- table_e_suomu[is.na(table_e_suomu$totwghtlandg),]
missing_domains_SUOMU_DISTINCT = missing_domains_SUOMU %>% distinct(domain_landings, .keep_all = T)
length(missing_domains_SUOMU_DISTINCT$domain_landings)
#Liittyneet:
domains_SUOMU <- table_e_suomu[!is.na(table_e_suomu$totwghtlandg),]
domains_SUOMU_DISTINCT <- domains_SUOMU%>% distinct(domain_landings, .keep_all = T)
length(domains_SUOMU_DISTINCT$domain_landings)




# delete the missmatch values
table_e_pre1 <- filter(table_e_pre1, !is.na(totwghtlandg))
table_e_suomu <- filter(table_e_suomu, !is.na(totwghtlandg))

table_e_pre1$age_measurements_prop <-"NA"
table_e_pre1$nep_sub_region <- "NA"
#units
table_e_pre1$weight_unit <- "g"
table_e_pre1$length_unit <- "cm"

table_e_pre1 <- table_e_pre1 |> mutate(age = as.integer(agelength))
table_e_pre1 <- table_e_pre1 |> group_by(country, year, domain_landings,species) |> mutate(min_age = min(age), max_age = max(age))
table_e_pre1$no_age <- as.numeric(table_e_pre1$numbercaught)*1e6
# arrange the variables in proper order and put them to upper case
#table_E <- table_e_pre2  %>% select(country, year, domain_landings, species, totwghtlandg, no_samples_landg, no_age_measurements_landg, age_measurements_prop, min_age, max_age, age, no_age_landg, mean_weight_landg, mean_length_landg) %>% rename_all(toupper)


table_e_pre2 <- table_e_pre1 |> select(
       country,
       year,
       domain_landings,
       nep_sub_region,
       species,
       totwghtlandg,
       total_sampled_trips=numsamplesage,
       no_age_measurements=numagemeas,
       age_measurements_prop,
       min_age,
       max_age,
       age,
       no_age,
       mean_weight=meanweight,
       weight_unit,
       mean_length=meanlength,
       length_unit
)

table_e_pre2 <- table_e_pre2 |> mutate(
       no_age = as.integer(no_age),
       mean_weight = as.numeric(mean_weight),
       no_age_measurements = as.integer(no_age_measurements),
       total_sampled_trips = as.integer(total_sampled_trips)
)

#-9 removal
table_e_pre2 <- table_e_pre2 |> mutate(
       total_sampled_trips = na_if(total_sampled_trips, -9),
       no_age_measurements = na_if(no_age_measurements, -9),
       no_age = na_if(no_age, -9),
       mean_length = na_if(mean_length, -9)
)

#NK input
table_e_pre2 <- table_e_pre2 |> mutate(
       total_sampled_trips = replace_na(as.character(total_sampled_trips), "NK"),
       no_age_measurements = replace_na(as.character(no_age_measurements), "NK"),
       no_age = replace_na(no_age, "NK"),
       mean_length = replace_na(as.character(mean_length), "NK")
)

table_E <- table_e_pre2 |> rename_all(toupper)

table_E <- table_E |> arrange(COUNTRY,YEAR,DOMAIN_LANDINGS,SPECIES,AGE)

# set working directory to save table E and table of deleted observations
openxlsx::write.xlsx(table_E, paste0(path_out,.Platform$file.sep,"FIN_TABLE_E_NAO_OFR_LANDINGS_AGE.xlsx"), sheetName = "TABLE_E", colNames = TRUE, rowNames = FALSE)
#openxlsx::write.xlsx(missing_domains2, paste0(path_out,.Platform$file.sep,"DELETED_TABLE_E.xlsx"), sheetName = "TABLE_E", colNames = TRUE, rowNames = FALSE)

suomu2_e <- suomu2_e1 |> rename_all(toupper)
mega_E <- table_E |> full_join(suomu2_e, relationship="many-to-many", by=join_by(COUNTRY, YEAR, DOMAIN_LANDINGS, AGE), suffix=c("_A","_SUOMU"))

openxlsx::write.xlsx(mega_E, paste0(path_out,.Platform$file.sep,"FIN_TABLE_MEGA_E.xlsx"), sheetName = "TABLE_E", colNames = TRUE, rowNames = FALSE)

message("Matching rows: ", mega_E |> filter(!is.na(NO_AGE_A) & !is.na(NO_AGE_SUOMU)) |> ungroup() |> tally())
message("Missing in suomu: ", mega_E |> filter(!is.na(NO_AGE_A) & is.na(NO_AGE_SUOMU)) |> ungroup() |> tally())
message("Missing in table_A: ", mega_E |> filter(is.na(NO_AGE_A) & !is.na(NO_AGE_SUOMU)) |> ungroup() |> tally())

