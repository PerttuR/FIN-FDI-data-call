#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE B
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa, Marta Suska
#
# Date: JUN-2018
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for preparing FDI data tables from Table A fron stat DEP (Pirkko)
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
#path_tablea <- "C:/2018/FDI/work/data/orig/" # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
#path_rproject <- "C:/2018/FDI/work/prog/FIN-FDI-data-call/" # folder where the r project is (and the source file db.R!)
#path_salmon <- "C:/2018/FDI/work/data/orig/" # folder where the salmon data is (stecf.csv)
#path_out <- "C:/2018/FDI/work/data/der/" # folder where the output is saved

# Perttu:
#path_tablea <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call/orig" # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call" # folder where the r project is (and the source file db.R!)
#path_salmon <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call/orig" # folder where the salmon data is (stecf.csv)
path_out <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call/results" # folder where the output is saved


#-------------------------------------------------------------------------------
#                       1. set TABLE B columns                       
#-------------------------------------------------------------------------------

setwd(path_rproject)

source("db.R")

lottery_results <- read.dbTable("suomu","sampling_result")
lottery_raw <- read.dbTable("suomu","sampling_source")
lottery_weights <- read.dbTable("suomu","sampling_source_weight")
species <- read.dbTable("suomu","species")

seurantataulukot <- read.dbTable("suomu","tracking_metier_name")
tracking_metier <- read.dbTable("suomu","tracking_metier")
metier <- read.dbTable("suomu","metier")

design_metiers <- seurantataulukot %>% filter(sampling_target == TRUE)
design_metiers2 <- design_metiers %>% left_join(tracking_metier, by = c("id" = "tracking_metier_name_fk"))
design_metiers3 <- design_metiers2 %>% left_join(metier, by = c("metier_fk" = "id"))


lottery_raw$real_year <- lottery_raw$year + 1
lottery_raw2 <- lottery_raw %>% left_join(lottery_results, by = c("id" = "sample_source_fk"))

lottery_raw3 <- lottery_raw2 %>% group_by(real_year, quarter, area, tracking_metier_name_fk) %>% summarise(REFUSAL = length(as.numeric(as.character(status))))


tracking_metier2 <- tracking_metier %>% left_join(design_metiers)


design_metiers$metier_fk <-  design_metiers$id
design_metiers <- design_metiers %>% select(metier_fk)



table_b <- table_b %>% select(COUNTRY, YEAR, SAMPLE_FRAME, REFUSAL_RATE, COVERAGE_RATE, NONRESPONSE_RATE, VESSELS_FLEET, TRIPS_FLEET, TRIPS_SAMPLED_ONBOARD, UNIQUE_VESSELS_SAMPLED, UNIQUE_VESSELS_CONTACTED, NOT_AVAILABLE, NO_CONTACT_DETAILS, NO_ANSWER, OBSERVER_DECLINED, INDUSTRY_DECLINED, SUCCESS_RATE, TOT_SELECTIONS)

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#                       2. set TABLE B columns                      
#-------------------------------------------------------------------------------

table_b$COUNTRY <-"FIN"
table_b$YEAR <-"2019"
table_b$SAMPLE_FRAME <- "B5(OTM_SPF&PTM_SPF) Q2SD30"

#-------------------------------------------------------------------------------
# choose upper (WP&AR) hierarcy gears/metiers only (DEL national sub metiers)

wp_gears <- c("OTM", "PTM", "FYK", "GNS")
metiers <- filter(metiers, gear_code %in% wp_gears) 


years <- c(2015, 2016, 2017, 2018)

table_b <- expand.grid(years,metiers$level6)
table_b <- table_b %>% rename(year = Var1, level6=Var2)

#select project and years
samples <- samples %>% filter(project == "EU-tike(CS, kaupalliset näytteet)" & year %in% c(2015,2016,2017,2018))
#merge national metiers to DCF
samples <- samples %>% mutate(level6 = replace(level6, level6 == 'FPN_FWS_>0_0_0', 'FYK_FWS_>0_0_0'), level6 = replace(level6, level6 == 'FPN_SPF_>0_0_0', 'FYK_SPF_>0_0_0'))
#summarise samples
samples <- samples %>% group_by(year, level6) %>% summarise(trip_count = sum(trip_count))



# import data from salmon samples
setwd(path_tablea)
salmon <- read.csv("stecf.csv", sep = ";", header = T)
salmon2 <- salmon %>% group_by(YEAR, METIER) %>% summarise(trip_count = n_distinct(DB_TRIP_ID))
salmon2 <- salmon2 %>% rename(year = YEAR, level6=METIER)
salmon2 <- salmon2 %>% filter(level6 == "FYK_ANA_0_0_0")
salmon2$level6 <- 'FYK_ANA_>0_0_0'

samples <- bind_rows(samples, salmon2)

table_b <- left_join(table_b,samples,  by = c("year", "level6"))

table_b <- table_b %>% rename(YEAR = year, SAMPLE_FRAME = level6, SUCCESFUL_SAMPLE = trip_count)
table_b <- table_b[order(table_b$YEAR),]

table_b <- table_b[-which(is.na(table_b$SUCCESFUL_SAMPLE)),]

COUNTRY <- "FIN"
REFUSAL_RATE <- "NK"
COVERAGE_RATE <- "NK"
NONRESPONSE_RATE <- "NK"
VESSELS_FLEET <- "NK"
TRIPS_FLEET <- "NK"
TRIPS_SAMPLED_ONBOARD <- 0
UNIQUE_VESSEL_SAMPLED <- "NK"
VESSELS_CONTACTED <- "NK"
NOT_AVAILABLE <- "NK"
NO_CONTACT_DETAILS <- "NK"
NO_ANSWER <- "NK"
OBSERVER_DECLINED	<- "NK"
INDUSTRY_DECLINED	<- "NK"
#SUCCESFUL_SAMPLE <-"NK"
TOT_SELECTIONS <- "NK"


table_b$COUNTRY <- COUNTRY
table_b$REFUSAL_RATE <- REFUSAL_RATE
table_b$COVERAGE_RATE <- COVERAGE_RATE
table_b$NONRESPONSE_RATE <- NONRESPONSE_RATE
table_b$VESSELS_FLEET <- VESSELS_FLEET
table_b$TRIPS_FLEET <- TRIPS_FLEET
table_b$TRIPS_SAMPLED_ONBOARD <- TRIPS_SAMPLED_ONBOARD
table_b$UNIQUE_VESSEL_SAMPLED <- UNIQUE_VESSEL_SAMPLED
table_b$VESSELS_CONTACTED <- VESSELS_CONTACTED
table_b$NOT_AVAILABLE <- NOT_AVAILABLE
table_b$NO_CONTACT_DETAILS <- NO_CONTACT_DETAILS
table_b$NO_ANSWER <- NO_ANSWER
table_b$OBSERVER_DECLINED <- OBSERVER_DECLINED	
table_b$INDUSTRY_DECLINED <- INDUSTRY_DECLINED	
#table_b$SUCCESFUL_SAMPLE <- SUCCESFUL_SAMPLE	
table_b$TOT_SELECTIONS <- TOT_SELECTIONS

table_b <- table_b %>% select(COUNTRY,	YEAR,	SAMPLE_FRAME,	REFUSAL_RATE,	COVERAGE_RATE,	NONRESPONSE_RATE,	VESSELS_FLEET,	TRIPS_FLEET,	TRIPS_SAMPLED_ONBOARD,	UNIQUE_VESSEL_SAMPLED,	VESSELS_CONTACTED,	NOT_AVAILABLE,	NO_CONTACT_DETAILS,	NO_ANSWER,	OBSERVER_DECLINED,	INDUSTRY_DECLINED,	SUCCESFUL_SAMPLE,	TOT_SELECTIONS)

#TO DO.....
setwd(path_tablea)
trips <- read.xlsx("Tripit ja alukset 2015-2018.xlsx", sheet="rk_pvk", header = T)
?read.xlsx

# set working directory to save table B and table
setwd(path_out)
write.csv(table_b, "FIN_TABLE_B_REFUSAL_RATE.csv", row.names = F)
#install.packages('xlsx')
library(xlsx)
write.xlsx(table_b, "FIN_TABLE_B_REFUSAL_RATE.xlsx", sheetName="Sheet1", col.names=TRUE, row.names=FALSE,)


