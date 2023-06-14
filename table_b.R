#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE B
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
#
# Creation Date: JUL-2019 (Petri and Perttu)
# Updated: April 2022 (Perttu)
# Updated: Jun 2023 (Perttu, Petri)
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# Table B data is from Finish eu-dcf sampling database Suomu
# sampling frame data from FIN sample lottery machine system
#-------------------------------------------------------------------------------


#install.packages("RPostgreSQL")
# install.packages("dplyr")


#- Clear workspace
rm(list=ls())

# needed libraries test
library(dplyr)
library(RPostgreSQL)
library(openxlsx)


#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------

# Paths:
path_tablea <- paste0(getwd(), .Platform$file.sep, "orig") # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- getwd() # folder where the r project is (and the source file db.R!)
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep, "results", .Platform$file.sep, "2023")

# create directories if missing, but ignore warnings in case they already exist
dir.create(path_tablea, showWarnings = FALSE)
dir.create(path_out, showWarnings = FALSE)

#-------------------------------------------------------------------------------
#                       1. set TABLE B columns                       
#-------------------------------------------------------------------------------


source("db.R")

#Fetch sampling lottery tables from SUOMU sampling database
sampling_result <- read.dbTable("suomu","sampling_result")
sampling_source <- read.dbTable("suomu","sampling_source")
sampling_source_weight <- read.dbTable("suomu","sampling_source_weight")
species <- read.dbTable("suomu","species")
gear <- read.dbTable("suomu","gear")
#follow up tables from database
seurantataulukot <- read.dbTable("suomu","tracking_metier_name")
tracking_metier <- read.dbTable("suomu","tracking_metier")
tracking_species <- read.dbTable("suomu", "tracking_species");
metier <- read.dbTable("suomu","metier")
#get trip and haul data from database
trip <- read.dbTable("suomu", "trip")
haul <- read.dbTable("suomu", "haul")

#set reference year PSU counts to sampling year. 
sampling_source_fixed_year <- sampling_source %>% mutate(year = year+1)

#lottery status levels map table:
sampling_result_status_map <- data.frame(key=seq(from=0,to=7),value=c("assigned","inactive","rejected","sampled","out of area","no_contact_details","no_answer","observer_declined"))

#join status to sampling PSU source data
sampling_result_source_fixed_year <- sampling_result %>%
  left_join(sampling_source_fixed_year, by=c("sample_source_fk"="id")) %>%
  left_join(sampling_result_status_map, by=c("status" = "key")) %>% mutate(status = value)


#lottery machine was operational since 2018 Q4
trip_selected <- trip %>% filter(!is.na(target_species_fk) & year >= 2018)
trip_haul <- trip_selected %>% left_join(haul, by=c("id" = "trip_fk"))

#join trip haul and metier to form SAMPLING_FRAME column:
#-----
trip_haul_metier <- trip_haul %>% left_join(metier, by=c("metier_fk" = "id"))

species_gear_code <- trip_haul_metier %>% select(target_species_fk, level5)

species_gear_code_tally <- species_gear_code %>% group_by(target_species_fk, level5) %>% tally()

species_gear_code_tally2 <- species_gear_code_tally %>% top_n(1, n)

species_gear_code_tally3 <- species_gear_code_tally2 %>% mutate(frame = case_when(
  (target_species_fk == 22 | target_species_fk == 45) & (level5 == "OTM_SPF" | level5 == "PTM_SPF") ~ "Pelagic trawl(OTM/PTM)",
  target_species_fk == 1 & (level5 == "OTM_FWS" | level5 == "PTM_FWS") ~ "Freshwater trawl(Vendace)",
  TRUE ~ ""))

species_metier_map <- species_gear_code_tally3

#-----


#-------------------------------------------------------------------------------
#                       2. set TABLE B columns                      
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# tekstiä

#wp_gears <- c("OTM", "PTM", "FYK", "GNS")
#metiers <- filter(metier, gear_code %in% wp_gears) 

#FDI data call years:
years <- seq(from=2013, to=2022)

super_round <- function(a) {
  round(as.numeric(as.character(a)), digits = 2)
}

sampling_result_source_filtered <- sampling_result_source_fixed_year %>% filter(year %in% years)

#join species and metier texts to sampling result table data:
sampling_result_frame <- sampling_result_source_filtered %>%
  left_join(tracking_species, by="tracking_metier_name_fk") %>%
  left_join(species_metier_map, by=c("species_fk"= "target_species_fk"))

#Set data call columns
sampling_result_frame_quarter_sd <- sampling_result_frame %>% mutate(frame = paste0(frame, ' Q', quarter, ' SD', area))
sampling_result_frame_quarter_sd$year <- as.factor(sampling_result_frame_quarter_sd$year)
sampling_result_frame_quarter_sd$frame <- as.factor(sampling_result_frame_quarter_sd$frame)
sampling_result_frame_quarter_sd$COUNTRY <- "FIN";

#form table B:
table_b <- sampling_result_frame_quarter_sd %>% select(COUNTRY, YEAR=year, SAMPLE_FRAME=frame) %>% distinct() %>%
  arrange(YEAR, SAMPLE_FRAME)

#table_b$REFUSAL_RATE <- 0
table_b$VESSELS_FLEET <- "NK"
table_b$TRIPS_FLEET <- "NK"
table_b$TRIPS_SAMPLED_ONBOARD <- 0
table_b$UNIQUE_VESSELS_SAMPLED <- 0
#table_b$UNIQUE_VESSELS_CONTACTED <- "NK"
table_b$NOT_AVAILABLE <- "NK"
table_b$NO_CONTACT_DETAILS <- 0
#table_b$NO_ANSWER <- "NK"
#table_b$OBSERVER_DECLINED <- 0 #Not collected as a status
#table_b$INDUSTRY_DECLINED <- 0 #Not collected as a status
table_b$TOT_SELECTIONS <- 0

#Aggragate table B to sampling frame level:
sampling_result_grouped <- sampling_result_frame_quarter_sd %>% mutate(year_frame = paste0(year," ",frame))
sampling_result_grouped$year_frame <- as.factor(sampling_result_grouped$year_frame)
sampling_result_grouped <- sampling_result_grouped %>% group_by(year_frame, .drop = FALSE) %>% arrange(year_frame)

#TODO: call count

#count Refusals & nonresponses
tally_rejection <- sampling_result_grouped %>% filter(status == "rejected") %>% tally()

tally_nonresponses <- sampling_result_grouped %>% filter(status %in% c("rejected","no_contact_details","no_answer","observer_declined")) %>% tally()



#count contacts
tally_all <- sampling_result_grouped %>% summarise(sum=sum(if_else(call_count == 0,1,as.double(call_count))))

#Traverse the sampling_source diamond in the reverse direction
sampling_diamond_reversal <- sampling_source_fixed_year %>%
  left_join(sampling_source_weight, by=c("id" = "sampling_source_fk")) %>%
  left_join(species_metier_map, by=c("species_fk"="target_species_fk"))

sampling_diamond_reversal <- sampling_diamond_reversal %>%
  mutate(year_frame = paste0(year," ",frame," Q",quarter," SD",area))

sampling_result_grouped$year_frame_character <- as.character(sampling_result_grouped$year_frame)

actual_frames <- sampling_result_grouped %>% select(year_frame_character) %>% distinct()

sampling_diamond_reversal <- sampling_diamond_reversal %>%
  filter(!is.na(year_frame)) %>%
  filter(year_frame %in% actual_frames$year_frame_character)

unique_vessels_sampled_tally <- sampling_result_grouped %>% filter(status == "sampled", .preserve=TRUE)  %>% select(sample_source_fk) %>% distinct() %>% tally()

sampling_diamond_reversal$year_frame <- as.factor(sampling_diamond_reversal$year_frame)
sampling_diamond_reversal_tally <- sampling_diamond_reversal %>%
  group_by(year_frame) %>%
  tally()

not_available_tally <- sampling_result_grouped %>% filter(status == "out of area", .preserve=TRUE) %>%
  distinct(year_frame,sample_source_fk) %>% tally()

unique_vessels_contacted_tally <- sampling_result_grouped %>% filter(status %in% c("assigned","inactive","rejected","sampled","out of area","no_contact_details","no_answer","observer_declined"), .preserve=TRUE) %>% distinct(year_frame,sample_source_fk) %>% tally()

no_answer_tally <- sampling_result_grouped %>% filter(status %in% c("no_answer"), .preserve=TRUE) %>% distinct(year_frame,sample_source_fk) %>% tally()

rejected_tally <- sampling_result_grouped %>% filter(status %in% c("rejected"), .preserve=TRUE) %>% distinct(year_frame,sample_source_fk) %>% tally()

observer_declined_tally <- sampling_result_grouped %>% filter(status %in% c("observer_declined"), .preserve=TRUE) %>% distinct(year_frame,sample_source_fk) %>% tally()


table_b$NOT_AVAILABLE <- not_available_tally$n
table_b$VESSELS_FLEET <- sampling_diamond_reversal_tally$n
table_b$TOT_SELECTIONS <- tally_all$sum
table_b$REFUSAL_RATE <- tally_rejection$n
table_b <- table_b %>% mutate(REFUSAL_RATE = super_round(REFUSAL_RATE/TOT_SELECTIONS))
table_b$UNIQUE_VESSELS_SAMPLED <- unique_vessels_sampled_tally$n
table_b$COVERAGE_RATE <- super_round(table_b$UNIQUE_VESSELS_SAMPLED / table_b$VESSELS_FLEET)
table_b$NONRESPONSE_RATE <- super_round(tally_nonresponses$n / tally_all$sum)
table_b$UNIQUE_VESSELS_CONTACTED <- unique_vessels_contacted_tally$n
table_b$NO_ANSWER <- no_answer_tally$n
table_b$OBSERVER_DECLINED <- rejected_tally$n
table_b$INDUSTRY_DECLINED <- observer_declined_tally$n

#TODO: use trips
#table_b <- table_b %>% mutate(COVERAGE_RATE = super_round(UNIQUE_VESSELS_SAMPLED / VESSELS_FLEET))

# ... order columns 
table_b <- table_b[, c("COUNTRY","YEAR","SAMPLE_FRAME","REFUSAL_RATE","COVERAGE_RATE","NONRESPONSE_RATE","VESSELS_FLEET","TRIPS_FLEET","TRIPS_SAMPLED_ONBOARD","UNIQUE_VESSELS_SAMPLED","UNIQUE_VESSELS_CONTACTED","NOT_AVAILABLE","NO_CONTACT_DETAILS","NO_ANSWER","OBSERVER_DECLINED","INDUSTRY_DECLINED","TOT_SELECTIONS")]

openxlsx::write.xlsx(table_b, paste0(path_out,.Platform$file.sep,"FIN_TABLE_B_REFUSAL_RATE.xlsx"), sheetName = "TABLE_B", colNames = TRUE, rowNames = FALSE)


