
#--------------------------------------------------------------------------------------------
#       3. Import SALMON data to length classes and merge it with LANDING data      HUOM:  Prior 2024..               
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
#       3.2 Merge ANA1 (Oracle ana-samples 2013-2021) and ANA2 (MOngo 2022 onwards anadromous samples)                    
#--------------------------------------------------------------------------------------------

source("mongo.R")

ices_rectangle <- read.csv2("ices_rectangle.csv", sep=",", dec=".")

batch <- read.mongoCollectionToDataframe("batch")
individual <- read.mongoCollectionToDataframe("individual")
species <- read.mongoCollectionToDataframe("species")
species$laji <- as.character(species$laji)
batch <- batch %>% left_join(species, by = c("species_id" = "laji"))
project <- read.mongoCollectionToDataframe("project")

# filter only landed salmon and sea trout samples
individual$pitcm <-  as.integer(individual$pitcm)
# select landing salmons and sea trouts based on length
individual <- individual  %>% filter(pitcm >= 60) 


# Resolve trip names

batch$trip_id <- paste(batch$year, batch$lajikv1, batch$batch_number, sep="_")

#join batch to individual

individual2 <- individual %>% left_join(batch, by = c("sample_id" = "batch_id"))

#join project to individual

individual3 <- individual2 %>% left_join(project, by = c("project_id" = "project_id"))

#filter empty lenths
#filter only commercial samples
#form lengthclasses
individual4 <- individual3 %>%
  filter(!is.na(as.numeric(pitcm))) %>%
  filter(number==0) %>% #filter only commercial samples included 
  mutate(length = as.numeric(pitcm) * 10) %>%
  mutate(lengthclass = length - length %% 10) %>%
  arrange(lengthclass, length)

#renaming
individual4$PITUUS <- individual4$length
individual4$DB_TRIP_ID <- individual4$trip_id
individual4$YEAR <- individual4$year
individual4$PYYDYSKOODI <- individual4$pyydys

#Age to a single number from me_vu and po_vu variables
individual4$IKA <- get.age(individual4)

#weight in grams
individual4$PAINO_GRAMMOINA <- individual4$pakg * 1000

#calculate month
individual4$MONTH <- format(as.Date(individual4$pvm, format="%Y-%m-%d"),"%m")

#add year Quarters
Q1 <- c("01","02","03")
Q2 <- c("04","05","06")
Q3 <- c("07","08","09")
Q4 <- c("10","11","12")
individual4$QUARTER [individual4$MONTH %in% Q1]<-1
individual4$QUARTER [individual4$MONTH %in% Q3]<-3
individual4$QUARTER [individual4$MONTH %in% Q4]<-4
individual4$QUARTER [individual4$MONTH %in% Q2]<-2

individual4$MONTH <- as.numeric(individual4$MONTH)
individual4$PYYDYSKOODI <- as.numeric(individual4$PYYDYSKOODI)

#rename SD, add metier, rename species code
individual4$ICES_OA <- as.numeric(individual4$osa_al)
individual4$METIER <- "FYK_ANA_>0_0_0"
individual4$FAO <- individual4$lajikv1

# TO DO select correct variables from MONGO data as ana2

ana2 <- individual4
#cast Oracle DB_TRIP_ID to character
ana1$DB_TRIP_ID <- as.character(ana1$DB_TRIP_ID)
#rowbind ana1 and ana2 to ana

ana <- dplyr::bind_rows(ana1, ana2)

#--------------------------------------------------------------------------------------------
#       3.3 aggregate SALMON data to length classes and merge it with LANDING data                       
#--------------------------------------------------------------------------------------------







# after combining Oracle and Mongo ana samples make individually all the parts that form the key
# make the key variable to match table A key (domain_discards or domain_landings)
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
ana3 <- ana %>% select(YEAR, DB_TRIP_ID, PITUUS, PAINO_GRAMMOINA, IKA, domain_landings) %>% rename(vuosi = YEAR, nayteno = DB_TRIP_ID, pituus = PITUUS, paino = PAINO_GRAMMOINA, ika = IKA)

#all ana weights from g -> to kg
ana3$paino <- ana3$paino/1000

#all ana lenhgts from g -> to kg
ana3$pituus <- ana3$pituus/10

# remove missing age values
ana4 <- filter(ana3, !is.na(ika) & !is.na(paino) & !is.na(pituus))
ana_deleted_missing_feno_data <- filter(ana3, is.na(ika) |  is.na(paino) |  is.na(pituus))
