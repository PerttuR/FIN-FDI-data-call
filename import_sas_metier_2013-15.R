# import SAS metier tables for 2013 to 2015
# Authors: J. Demmler, Perttu
# Date: 10/06/2025
# Last revision: 16/06/2025

#.------------------------------------------------------------------------------
#                   PRE. libraries and functions                            ####              
#.------------------------------------------------------------------------------
rm(list=ls())

library(tidyverse) # tidy data science
library(haven)     # import SAS files
library(openxlsx)  # read from/to Excel
library(stringr)   # string operations
library(RCurl)     # get data from Github
library(flextable) # html table in Viewer window
library(sf)        # simple spatial features
library(labelled)  # remove sas imported column labels
library(janitor)
library(readxl)
library(lubridate)

source("db.r")

#.------------------------------------------------------------------------------
#                   0. set working directories to match folder paths        ####              
#.------------------------------------------------------------------------------
# Common paths data call folders:

run.year = 2025

# Output folder
path_der <- paste0(getwd(), .Platform$file.sep, "der/", run.year,"/")
# Orig folder
path_orig <- paste0(getwd(), .Platform$file.sep, "orig/")

#.------------------------------------------------------------------------------
#                   1. get data from G or C drive                           ####              
#.------------------------------------------------------------------------------

#for (i in 2013:2015){
#  
#  tmp <- read_sas(paste0("G:/Luke2/Stat_kala_merikalastus/Metier/Data/pvkarvo",i,"_metier.sas7bdat")) |>
#                  mutate(KALASTUSVUOSI = i)
#  
#  assign(paste0("metier_",i), tmp)
#  
#} 


## 1.1 metier data ####
 for (i in 2013:2015){
   
   tmp <- read_sas(paste0("orig/pvkarvo",i,"_metier.sas7bdat")) |>
     mutate(KALASTUSVUOSI = i)
   
   assign(paste0("metier_",i), tmp)
   
 } 

# change all column names to upper case
names(metier_2013) <- toupper(names(metier_2013))
names(metier_2014) <- toupper(names(metier_2014))
names(metier_2015) <- toupper(names(metier_2015))

# remove column labels
var_label(metier_2013) <- NULL
var_label(metier_2014) <- NULL
var_label(metier_2015) <- NULL

# match column names
metier_2013.2 <- metier_2013 |> 
                  add_column(ALUE="NA", MIEHISTO=NA, VESSEL_LENGTH="NA", KUNTA_KNRO="NA", 
                             SATAMA="NA", PURKU_KUNTA="NA", LOCODE=NA, PURKUMAA="NA", 
                             HANKERIAS=NA, SATAMAKUNTA="NA", RECTANGLE="NA", AMMATTI=NA) |>
                  mutate(KUNTA = as.character(KUNTA)) |>
                  select(ALUS, ALUSNIMI, KALASTUSKERTATUNNUS, PAIVAKIRJANRO, SIVUNRO, 
                         PARI, PARINIMI, LAHTOPVM, PALUUPVM, PURKUPVM, PYYDYS, 
                         KALVYOH, PYYDLKM, PVM, VETOAIKA, AIKAMIN, RUUTU, TE, TE_PAL, 
                         SILMAKOKO, HUOM, LABEL="_LABEL_", TURSKA, KAMPELA, 
                         SILAKKA, KILOHAIL, VALKOTURSKA, MUU_KALA, LOHI_KG, 
                         SIIKA, MUIKKU, AHVEN, KUHA, SARKI, KUORE, PKAMP, HAUKI, 
                         MADE, TAIMEN, LAHNA, SAYNE, LOHI_KPL, PUNAKAMPELA, ANKERIAS, 
                         KK, PAINO, PYYNTIPV, PARITRO, TYYPPI, OMISTAJA=NIMI, 
                         KUNTA, RAKENTAMISAIKA, PITUUS, ALUE, PYYDYS1, PYYDYS2, 
                         PYYDYS3, PYYDYS4, TEHO, VETOISUUS, ASTUNNUS=ASIAKASTUNNUS, 
                         ALUSKOODI, MIEHISTO, FT, GEAR, KIRJOLOH, KIISKI,
                         LEVEL4, ALL, DEMERSAL, FRESH, SMALLPELAGIC, ANADR, PV, 
                         LEVEL5, LEVEL6, GROUND, GROUND2, VESSEL_LENGTH, LENGTH,
                         METIER, KUNTA_KNRO, SATAMA, PURKU_KUNTA, LOCODE,
                         PURKUMAA, HSILAKKA, HKILOHAIL, HTURSKA, HKAMPELA, 
                         HPKAMP, HVALKOTURSKA, HPUNAKAMPELA, HMUU_KALA, HHAUKI,
                         HSIIKA, HLOHI_KG, HTAIMEN, HKIRJOLOH, HKUORE, HLAHNA,
                         HSAYNE, HSARKI, HMADE, HAHVEN, HKUHA, HANKERIAS, 
                         HMUIKKU, HYHT, SATAMAKUNTA, RECTANGLE, ICES, 
                         AMMATTI, KALASTUSVUOSI, ALRYH_ALUSRYHMATUNNUS)


metier_2014.2 <- metier_2014 |> 
                  add_column(TE_PAL="NA", LABEL="NA", LENGTH="NA", SATAMAKUNTA="NA",
                             RECTANGLE="NA", ALRYH_ALUSRYHMATUNNUS=NA) |>
                  select(ALUS, ALUSNIMI, KALASTUSKERTATUNNUS, PAIVAKIRJANRO, SIVUNRO, 
                         PARI, PARINIMI, LAHTOPVM, PALUUPVM, PURKUPVM, PYYDYS, 
                         KALVYOH, PYYDLKM, PVM, VETOAIKA, AIKAMIN, RUUTU, TE, TE_PAL, 
                         SILMAKOKO, HUOM, LABEL, TURSKA, KAMPELA,
                         SILAKKA, KILOHAIL, VALKOTURSKA, MUU_KALA, LOHI_KG, 
                         SIIKA, MUIKKU, AHVEN, KUHA, SARKI, KUORE, PKAMP, HAUKI, 
                         MADE, TAIMEN, LAHNA, SAYNE, LOHI_KPL, PUNAKAMPELA, ANKERIAS, 
                         KK, PAINO, PYYNTIPV, PARITRO, TYYPPI, OMISTAJA, 
                         KUNTA, RAKENTAMISAIKA, PITUUS, ALUE, PYYDYS1, PYYDYS2, 
                         PYYDYS3, PYYDYS4, TEHO, VETOISUUS, ASTUNNUS, 
                         ALUSKOODI, MIEHISTO, FT, GEAR, KIRJOLOH, KIISKI, 
                         LEVEL4, ALL, DEMERSAL, FRESH, SMALLPELAGIC, ANADR, PV, 
                         LEVEL5, LEVEL6, GROUND, GROUND2, VESSEL_LENGTH, LENGTH,
                         METIER, KUNTA_KNRO, SATAMA=SATAMA_NIMI, PURKU_KUNTA, LOCODE, 
                         PURKUMAA, HSILAKKA, HKILOHAIL, HTURSKA, HKAMPELA, 
                         HPKAMP, HVALKOTURSKA, HPUNAKAMPELA, HMUU_KALA, HHAUKI, 
                         HSIIKA, HLOHI_KG, HTAIMEN, HKIRJOLOH, HKUORE, HLAHNA, 
                         HSAYNE, HSARKI, HMADE, HAHVEN, HKUHA, HANKERIAS, 
                         HMUIKKU, HYHT, SATAMAKUNTA, RECTANGLE, ICES, 
                         AMMATTI, KALASTUSVUOSI, ALRYH_ALUSRYHMATUNNUS)



metier_2015.2 <- metier_2015 |> 
                  add_column(TE_PAL="NA", KUNTA_KNRO="NA", PURKU_KUNTA="NA", 
                             HPUNAKAMPELA=NA, ALRYH_ALUSRYHMATUNNUS=NA) |>
                  select(ALUS, ALUSNIMI, KALASTUSKERTATUNNUS, PAIVAKIRJANRO, SIVUNRO, 
                         PARI, PARINIMI, LAHTOPVM, PALUUPVM, PURKUPVM, PYYDYS, 
                         KALVYOH, PYYDLKM, PVM, VETOAIKA, AIKAMIN, RUUTU, TE, TE_PAL, 
                         SILMAKOKO, HUOM, LABEL="_LABEL_", TURSKA, KAMPELA, 
                         SILAKKA, KILOHAIL, VALKOTURSKA, MUU_KALA, LOHI_KG, 
                         SIIKA, MUIKKU, AHVEN, KUHA, SARKI, KUORE, PKAMP, HAUKI, 
                         MADE, TAIMEN, LAHNA, SAYNE, LOHI_KPL, PUNAKAMPELA, ANKERIAS,
                         KK, PAINO, PYYNTIPV, PARITRO, TYYPPI, OMISTAJA, 
                         KUNTA, RAKENTAMISAIKA, PITUUS, ALUE, PYYDYS1, PYYDYS2, 
                         PYYDYS3, PYYDYS4, TEHO, VETOISUUS, ASTUNNUS, 
                         ALUSKOODI, MIEHISTO, FT, GEAR, KIRJOLOH,  KIISKI, 
                         LEVEL4, ALL, DEMERSAL, FRESH, SMALLPELAGIC, ANADR, PV, 
                         LEVEL5, LEVEL6, GROUND, GROUND2, VESSEL_LENGTH, LENGTH,
                         METIER, KUNTA_KNRO, SATAMA, PURKU_KUNTA, LOCODE, 
                         PURKUMAA, HSILAKKA, HKILOHAIL, HTURSKA, HKAMPELA, 
                         HPKAMP, HVALKOTURSKA, HPUNAKAMPELA, HMUU_KALA, HHAUKI, 
                         HSIIKA, HLOHI_KG, HTAIMEN, HKIRJOLOH, HKUORE, HLAHNA, 
                         HSAYNE, HSARKI, HMADE, HAHVEN, HKUHA, HANKERIAS, 
                         HMUIKKU, HYHT, SATAMAKUNTA, RECTANGLE, ICES, 
                         AMMATTI, KALASTUSVUOSI, ALRYH_ALUSRYHMATUNNUS)

# combine into 1 table
metier_2013_15 <- bind_rows(metier_2013.2, metier_2014.2, metier_2015.2)

# filter if wanted
# metier_2013_15_FIN <- metier_2013_15 |> filter(grepl("FIN",alus))

# remove temporary files
rm(metier_2013, metier_2014, metier_2015, metier_2013.2, metier_2014.2, metier_2015.2, tmp)
invisible(gc())

#save to der folder
saveRDS(metier_2013_15, file = paste0(path_der,"sas_logbook_2013_15_raw.rds"))

# dim(metier_2013_15)

# check
# tmp <- data.frame(metier=names(metier_2013_15)) |> rowid_to_column()
# tmp <- tmp[order(tmp$metier),]
# write.table(tmp, "clipboard", row.names = FALSE, sep=";")

## 1.2 shore data ####
for (i in 2013:2015){
  
  tmp <- read_sas(paste0("orig/rkarvo",i,"_metier.sas7bdat")) |>
    mutate(KALASTUSVUOSI = i)
  
  assign(paste0("shore_",i), tmp)
  
} 

# change all column names to upper case
names(shore_2013) <- toupper(names(shore_2013))
names(shore_2014) <- toupper(names(shore_2014))
names(shore_2015) <- toupper(names(shore_2015))

# remove column labels
var_label(shore_2013) <- NULL
var_label(shore_2014) <- NULL
var_label(shore_2015) <- NULL

# match column names
shore_2013.2 <- shore_2013 |>
                  add_column(AMMATTI=NA, HANKERIAS=NA, LIOTUSAIKA=NA, LOCODE="NA", 
                             LOHIALUS="NA", PLE=NA, PURKU_KUNTA="NA", PURKUMAA="NA", 
                             PYYNTIAIKA=NA, SATAMA="NA", VESSEL_LENGTH="NA", RECTANGLE="NA", 
                             SATAMAKUNTA="NA", SATAMATIETO="NA") |>
                  mutate(PURKUPVM= as.character(PURKUPVM)) |>
                  select(LABEL="_LABEL_", AHVEN, ALL, ALUS, AMMATTI, ANADR, ANKERIAS, 
                         ASIAKASTUNNUS, DEMERSAL, FRESH, FT, GEAR, GROUND, GROUND2, 
                         HAHVEN, HANKERIAS, HAUKI, HHAUKI, HKAMPELA, HKILOHAIL, 
                         HKIRJOLOH, HKUHA, HKUORE, HLAHNA, HLOHI_KG, HMADE, HMUIKKU, 
                         HMUU_KALA, HPKAMP, HPUNAKAMPELA, HSARKI, HSAYNE, HSIIKA, 
                         HSILAKKA, HTAIMEN, HTURSKA, HUOM, HVALKOTURSKA, HYHT, ICES, 
                         KALASTUSTA, KALASTUSVUOSI, KAMPELA, KIISKI, KILOHAIL, 
                         KIRJOLOH, KK, KNO, KUHA, KUORE, LAHNA, LENGTH, LEVEL4, 
                         LEVEL5, LEVEL6, LIOTUSAIKA, LOCODE, LOHI_KG, LOHI_KPL, 
                         LOHIALUS, LONRO, MADE, METIER, MUIKKU, MUU_KALA, NIMI, 
                         PAINO, PITUUS, PKAMP, PLE, PUNAKAMPELA, PURKU_KUNTA, 
                         PURKUMAA, PURKUPV, PURKUPVM, PYYDLKM, PYYDYS, PYYNTIAIKA, 
                         PYYNTIPV, RAKENTAMISAIKA, RANNIKKOPYYNTITUNNUS, RECTANGLE, 
                         RUUTU, SARKI, SATAMA, SATAMAKUNTA, SATAMATIETO, SAYNE, 
                         SELITE, SIIKA, SILAKKA, SMALLPELAGIC, TAIMEN, TE, TEHO, 
                         TULOLUOKKA, TURSKA, VALKOTURSKA, VESSEL_LENGTH, VETOISUUS, VUOSI)

shore_2014.2 <- shore_2014 |>
                  add_column(HUOM="NA", PURKUPVM="NA", LIOTUSAIKA=NA, LOHIALUS="NA",
                             RECTANGLE="NA", SATAMAKUNTA="NA", SATAMATIETO="NA") |>
                  #mutate(PURKUPVM= as.POSIXct(PURKUPVM)) |>
                  select(LABEL="_LABEL_", AHVEN, ALL, ALUS, AMMATTI, ANADR, ANKERIAS, 
                         ASIAKASTUNNUS, DEMERSAL, FRESH, FT, GEAR, GROUND, GROUND2, 
                         HAHVEN, HANKERIAS, HAUKI, HHAUKI, HKAMPELA, HKILOHAIL, 
                         HKIRJOLOH, HKUHA, HKUORE, HLAHNA, HLOHI_KG, HMADE, HMUIKKU, 
                         HMUU_KALA, HPKAMP, HPUNAKAMPELA, HSARKI, HSAYNE, HSIIKA, 
                         HSILAKKA, HTAIMEN, HTURSKA, HUOM, HVALKOTURSKA, HYHT, ICES, 
                         KALASTUSTA, KALASTUSVUOSI, KAMPELA, KIISKI, KILOHAIL, 
                         KIRJOLOH, KK, KNO, KUHA, KUORE, LAHNA, LENGTH, LEVEL4, 
                         LEVEL5, LEVEL6, LIOTUSAIKA, LOCODE, LOHI_KG, LOHI_KPL, 
                         LOHIALUS, LONRO, MADE, METIER, MUIKKU, MUU_KALA, NIMI, 
                         PAINO, PITUUS, PKAMP, PLE, PUNAKAMPELA, PURKU_KUNTA, 
                         PURKUMAA, PURKUPV, PURKUPVM, PYYDLKM, PYYDYS, PYYNTIAIKA, 
                         PYYNTIPV, RAKENTAMISAIKA, RANNIKKOPYYNTITUNNUS, RECTANGLE, 
                         RUUTU, SARKI, SATAMA, SATAMAKUNTA, SATAMATIETO, SAYNE, 
                         SELITE, SIIKA, SILAKKA, SMALLPELAGIC, TAIMEN, TE, TEHO, 
                         TULOLUOKKA, TURSKA, VALKOTURSKA, VESSEL_LENGTH, VETOISUUS, VUOSI)

shore_2015.2 <- shore_2015 |>
                  add_column(HPUNAKAMPELA=NA, PAINO=NA, PLE=NA, PUNAKAMPELA=NA, 
                             PURKU_KUNTA="NA",  PURKUPVM="NA", PYYNTIAIKA=NA) |>
                  select(LABEL="_LABEL_", AHVEN, ALL, ALUS, AMMATTI, ANADR, ANKERIAS, 
                         ASIAKASTUNNUS, DEMERSAL, FRESH, FT, GEAR, GROUND, GROUND2, 
                         HAHVEN, HANKERIAS, HAUKI, HHAUKI, HKAMPELA, HKILOHAIL, 
                         HKIRJOLOH, HKUHA, HKUORE, HLAHNA, HLOHI_KG, HMADE, HMUIKKU, 
                         HMUU_KALA, HPKAMP, HPUNAKAMPELA, HSARKI, HSAYNE, HSIIKA, 
                         HSILAKKA, HTAIMEN, HTURSKA, HUOM, HVALKOTURSKA, HYHT, ICES, 
                         KALASTUSTA, KALASTUSVUOSI, KAMPELA, KIISKI, KILOHAIL, 
                         KIRJOLOH, KK, KNO, KUHA, KUORE, LAHNA, LENGTH, LEVEL4, 
                         LEVEL5, LEVEL6, LIOTUSAIKA, LOCODE, LOHI_KG, LOHI_KPL, 
                         LOHIALUS, LONRO, MADE, METIER, MUIKKU, MUU_KALA, NIMI, 
                         PAINO, PITUUS, PKAMP, PLE, PUNAKAMPELA, PURKU_KUNTA, 
                         PURKUMAA, PURKUPV,  PURKUPVM, PYYDLKM, PYYDYS, PYYNTIAIKA, 
                         PYYNTIPV, RAKENTAMISAIKA, RANNIKKOPYYNTITUNNUS, RECTANGLE, 
                         RUUTU, SARKI, SATAMA, SATAMAKUNTA, SATAMATIETO, SAYNE, 
                         SELITE, SIIKA, SILAKKA, SMALLPELAGIC, TAIMEN, TE, TEHO, 
                         TULOLUOKKA, TURSKA, VALKOTURSKA, VESSEL_LENGTH, VETOISUUS, VUOSI)


# combine into 1 table
shore_2013_15 <- bind_rows(shore_2013.2, shore_2014.2, shore_2015.2)

# remove temporary files
rm(shore_2013, shore_2014, shore_2015, shore_2013.2, shore_2014.2, shore_2015.2, tmp)
invisible(gc())

#save to der folder
saveRDS(shore_2013_15, file = paste0(path_der,"shore_2013_15_raw.rds"))

### find newest db schema ####
# ... time stamp to latest Logbook DB source: YYYY-MM-DD
table.list <- list.dbTable("kake_siirto")[,1] |> as.character(table)
table.list <- substr(table.list, 30, nchar(table.list)-3)
table.dates <- sort(unique(substr(table.list,1,10)))
# only keep dates
table.dates <- grep("\\d{4}-\\d{2}-\\d{2}", table.dates, value=TRUE)

# output choice here
schemadate <- max(table.dates)
message("Newest schema is from: ", schemadate)

#NEED to write to DB only once. uncomment if needed
#
## Write combined metier datafiles 2013-2015 output data to Luke LOGBOOK database
#
dcprodschema <- paste0(schemadate, "-dcprod")

# SAVEPOINT -------- ####
# uncomment and save to DB if renewed
# invisible(write.dbTable(dbname = 'kake_siirto', dcprodschema, "sas_logbook_raw_2013_15", metier_2013_15, overwrite = TRUE))
# invisible(write.dbTable(dbname = 'kake_siirto', dcprodschema, "sas_shorelogs_raw_2013_15", shore_2013_15, overwrite = FALSE))

# dim(shore_2013_15)



## Write species lookup table to Luke LOGBOOK database
#dcprodschema <- paste0(schemadate, "-dcprod")
#invisible(write.dbTable(dbname = "kake_siirto", dcprodschema, "species_lookup", species_lookup, overwrite = TRUE))


#.------------------------------------------------------------------------------
#                   2. add vars to 2013.-2015 metier table                  ####              
#.------------------------------------------------------------------------------

# metier_2013_15 <- readRDS(paste0(path_der,"metier_2013_15.rds"))

logbook_13_15 <- metier_2013_15 |> 
                  mutate(
                    COUNTRY = "FIN",
                    YEAR	= KALASTUSVUOSI,
                    QUARTER	= case_when(
                      KK %in% seq(1,3) ~ "1",
                      KK %in% seq(4,6) ~ "2",
                      KK %in% seq(7,9) ~ "3",
                      KK %in% seq(10,12) ~ "4"),
                    VESSEL_LENGTH,
                    FISHING_TECH =	FT,
                    GEAR_TYPE	= stringr::str_sub(METIER, 1,3), 
                    TARGET_ASSEMBLAGE	= stringr::str_sub(METIER, 5,7), 
                    # MESH_SIZE_RANGE	HERE!
                    # METIER HERE!       
                    METIER_7 = "NA",
                    # DOMAIN_DISCARDS HERE!
                    # DOMAIN_LANDINGS	HERE!
                    SUPRA_REGION	= "NAO",
                    # SUB_REGION HERE!
                    EEZ_INDICATOR	= "NA",
                    GEO_INDICATOR	= "NGI",
                    NEP_SUB_REGION	= "NA",
                    SPECON_TECH	= "NA",
                    DEEP	= "NA",
                    # SPECIES	= "",              # FISHNAME COL 20-41
                    # TOTWGHTLANDG	= "",        # FISHNAME COL 20-41
                    # TOTVALLANDG	= "",          # H+FISHNAME COL 20-41
                    DISCARDS	= "NA",
                    #CONFIDENTIAL HERE!
                    )

rm(metier_2013_15); invisible(gc())

shorelogs_13_15 <- shore_2013_15 |> 
                  mutate(
                    COUNTRY = "FIN",
                    YEAR	= KALASTUSVUOSI,
                    QUARTER	= case_when(
                      KK %in% seq(1,3) ~ "1",
                      KK %in% seq(4,6) ~ "2",
                      KK %in% seq(7,9) ~ "3",
                      KK %in% seq(10,12) ~ "4"),
                    VESSEL_LENGTH,
                    FISHING_TECH =	FT,
                    GEAR_TYPE	= stringr::str_sub(METIER, 1,3), 
                    TARGET_ASSEMBLAGE	= stringr::str_sub(METIER, 5,7), 
                    # MESH_SIZE_RANGE	HERE!
                    # METIER HERE!       
                    METIER_7 = "NA",
                    # DOMAIN_DISCARDS HERE!
                    # DOMAIN_LANDINGS	HERE!
                    SUPRA_REGION	= "NAO",
                    # SUB_REGION HERE!
                    EEZ_INDICATOR	= "NA",
                    GEO_INDICATOR	= "NGI",
                    NEP_SUB_REGION	= "NA",
                    SPECON_TECH	= "NA",
                    DEEP	= "NA",
                    # SPECIES	= "",              # FISHNAME COL 20-41
                    # TOTWGHTLANDG	= "",        # FISHNAME COL 20-41
                    # TOTVALLANDG	= "",          # H+FISHNAME COL 20-41
                    DISCARDS	= "NA",
                    #CONFIDENTIAL HERE!
                  )

rm(shore_2013_15); invisible(gc())

# check new columns
# View(metier_2013_15[,130:152])

# save to der folder
# saveRDS(metier_2013_15, file = paste0(path_der,"metier_2013_15.rds"))

#.------------------------------------------------------------------------------
#                   3. correct vessel lengths                               ####              
#.------------------------------------------------------------------------------

# we are cross-validation the capacity length categories - if they are different use the
# ones in kapasiteetti table instead

# metier_2013_15 <- readRDS(paste0(path_der,"metier_2013_15.rds"))

# vessel GOLDEN ROSE CHANGED OWNER 2014, BUT WRONG IN KAPASITEETI (only has one registration number)

# find the correct table name and date
tbl.list <- list.dbTable.tbl(dbname = "kake_siirto", schema=paste0(schemadate, "-dcprod"))
# find second table
tag <- grep("kapasiteetti", tbl.list$table)
tablename <- tbl.list$table[tag]
class(tablename) <- "character"
tablename <- unlist(strsplit(tablename, '"'))
tablename <- tablename[length(tablename)-1]

message(paste("Reading table:", tablename))

# !!!HERE!!! ####
# in kapasittetti there are 
# duplicate names for the same VUOSI, ULKOINENTUNNUS
# other duplicates by VUOSI, ULKOINENTUNNUS, NIMI, VLENGTH_FDI

kapasiteetti <- read.dbTable(schema=paste(schemadate, "-dcprod", sep = ""), 
                             table=tablename, dbname = "kake_siirto") |> 
                  select(VUOSI, ULKOINENTUNNUS, NIMI, VLENGTH_FDI) |> 
                  mutate(NIMI = toupper(NIMI)) |> distinct()

### 3.1 clean logbook ####
ship.length <- logbook_13_15 |> select(KALASTUSVUOSI, ALUS, ALUSNIMI, VESSEL_LENGTH) |> 
  mutate(ALUSNIMI = toupper(ALUSNIMI)) |> distinct() |>
  left_join(kapasiteetti, 
            by= c("KALASTUSVUOSI"="VUOSI","ALUS"="ULKOINENTUNNUS")) |>
  mutate(DIFF = if_else(VESSEL_LENGTH != VLENGTH_FDI, 1, 0),
         VESSEL_LENGTH = case_when(
           is.na(VESSEL_LENGTH) ~ VLENGTH_FDI,
           VESSEL_LENGTH != VLENGTH_FDI ~ VLENGTH_FDI,
           .default = VESSEL_LENGTH
         )) |> distinct()

# check 2014 GOLDEN ROSE - should now be NA, but vessel_length is correct

# link ship length lookup to table
# remove logbook vessel length and use cross-validated one instead
logbook_13_15 <- logbook_13_15 |> rename(length_orig=VESSEL_LENGTH) |> 
                    left_join(ship.length |> select(KALASTUSVUOSI, ALUS, VESSEL_LENGTH), 
                              by=c("KALASTUSVUOSI"="KALASTUSVUOSI",
                                   "ALUS"="ALUS")) |>
                    relocate(VESSEL_LENGTH, .after = QUARTER)

# checking records
logbook_13_15 |> count(length_orig) |> flextable() |> 
  set_caption("Check length_orig classes in logbook_13_15")

logbook_13_15 |> count(VESSEL_LENGTH) |> flextable() |> 
  set_caption("Check VESSEL_LENGTH classes in logbook_13_15")

### 3.2 clean shorelogs ####
# missing vessel identifiers
# only owners can be identified, but there's no way to link to capacity table
# shorelogs_13_15 |> filter(ALUS == "") |> select(NIMI) |> distinct() |> pull(NIMI) -> tmp

ship.length <- shorelogs_13_15 |> 
  select(KALASTUSVUOSI, ALUS, VESSEL_LENGTH) |> distinct() |>
  left_join(kapasiteetti, 
            by= c("KALASTUSVUOSI"="VUOSI","ALUS"="ULKOINENTUNNUS")) |>
  mutate(DIFF = if_else(VESSEL_LENGTH != VLENGTH_FDI, 1, 0),
         VESSEL_LENGTH = case_when(
           is.na(VESSEL_LENGTH) ~ VLENGTH_FDI,
           VESSEL_LENGTH != VLENGTH_FDI ~ VLENGTH_FDI,
           .default = VESSEL_LENGTH
         ))

# link ship length lookup to table
# remove logbook vessel length and use cross-validated one instead

shorelogs_13_15 <- shorelogs_13_15 |> rename(length_orig=VESSEL_LENGTH, OMISTAJA=NIMI) |> distinct() |>
  left_join(ship.length |> select(KALASTUSVUOSI, ALUS, VESSEL_LENGTH, ALUSNIMI=NIMI), 
            by=c("KALASTUSVUOSI"="KALASTUSVUOSI",
                 "ALUS"="ALUS")) |>
  relocate(VESSEL_LENGTH, .after = QUARTER) |>
  mutate(VESSEL_LENGTH = if_else(VESSEL_LENGTH == "NA", "NK", VESSEL_LENGTH))

# checking records
shorelogs_13_15 |> count(length_orig) |> flextable() |> 
  set_caption("Check length_orig classes in shorelogs_13_15")

shorelogs_13_15 |> count(VESSEL_LENGTH) |> flextable() |> 
  set_caption("Check VESSEL_LENGTH classes in shorelogs_13_15")

#.------------------------------------------------------------------------------
#                   4. mesh size lookup                                     ####    
#.------------------------------------------------------------------------------

active.passive <- data.frame(TYPE = c(rep("passive",5), rep("active", 4)),
                             METIER4 = c("FPO", "FYK", "GNS", "LLD", "LLS", "MIS", "OTB", "OTM", "PTM"),
                             MESH    = c("YES", "YES", "YES", "NO", "NO", "PERHAPS", "YES", "YES", "YES"))

active.passive |> flextable() |> autofit() |> set_caption("METIER4 lookup for active and passive gears")

mesh.sizes <- data.frame(TYPE = c(rep("passive",6), rep("active", 6)),
                         GEARS = c("Diamond mesh <16 mm",
                                   "Diamond mesh >=16 mm and <32 mm",
                                   "Diamond mesh >=32 mm and <90 mm",
                                   "Diamond mesh >=90 mm and <110 mm",
                                   "Diamond mesh >=110 mm and <157 mm",
                                   "Diamond mesh >=157 mm",
                                   "Diamond mesh <16 mm",
                                   "Diamond mesh >=16 mm and <32 mm",
                                   "Diamond mesh >=32 mm and <90 mm",
                                   "Diamond mesh >=90 mm and <105 mm",
                                   "Diamond mesh >=105 mm and <110 mm",
                                   "Diamond mesh >=110 mm"),
                         FROM  = c( 0,16,32,90,110,157, 0,16,32,90,105,110),
                         TO   = c(15,31,89,109,156,Inf,15,31,89,104,109,Inf),
                         CODE = c("00D16", "16D32", "32D90", "90D110", "110D157", "157DXX",
                                  "00D16", "16D32", "32D90", "90D105", "105D110", "110DXX")) |>
                    mutate(TO = as.numeric(TO),
                           FROM = as.numeric(FROM))

mesh.sizes |> flextable() |> autofit() |> hline(i=6) |> set_caption("Mesh size ranges for Baltic")

# baseline
mesh.lookup1 <- active.passive |> filter(MESH != "NO") |> left_join(mesh.sizes, by=c("TYPE"="TYPE"))

# lines = NA
mesh.lookup2 <- active.passive |> filter(MESH == "NO") |> 
                  mutate(GEARS = "NA", FROM=NA, TO=NA, CODE ="NA") |>
                  relocate(GEARS, .after=METIER4)

# missing = NK
mesh.lookup3 <- mesh.lookup1 |> select(TYPE, METIER4, MESH) |> distinct() |>
                  mutate(GEARS = "NA", FROM=NA, TO=NA, CODE ="NK") |>
                  relocate(GEARS, .after=METIER4)
# glue together
mesh.lookup <- bind_rows(mesh.lookup1, mesh.lookup2, mesh.lookup3) |> arrange(TYPE, METIER4,FROM)

mesh.lookup |> flextable() |> autofit() |> set_caption("all possible mesh size combinations")

rm(active.passive, mesh.sizes, mesh.lookup1, mesh.lookup2, mesh.lookup3)
invisible(gc())

# SAVEPOINT -------- ####
# save to DB if needed
#invisible(write.dbTable(dbname = 'kake_siirto',dcprodschema, "mesh_sizes_DC2024", mesh.lookup, overwrite = FALSE))
# added to DB by Perttu 19-06-2025

### 4.1 logbook mesh sizes ####

logbook_13_15 <- logbook_13_15 |> mutate(METIER6 = METIER,
                                         METIER5 = stringr::str_sub(METIER, 1,7),
                                         METIER4 = stringr::str_sub(METIER, 1,3)) |>
                    rename(metier_orig=METIER)

# checking for LINE metiers with mesh sizes
# logbook_13_15 |> filter(METIER4 %in% c("LLD","LLS") & !is.na(SILMAKOKO)) |> 
#   select(YEAR, ALUS, ALUSNIMI, SILMAKOKO, METIER6) |> 
#   flextable() |> autofit() |> set_caption("Enties for lines but giving mesh size")

# correct data - remove mesh size if line metier
logbook_13_15 <- logbook_13_15 |> mutate(SILMAKOKO = case_when(
  METIER4 %in% c("LLD","LLS") ~ NA,
  .default = as.numeric(SILMAKOKO)
))

# logbook_13_15 |> count(SILMAKOKO)
# check missing mesh sizes in SAS data
#logbook_13_15 |> filter(!METIER4 %in% c("LLD","LLS")) |>
#  select(YEAR, PVM, ALUS, ALUSNIMI, SILMAKOKO, METIER6, METIER5, METIER4) |> 
#  left_join(mesh.lookup, join_by(METIER4, between(SILMAKOKO,TO,FROM))) |> 
#  filter(is.na(CODE)) |>
#  distinct() |> select(-c(METIER5,METIER4,TYPE,MESH,GEARS,TO,FROM)) |> 
#  arrange(ALUS,YEAR,PVM, METIER6) |>
#  flextable() |> autofit() |> set_caption("ships with missing net sizes")

# join mesh size range codes

logbook_13_15 <- logbook_13_15 |> 
  left_join(mesh.lookup |> select(METIER4, FROM, TO, CODE),
            join_by(METIER4, between(SILMAKOKO,FROM,TO)))

# fix wrong METIER4 code
logbook_13_15 <- logbook_13_15 |> 
                    mutate(METIER5 = if_else(METIER5 == "MISSING", "MIS_MIS", METIER5))

logbook_13_15  <- logbook_13_15 |> rename(metier6_orig=METIER6) |>
                    mutate(METIER6 = case_when(
                      FROM == 0 ~ paste0(METIER5, "_<", TO+1, "_0_0"),
                      TO == Inf ~ paste0(METIER5, "_>=", FROM, "_0_0"),
                      FROM > 0 & TO != Inf ~ paste0(METIER5, "_", FROM, "-", TO+1, "_0_0"),
                      CODE == "NK" | CODE == "NA" ~ paste0(METIER5,"_0_0_0")))

logbook_13_15 |> count(metier6_orig) |> flextable() |> set_caption("before cleaning")                    
logbook_13_15 |> count(METIER6) |> flextable() |> autofit() |> set_caption("after cleaning") 

### 4.2 shorelogs mesh sizes ####

### !!!HERE!!! ####
# need to find or derive mesh sizes before we can continue here
shorelogs_13_15 <- shorelogs_13_15 |> mutate(METIER6 = METIER,
                                         METIER5 = stringr::str_sub(METIER, 1,7),
                                         METIER4 = stringr::str_sub(METIER, 1,3)) |>
  rename(metier_orig=METIER)

# quick fix, no mesh sizes
shorelogs_13_15 <- shorelogs_13_15 |> mutate(SILMAKOKO = NA, FROM=NA, TO=NA, CODE="NK")

# fix wrong METIER4 code
shorelogs_13_15 <- shorelogs_13_15 |> 
  mutate(METIER5 = if_else(METIER5 == "MISSING", "MIS_MIS", METIER5))

shorelogs_13_15  <- shorelogs_13_15 |> rename(metier6_orig=METIER6) |>
  mutate(METIER6 = case_when(
    FROM == 0 ~ paste0(METIER5, "_<", TO+1, "_0_0"),
    TO == Inf ~ paste0(METIER5, "_>=", FROM, "_0_0"),
    FROM > 0 & TO != Inf ~ paste0(METIER5, "_", FROM, "-", TO+1, "_0_0"),
    CODE == "NK" | CODE == "NA" ~ paste0(METIER5,"_0_0_0")))

shorelogs_13_15 |> count(metier6_orig) |> flextable() |> set_caption("before cleaning")                    
shorelogs_13_15 |> count(METIER6) |> flextable() |> autofit() |> set_caption("after cleaning") 

# !!!HERE!!! ####
# create reasonble lookup for meshsizes based on metier and species and impute meshsize

rm(mesh.sizes, mesh.lookup, active.passive, metier_check, metier.lookup, metier.lookup.fin)
invisible(gc())

#.------------------------------------------------------------------------------
#                   5. ADD ICES AREAS                                       ####    
#.------------------------------------------------------------------------------

# metier_2013_15 <- readRDS(paste0(path_der,"metier_2013_15.rds"))

areas <- seq(23,32)

ewkb_to_sf <- function(data) {
  return(st_as_sfc(structure(data, class="WKB"), EWKB=T))
}

icesRectangle <- read.dbTable(schema='rek', table='ices_rectangle', dbname = "rktl")
icesRectangle <- icesRectangle[icesRectangle$statistical_area_name %in% areas,]
icesRectangle$geometry <- ewkb_to_sf(icesRectangle$geometry)

icesRectangle <- st_as_sf(icesRectangle) |> st_transform(epsg = 4326)

# test statistical areas
# plot(icesRectangle["statistical_area_name"])

# fix ices full area codes for region 28
icesRectangle <- icesRectangle |> mutate(area_code_full = case_when(
  ices_name %in% c("45H2","45H3","45H4","44H2","44H3","44H4","43H2","43H3","43H4","42H3") ~ "27.3.d.28.1",
  rdb_area_code == "27.3.d.28" & !ices_name %in% c("45H2","45H3","45H4","44H2","44H3","44H4","43H2","43H3","43H4","42H3") ~ "27.3.d.28.2",
  .default = as.character(rdb_area_code)
))

icesRectangle$rktl_name <- as.numeric(icesRectangle$rktl_name)

# test full area
# plot(icesRectangle["area_code_full"])

# join to sas data
logbook_13_15 <- logbook_13_15 |> 
  left_join(icesRectangle |> 
              select(rktl_name, ices_name, area_code_full) |> st_drop_geometry(), 
            by=c("RUUTU"="rktl_name"))

shorelogs_13_15 <- shorelogs_13_15 |> 
  left_join(icesRectangle |> 
              select(rktl_name, ices_name, area_code_full) |> st_drop_geometry(), 
            by=c("RUUTU"="rktl_name"))

# testing
logbook_13_15 |> count(area_code_full,ices_name) |> flextable()
shorelogs_13_15 |> count(area_code_full,ices_name) |> flextable()

# save to der folder
# saveRDS(metier_2013_15, file = paste0(path_der,"metier_2013_15.rds"))

rm(icesRectangle,areas,ewkb_to_sf)
invisible(gc())

#.------------------------------------------------------------------------------
#                   6. longitude / latitude                                 ####    
#.------------------------------------------------------------------------------

# metier_2013_15 <- readRDS(paste0(path_der,"metier_2013_15.rds"))

source("spatial.R")
# sas data
## logbooks
midpoints <- latlon(logbook_13_15$ices_name, midpoint=TRUE)

logbook_13_15 <- tibble::rowid_to_column(logbook_13_15, "ID")
midpoints <- tibble::rowid_to_column(midpoints, "ID")

logbook_13_15 <- left_join(logbook_13_15, midpoints, copy = TRUE)

logbook_13_15 <- logbook_13_15 |> rename(LATITUDE = SI_LATI, LONGITUDE = SI_LONG) |> 
  select(-ID)

## shorelogs
midpoints <- latlon(shorelogs_13_15$ices_name, midpoint=TRUE)

shorelogs_13_15 <- tibble::rowid_to_column(shorelogs_13_15, "ID")
midpoints <- tibble::rowid_to_column(midpoints, "ID")

shorelogs_13_15 <- left_join(shorelogs_13_15, midpoints, copy = TRUE)

shorelogs_13_15 <- shorelogs_13_15 |> rename(LATITUDE = SI_LATI, LONGITUDE = SI_LONG) |> 
  select(-ID)

# save to der folder
# saveRDS(metier_2013_15, file = paste0(path_der,"metier_2013_15.rds"))

rm(midpoints)
invisible(gc())

#.------------------------------------------------------------------------------
#                   7. fish column names                                   ####    
#.------------------------------------------------------------------------------

#import species lookup table ASFIS_sp_2024.xlsx

# species_lookup <- read.xlsx("orig/ASFIS_sp_2024.xlsx", sheet = "ASFIS_sp")
# head(species_lookup)
# 
# # save it as .rds
# saveRDS(species_lookup, file = paste0(path_der,"species_lookup.rds"))

# https://www.fao.org/fishery/en/collection/asfis
# lookup table for Finnish fish names

# grab table from database
# data is not in the newest schema!

fish.lookup <- read.dbTable(schema=paste("2025-04-10", "-dcprod", sep = ""), 
                             table="species_lookup", dbname = "kake_siirto")

fish.lookup <- fish.lookup |> filter(!is.na(Finnish_name)) |> 
  arrange(Alpha3_Code) |> select(Alpha3_Code, Scientific_Name, English_name, Finnish_name, SAS_name)

# create column names

fish.lookup <- fish.lookup |> mutate(KG_LABEL = paste0("SVT_KG_",Alpha3_Code),
                      VALUE_LABEL = paste0("SVT_VALUE_",Alpha3_Code),
                      IN_DCPROD = case_when(
                        Alpha3_Code == "HER" ~ "Y",
                        Alpha3_Code == "SPR" ~ "Y",
                        Alpha3_Code == "COD" ~ "Y",
                        Alpha3_Code == "FLE" ~ "Y",
                        Alpha3_Code == "TUR" ~ "Y",
                        Alpha3_Code == "PLN" ~ "Y",
                        Alpha3_Code == "SAL" ~ "Y",
                        Alpha3_Code == "TRS" ~ "Y",
                        Alpha3_Code == "SME" ~ "Y",
                        Alpha3_Code == "FBM" ~ "Y",
                        Alpha3_Code == "FID" ~ "Y",
                        Alpha3_Code == "FRO" ~ "Y",
                        Alpha3_Code == "FPI" ~ "Y",
                        Alpha3_Code == "FPE" ~ "Y",
                        Alpha3_Code == "FPP" ~ "Y",
                        Alpha3_Code == "FBU" ~ "Y",
                        Alpha3_Code == "TRR" ~ "Y",
                        Alpha3_Code == "FVE" ~ "Y",
                        Alpha3_Code == "ELE" ~ "Y",
                        Alpha3_Code == "FIN" ~ "Y",
                        .default = as.character("NO")
                      )) 

# metier_2013_15 |> filter(Punakampela != 0) |> View()
# metier_2013_15 |> filter(Valkoturska != 0) |> View()

# rename fish columns (add as duplicate)
# logbooks
logbook_13_15 <- logbook_13_15 |> mutate(
                        SVT_KG_COD = if_else(is.na(TURSKA),0,TURSKA),
                        SVT_KG_ELE = if_else(is.na(ANKERIAS),0,ANKERIAS),   
                        SVT_KG_FBM = if_else(is.na(LAHNA),0,LAHNA),
                        SVT_KG_FBU = if_else(is.na(MADE),0,MADE),
                        SVT_KG_FID = if_else(is.na(SAYNE),0,SAYNE),
                        SVT_KG_FIN = if_else(is.na(MUU_KALA),0,MUU_KALA),
                        SVT_KG_FLE = if_else(is.na(KAMPELA),0,KAMPELA),
                        SVT_KG_FPE = if_else(is.na(AHVEN),0,AHVEN),
                        SVT_KG_FPI = if_else(is.na(HAUKI),0,HAUKI),
                        SVT_KG_FPP = if_else(is.na(KUHA),0,KUHA),
                        SVT_KG_FRO = if_else(is.na(SARKI),0,SARKI),
                        SVT_KG_FVE = if_else(is.na(MUIKKU),0,MUIKKU),
                        SVT_KG_HER = if_else(is.na(SILAKKA),0,SILAKKA),
                        SVT_KG_PLE = if_else(is.na(PUNAKAMPELA),0,PUNAKAMPELA),
                        SVT_KG_PLN = if_else(is.na(SIIKA),0,SIIKA),
                        SVT_KG_SAL = if_else(is.na(LOHI_KG),0,LOHI_KG),
                        SVT_KG_SME = if_else(is.na(KUORE),0,KUORE),
                        SVT_KG_SPR = if_else(is.na(KILOHAIL),0,KILOHAIL),
                        SVT_KG_TRR = if_else(is.na(KIRJOLOH),0,KIRJOLOH),
                        SVT_KG_TRS = if_else(is.na(TAIMEN),0,TAIMEN),
                        SVT_KG_TUR = if_else(is.na(PKAMP),0,PKAMP),
                        SVT_KG_WHG = if_else(is.na(VALKOTURSKA),0,VALKOTURSKA),
                        SVT_KG_TOTAL = SVT_KG_COD+SVT_KG_ELE+SVT_KG_FBM+SVT_KG_FBU+
                          SVT_KG_FID+SVT_KG_FIN+SVT_KG_FLE+SVT_KG_FPE+SVT_KG_FPI+
                          SVT_KG_FPP+SVT_KG_FRO+SVT_KG_FVE+SVT_KG_HER+SVT_KG_PLE+
                          SVT_KG_PLN+SVT_KG_SAL+SVT_KG_SME+SVT_KG_SPR+SVT_KG_TRR+
                          SVT_KG_TRS+SVT_KG_TUR+SVT_KG_WHG)

# shorelogs
shorelogs_13_15 <- shorelogs_13_15 |> mutate(
                        SVT_KG_COD = if_else(is.na(TURSKA),0,TURSKA),
                        SVT_KG_ELE = if_else(is.na(ANKERIAS),0,ANKERIAS),   
                        SVT_KG_FBM = if_else(is.na(LAHNA),0,LAHNA),
                        SVT_KG_FBU = if_else(is.na(MADE),0,MADE),
                        SVT_KG_FID = if_else(is.na(SAYNE),0,SAYNE),
                        SVT_KG_FIN = if_else(is.na(MUU_KALA),0,MUU_KALA),
                        SVT_KG_FLE = if_else(is.na(KAMPELA),0,KAMPELA),
                        SVT_KG_FPE = if_else(is.na(AHVEN),0,AHVEN),
                        SVT_KG_FPI = if_else(is.na(HAUKI),0,HAUKI),
                        SVT_KG_FPP = if_else(is.na(KUHA),0,KUHA),
                        SVT_KG_FRO = if_else(is.na(SARKI),0,SARKI),
                        SVT_KG_FVE = if_else(is.na(MUIKKU),0,MUIKKU),
                        SVT_KG_HER = if_else(is.na(SILAKKA),0,SILAKKA),
                        SVT_KG_PLE = if_else(is.na(PUNAKAMPELA),0,PUNAKAMPELA),
                        SVT_KG_PLN = if_else(is.na(SIIKA),0,SIIKA),
                        SVT_KG_SAL = if_else(is.na(LOHI_KG),0,LOHI_KG),
                        SVT_KG_SME = if_else(is.na(KUORE),0,KUORE),
                        SVT_KG_SPR = if_else(is.na(KILOHAIL),0,KILOHAIL),
                        SVT_KG_TRR = if_else(is.na(KIRJOLOH),0,KIRJOLOH),
                        SVT_KG_TRS = if_else(is.na(TAIMEN),0,TAIMEN),
                        SVT_KG_TUR = if_else(is.na(PKAMP),0,PKAMP),
                        SVT_KG_WHG = if_else(is.na(VALKOTURSKA),0,VALKOTURSKA),
                        SVT_KG_TOTAL = SVT_KG_COD+SVT_KG_ELE+SVT_KG_FBM+SVT_KG_FBU+
                          SVT_KG_FID+SVT_KG_FIN+SVT_KG_FLE+SVT_KG_FPE+SVT_KG_FPI+
                          SVT_KG_FPP+SVT_KG_FRO+SVT_KG_FVE+SVT_KG_HER+SVT_KG_PLE+
                          SVT_KG_PLN+SVT_KG_SAL+SVT_KG_SME+SVT_KG_SPR+SVT_KG_TRR+
                          SVT_KG_TRS+SVT_KG_TUR+SVT_KG_WHG)

#.------------------------------------------------------------------------------
#                   8. FT_TRIP                                              ####    
#.------------------------------------------------------------------------------

## logbooks ####
# DCPROD script 1
logbook_13_15 <- logbook_13_15 |> 
  mutate(FT_TRIP = 
           case_when(
             PYYDYS %in% c(16,17,18,19,20,38,39,40,41) ~ "TM", 
             # MGO = nuotat = muu aktiivinen pyydys
             PYYDYS %in% c(21) ~ "MGO", 
             # HOK = koukut
             PYYDYS %in% c(14,15,30,31,32) ~ "HOK", 
             # DFN = ajo- ja seisovat verkot
             PYYDYS %in% c(5,6,7,8,9,10,11,12,13,22,23,24,25,35,44,45) ~ "DFN", 
             # FPO = rysät, merrat ja muut sulkupyydykset, rysille vain yksi ft
             PYYDYS %in% c(1,2,3,4,26,27,28,29,34,37) ~ "FPO", 
             # PGO = muut passiiviset pyydykset
             PYYDYS %in% c(32,33,36) ~ "PGO"
           )) 

logbook_13_15$FT_TRIP <- ifelse(logbook_13_15$PITUUS < 1200 & logbook_13_15$PYYDYS %in% 
                              c(1,2,3,4,26,27,28,29,34,37,5,6,7,8,9,10,11,
                                12,13,22,23,24,25,35,44,45,14,15,30,31,32,32,33,36), "PG", logbook_13_15$FT_TRIP)

logbook_13_15 |> count(FT_TRIP)
logbook_13_15 |> count(FT)

# correct annual FT 
tmp <- logbook_13_15 |> count(ALUS, FT_TRIP, KALASTUSVUOSI)

tmp <- tmp |> 
  group_by(ALUS, KALASTUSVUOSI) |> 
  filter(n == max(n)) |> 
  rename(FT = FT_TRIP)

nrow(tmp)
tmp <- na.omit(tmp)
nrow(tmp) # .. ei puuttuvia 
tmp <- dplyr::distinct(tmp, ALUS, .keep_all = T)
nrow(tmp)

# merge back
logbook_13_15 <- dplyr::left_join(logbook_13_15, tmp |> select(-n), 
                              by  = join_by("ALUS", "KALASTUSVUOSI"), relationship='many-to-many')

logbook_13_15 <- logbook_13_15 |> rename(ft_orig=FT.x, FT=FT.y)

# ... muokataan vastaavalla tavalla 
logbook_13_15$FT <- ifelse(logbook_13_15$FT != "TM", "PG", logbook_13_15$FT)

## shore ####
shorelogs_13_15 <- shorelogs_13_15 |>
  mutate(FT_TRIP = 
           case_when(
             PYYDYS %in% c(16,17,18,19,20,38,39,40,41) ~ "TM", 
             # MGO = nuotat = muu aktiivinen pyydys
             PYYDYS %in% c(21) ~ "MGO", 
             # HOK = koukut
             PYYDYS %in% c(14,15,30,31,32) ~ "HOK", 
             # DFN = ajo- ja seisovat verkot
             PYYDYS %in% c(5,6,7,8,9,10,11,12,13,22,23,24,25,35,44,45) ~ "DFN", 
             # FPO = rysät, merrat ja muut sulkupyydykset, rysille vain yksi ft
             PYYDYS %in% c(1,2,3,4,26,27,28,29,34,37) ~ "FPO", 
             # PGO = muut passiiviset pyydykset
             PYYDYS %in% c(32,33,36) ~ "PGO"
           )) 

shorelogs_13_15$FT_TRIP <- ifelse(shorelogs_13_15$PITUUS < 1200 & shorelogs_13_15$PYYDYS %in% 
                                    c(1,2,3,4,26,27,28,29,34,37,5,6,7,8,9,10,11,
                                      12,13,22,23,24,25,35,44,45,14,15,30,31,32,32,33,36), "PG", shorelogs_13_15$FT_TRIP)

shorelogs_13_15 |> count(KALASTUSVUOSI, FT_TRIP)
shorelogs_13_15 |> count(FT)

# correct annual FT 
tmp <- shorelogs_13_15 |> count(ALUS, FT_TRIP, KALASTUSVUOSI)
tmp <- tmp |> 
  group_by(ALUS, KALASTUSVUOSI) |> 
  filter(n == max(n)) |> 
  rename(FT = FT_TRIP)

nrow(tmp)
tmp <- na.omit(tmp)
nrow(tmp) # .. ei puuttuvia 
tmp <- dplyr::distinct(tmp, ALUS, .keep_all = T)
nrow(tmp)


shorelogs_13_15 <- dplyr::left_join(shorelogs_13_15, tmp |> select(-n), 
                                    by  = join_by("ALUS", "KALASTUSVUOSI"), relationship='many-to-many')

shorelogs_13_15 <- shorelogs_13_15 |> rename(ft_orig=FT.x, FT=FT.y)

# ... muokataan vastaavalla tavalla 
shorelogs_13_15$FT <- ifelse(shorelogs_13_15$FT != "PG", "PG", shorelogs_13_15$FT)

#.------------------------------------------------------------------------------
#                   8.1. fecR for logbooks only                             ####    
#.------------------------------------------------------------------------------

# here fecR  code !!!! script 1 DCRPOD, line 1846-2499

# ... ... ... ... ... ...  EFFORT ... ... ... ... ... ... #




# ... ... MERIPÄIVÄT JA KALASTUSPÄIVÄT ... ... #


# ... lataa paketit
#suppressMessages({
#library(dplyr)
#library(fecR, lib.loc = paste0(getwd(), "/fecR-LOCAL"))
#})

# ... apufunktio
missing_stats <- function(df, column, y) {
  
  tbl <- data.frame(VALUE_ROWS = table(is.na(df[,column]))[1],
                    NA_ROWS = table(is.na(df[,column]))[2])
  
  if (!(is.na(tbl$NA_ROWS))) {
    cat(paste("*************************", "\n","VARIABLE:", column, "\t YEAR:",y, "\n \n",
              "No. of rows in DF: ", nrow(df), "\n",
              "Rows with value: ", tbl$VALUE_ROWS,  "\n",
              "Rows with NA: ", tbl$NA_ROWS, "\n\n",
              "Missing percentage: ", 100*round(tbl$NA_ROWS/nrow(df), digits = 3), "%",
              "\n", "*************************", sep = ""))
  } else {
    invisible() # PASS message(paste("No missing values in variable", column, sep = " "))
  }
}







# ... ... ... @HUOM EFFORTIN LASKENTA TEHDÄÄN ERIKSEEN JOKAISELLE VUODELLE ... ... ... # 


#       SYY: fecR-paketin palauttama tulosaineistossa ei ole kalastuspäivämäärää mukana
#            Jotta takaisinaggregoinnin saa validoitua, on laskettava ponnistus vuosittain
#            (Kalastusmatkat, jotka alkavat loppuvuodesta ja päättyvät seuraavan vuoden puolella
#             aiheuttavata muutoin ongelmia)







# ... ... ... .. . DATA PREPARATION ... ... ... .. . #



# The effort for fishing diary logbooks is calculated via `fecR`-package developed at the 2nd Workshop on Transversal Variables
# held in Nikosia. The vignettes and package charasteristics can be found from
# https://rdrr.io/cran/fecR/f/vignettes/calculating_fishing_effort.Rmd

# The Nikosia package requires that the logbook data is processed into strict format including naming
# conventions etc. 


# The corresponding variables in FIN logbook data:

#* CHR eunr_id = ULKOINENTUNNUS  | Vessel identifier, anonymous
#* NUM loa = pituus   | Vessel length in cm (NOTE: not in meters!)
#* NUM gt = VETOISUUS  | Gross tonnage
#* NUM kw = PAAKONETEHO  | Engine power
#* CHR trip_id = KMATKA_TUNNUS   | Unique identifier for fishing trip
#* CHR depdate = LAHTOPVM, LAHTOPVM1   | Date of trip departure
#* CHR deptime = LAHTOAIKA   | Time of trip departure
#* CHR retdate = PALUUPVM    | Date of trip return
#* CHR rettime = PALUUAIKA   | Time of trip return
#* CHR fishdate = KALASTUSPVM  | Date of fishing operation
#* CHR gear = GEAR | Gear used for specific fishing operation
#* INT gear_mesh_size = SILMAKOKO | Mesh size in mm
#* CHR fishing_area = derive from ICES  | Area where the fishing operation took place. DCF level 3 (level 4 for Baltic)
#* CHR economic_zone = "EU" fixed value  | Economic zone where the fishing operation took place
#* CHR rectangle = RECTANGLE | Rectangle where fishing operation took place





# .. ladataan edellä muodostettu aineisto 
LogBookTimeSeries <- readRDS(paste0(getwd(), "/PROD/DATA-TEMP/PVK02T-KALASTUSPAIVAKIRJA.rds"))


# ... aineistossa olevat vuodet (KALASTUSPVM MUKAAN)
fishYears <- sort(unique(LogBookTimeSeries$KALASTUSVUOSI))

# ... tallennetaan vektori
saveRDS(fishYears, paste0(getwd(), "/PROD/DATA-TEMP/fishYears.rds"))


# ... lista tulosaineistoille 
DataListLogBookPrepared <- list()

# ... lista alkuperäisille vuosiaineistoilla, joihin on tehty fecR-muuttujat (linkkausta varten)
DataListLogBookOriginal <- list()

cat("\nValmistellaan aineisto pyyntiponnistuksen laskentaa varten ...")

cat("\n-fecR laskentafunktio ei salli puuttuvaa tietoa.")
cat("\nPoistetaan puuttuvat ...\n\n")

for (y in fishYears) {
  
  #cat("\rKasitellaan vuotta ", y, " ... " )
  
  # ... indeksi jokaiselle (vuosi)kierrokselle 
  i <- y+1-min(fishYears)
  
  # ... data 
  LogBook <- LogBookTimeSeries[which(LogBookTimeSeries$KALASTUSVUOSI==y),]
  
  
  ## CHR eunr_id = ULKOINENTUNNUS  | Vessel identifier, anonymous
  # RDBES NOTE: add the encryption of vessel ID later
  LogBook$eunr_id <- LogBook$ULKOINENTUNNUS
  #missing_stats(LogBook, "eunr_id", y)
  
  
  ## NUM loa = KOKPITUUS   | Vessel length in cm (NOTE: not in meters!)
  LogBook$loa <- as.numeric(LogBook$KOKONAISPITUUS*100.0)
  #missing_stats(LogBook, "loa", y)
  
  ## NUM gt = VETOISUUS  | Gross tonnage
  LogBook$gt <- as.numeric(LogBook$VETOISUUS)
  #missing_stats(LogBook, "gt", y)
  
  ## NUM kw = PAAKONETEHO  | Engine power,
  LogBook$kw <- as.numeric(LogBook$PAAKONETEHO)
  #missing_stats(LogBook, "kw", y)
  
  ## CHR trip_id = KMATKA_TUNNUS   | Unique identifier for fishing trip
  LogBook$trip_id <- as.character(LogBook$KMATKA_TUNNUS)
  #missing_stats(LogBook, "trip_id", y)
  
  ## CHR depdate = LAHTOPVM, LAHTOPVM1   | Date of trip departure
  # format: YYYYMMDD
  LogBook$depdate <- gsub('-','', as.character(LogBook$LAHTOPVM))
  #missing_stats(LogBook, "depdate", y)
  
  ## CHR deptime = LAHTOAIKA   | Time of trip departure
  # format: HHMM or HH:MM
  LogBook$deptime <- gsub(":", "", as.character(LogBook$LAHTOAIKA))
  #missing_stats(LogBook, "deptime", y)
  
  ## CHR retdate = PALUUPVM    | Date of trip return
  # format: YYYYMMDD
  
  # ... Jos paluupäivä puuttuu, niin PALUUPVM = PURKUPVM
  LogBook$PALUUPVM <- ifelse(is.na(LogBook$PALUUPVM), as.character(LogBook$PURKUPVM), as.character(LogBook$PALUUPVM))
  LogBook$retdate <- gsub('-','', as.character(LogBook$PALUUPVM))
  #missing_stats(LogBook, "retdate", y)
  
  ## CHR fishdate = KALASTUSPVM  | Date of fishing operation
  # format: YYYYMMDD
  LogBook$fishdate <- substr(gsub('-','', as.character(LogBook$KALASTUSPVM)),1,8)
  #missing_stats(LogBook, "fishdate", y)
  
  ## CHR gear = gear | Gear used for specific fishing operation
  LogBook$gear <- LogBook$GEAR
  #missing_stats(LogBook, "gear", y)
  
  # INT gear_mesh_size = SILMAKOKO | Mesh size in mm ()
  LogBook$gear_mesh_size = LogBook$SILMAKOKO
  #missing_stats(LogBook, "gear_mesh_size", y)
  
  
  ## CHR rettime = PALUUAIKA   | Time of trip return
  # format: HHMM or HH:MM
  LogBook$rettime <- gsub(":", "", LogBook$PALUUAIKA)
  #missing_stats(LogBook, "rettime", y)
  
  
  
  ## CHR | fishing_area = derived from ICES | Area where fishing operation took place
  # format: UPCASE
  LogBook[LogBook$ICES == 22, "fishing_area"] <- "27.3.d.22"
  LogBook[LogBook$ICES == 24, "fishing_area"] <- "27.3.d.24"
  LogBook[LogBook$ICES == 25, "fishing_area"] <- "27.3.d.25"
  LogBook[LogBook$ICES == 26, "fishing_area"] <- "27.3.d.26"
  LogBook[LogBook$ICES == 27, "fishing_area"] <- "27.3.d.27"
  LogBook[LogBook$ICES == 28, "fishing_area"] <- "27.3.d.28"
  LogBook[LogBook$ICES == 29, "fishing_area"] <- "27.3.d.29"
  LogBook[LogBook$ICES == 30, "fishing_area"] <- "27.3.d.30"
  LogBook[LogBook$ICES == 31, "fishing_area"] <- "27.3.d.31"
  LogBook[LogBook$ICES == 32, "fishing_area"] <- "27.3.d.32"
  
  LogBook$fishing_area <- toupper(LogBook$fishing_area)
  #missing_stats(LogBook, "fishing_area", y)
  
  ## CHR economic_zone = "EU" fixed value  | Economic zone the fishing operation took place
  LogBook$economic_zone <- "EU"
  
  ## CHR rectangle = RECTANGLE | Rectangle where fishing operation took place
  LogBook$rectangle <- as.character(LogBook$RECTANGLE)
  #missing_stats(LogBook, "rectangle", y)
  
  # The data cannot include any other columns
  fecR_columns <- c("eunr_id",
                    "loa",
                    "gt",
                    "kw",
                    "trip_id",
                    "depdate",
                    "deptime",
                    "retdate",
                    "rettime",
                    "fishdate",
                    "gear",
                    "gear_mesh_size",
                    "fishing_area",
                    "economic_zone",
                    "rectangle")
  
  
  
  
  # ... @HUOM ... #
  
  # ... aineisto jemmaan ennen kuin rajataan sisältö fecR-paketille 
  DataListLogBookOriginal[[i]] <- LogBook
  
  
  # ... Order & Select columns
  LogBook <- LogBook[, fecR_columns]
  
  
  # ... OMIT all missing values (NA not allowed)
  nrow_lb <- nrow(LogBook)
  LogBook <- na.omit(LogBook)
  nrow_lb_omitted <- nrow(LogBook)
  
  
  
  if ((nrow_lb-nrow_lb_omitted) > 0) {
    message(paste("\r\t", y ,"poistettu", nrow_lb-nrow_lb_omitted, "rivia [",
                  100*round((nrow_lb-nrow_lb_omitted) / nrow_lb, digits = 3) ,"% ]"))
  }
  
  
  
  # ... OMIT duplicate rows
  LogBook <- distinct(LogBook)
  
  # ... palautetaan data frameksi
  LogBook <- as.data.frame(LogBook)
  
  # ... tehdään matkan yksilöivä muuttuja (muuten tulee herjaa, koska on paritroolausta)
  LogBook$trip_id <- paste0(LogBook$eunr_id, LogBook$trip_id, LogBook$depdate)
  
  # ... poistetaan tuplat, jos on 
  LogBook <-  dplyr::distinct(LogBook, .keep_all = TRUE)
  
  
  # ... tallennetaan fecR-pakettia varten muokatut aineistot
  DataListLogBookPrepared[[i]] <- LogBook 
  
  
  
  
  
}








# ... ... ... .. . AINEISTON TARKISTAMINEN ... ... ... .. . #



cat("\nValidoidaan aineisto [fecR::check_format()] ...")
# ... tekninen validointi voidaan tehdä koko aikasarjalla yhdellä kertaa 
#suppressMessages({
#  check_format(dplyr::bind_rows(DataListLogBookPrepared))
#})

# ... funktio, joka hiljentää fecR-paketin tulostukset (varoitukset ja errorit näkyy, jos niitä tulee)
quietlyCheckFormat <- function() {
  sink(tempfile(), type = "output")
  invisible(fecR::check_format(dplyr::bind_rows(DataListLogBookPrepared)))
  sink()
}


quietlyCheckFormat()


# ... kommentoi ja aja allaoleva, jos haluat tietää mitä check_format() tekee
#?check_format()



# @HUOM FUNKTIO TULOSTAA OLETUKSENA DATAN (TYPERÄÄ->KORJATTU 12.3.2024 NS. TUOTANTOON);
#       SKROLLAAMALLA YLÖS NÄET TARKASTUKSEN TULOKSEN. KAIKKI ON HYVIN
#       JOS VIESTI SANOO "The returned data passes the check."
#       MUUSSA TAPAUKSESSA TULEE PUNAINEN VAROITUS, JOKA KERTOO MISSÄ VIKA 





# ... ... ... .. . LASKETAAN EFFORT ... ... ... .. . #



# fecR-package includes function calc_fishing_effort() to calculate the fishing effort 
# INPUT: Dataset holding individual fishing trips in format as described
# OUTPUT: A list of two dataframes holding the "days at sea" and "fishing days" 

DataListDaysAtSea <- list()
DataListFishingDays <- list()

# ... funktio, joka laskee ponnistuksen
# ?calc_fishing_effort()

cat("\nLasketaan ponnistus [fecR::calc_fishing_effort()] ...\n")
# ... lasketaan vuosittainen pyyntiponnistus 
for (y in fishYears) {
  
  
  # ... indeksi jokaiselle (vuosi)kierrokselle 
  i <- y+1-min(fishYears)
  
  # calculate the effort - function returns a list of two df 
  effort_pvk_LB <- fecR::calc_fishing_effort(DataListLogBookPrepared[[i]], check=FALSE)
  
  # Days at sea - dataframe 
  DataListDaysAtSea[[i]] <- effort_pvk_LB$days_at_sea
  
  # Fishingdays - dataframe
  DataListFishingDays[[i]] <- effort_pvk_LB$fishing_days
  
  
  #cat("\n\rLasketaan pyyntiponnistus vuodelle",y," - Käsittelyssä: ", i, " / ", length(fishYears)," aikasarjan vuosista.", flush = TRUE)
  cat("\rLasketaan kalastuspaivakirjan pyyntiponnistus",y," ... ")
  
}





# ... ... ... TALLENNETAAN AINEISTOLISTAT JA TYHJENNETÄÄN ISTUNTO ... ... ... # 


# ... kalastuspäivät löytyy 
#str(DataListFishingDays)
saveRDS(DataListFishingDays, paste0(getwd(), "/PROD/DATA-TEMP/DataListFishingDays.rds"))

# ... ja meripäivät 
#str(DataListDaysAtSea)
saveRDS(DataListDaysAtSea, paste0(getwd(), "/PROD/DATA-TEMP/DataListDaysAtSea.rds"))

# ... ja originaaliaineistot 
#str(DataListLogBookOriginal)
saveRDS(DataListLogBookOriginal, paste0(getwd(), "/PROD/DATA-TEMP/DataListLogBookOriginal.rds"))


# ... siivous
cleanup(except_these = c("ICES_AVE_KG_PRICES", "LOGBOOK"))





# ... ... Viedään EFFORT alkuperäiseen aineistoon ... ... #



cat("\nPalautetaan ponnistus aineistoon (fecR tuottaman aggregoinnin purkaminen) ...\n")

# ... luetaan edellä tallennetut aineistolistat 
DataListFishingDays <- readRDS(paste0(getwd(), "/PROD/DATA-TEMP/DataListFishingDays.rds"))
DataListDaysAtSea <- readRDS(paste0(getwd(), "/PROD/DATA-TEMP/DataListDaysAtSea.rds"))
DataListLogBookOriginal <- readRDS(paste0(getwd(), "/PROD/DATA-TEMP/DataListLogBookOriginal.rds"))


# ... ja kalastusvuodet vektori
fishYears <- readRDS(paste0(getwd(), "/PROD/DATA-TEMP/fishYears.rds"))

# ... tarvittavat paketit takaisin
#library(dplyr)


# ... tähän listaan tallennetaan aineistot, joihin on lisätty ponnistustiedot 
DataListLogBookWithEffort <- list()



for (y in fishYears) {
  
  cat("\rValidoidaan pyyntiponnistus", y, "... " )
  
  # ... indeksi jokaiselle (vuosi)kierrokselle 
  i <- y+1-min(fishYears)
  
  
  # ... originaalivuosiaineisto
  LBORIG <- DataListLogBookOriginal[[i]]
  
  # ... vuosikohtaiset tulosaineistot 
  seadays_pvk_LB <- DataListDaysAtSea[[i]]
  fishingdays_pvk_LB <- DataListFishingDays[[i]]
  
  
  
  
  
  
  
  # @HUOM OSASSA MATKOJA ON 0 MERI/KALASTUSPÄIVÄÄ TAI NEGATIIVINEN MERI/KALASTUSPÄIVÄT
  # -> JOHTUU VIRHEELLISISTÄ KIRJAUKSISTA LÄHDEAINEISTOSSA (NÄMÄ OLISI HYVÄ KÄYDÄ LÄPI,
  #     MUTTA LAITETAAN NYT NÄILLE ARVOKSI 1) 
  
  # @HUOM KOSKEE VAIN VUOSIA 2016-2019
  # cat("\n", y)
  # print(summary(seadays_pvk_LB$days_at_sea))
  # print(summary(fishingdays_pvk_LB$fishing_days))
  # cat("\n*****************************")
  
  
  
  
  seadays_pvk_LB$days_at_sea <- ifelse(seadays_pvk_LB$days_at_sea<0 | seadays_pvk_LB$days_at_sea==0,
                                       1,
                                       seadays_pvk_LB$days_at_sea)
  
  
  fishingdays_pvk_LB$fishing_days <- ifelse(fishingdays_pvk_LB$fishing_days<0 | fishingdays_pvk_LB$fishing_days==0,
                                            1,
                                            fishingdays_pvk_LB$fishing_days)
  
  
  # ... tehdään trip_id linkkausta varten (kuten se tehtiin aiemmin)
  # ... @HUOM LAHTOPVM on mukana siltä varalta, jos eri vuosina on samoja kalastusmatkatunnuksia 
  LBORIG$trip_id <- paste0(LBORIG$ULKOINENTUNNUS, LBORIG$KMATKA_TUNNUS, gsub('-','', as.character(LBORIG$LAHTOPVM)))
  
  
  
  
  # ... liitetään effort aineistoon
  
  
  
  # (1) Mikä sarakeyhdistelmä yksilöi tulosaineiston rivin?
  
  # ... meripäivien tulosaineistolle 
  seadays_pvk_LB$IDENTIFIER <- paste0(seadays_pvk_LB$trip_id, 
                                      seadays_pvk_LB$gear, 
                                      seadays_pvk_LB$gear_mesh_size, 
                                      seadays_pvk_LB$fishing_area)
  
  
  if (length(unique(seadays_pvk_LB$IDENTIFIER))==nrow(seadays_pvk_LB)) {
    invisible() # ... PASS 
  } else {
    warning("Vuonna ", y, " meripäivien tulosaineistossa puutteellinen IDENTIFIER")
  }
  
  
  
  # ... kalastuspäivien tulosaineistolle 
  fishingdays_pvk_LB$IDENTIFIER <- paste0(fishingdays_pvk_LB$trip_id, fishingdays_pvk_LB$gear, 
                                          fishingdays_pvk_LB$gear_mesh_size, fishingdays_pvk_LB$fishing_area,
                                          fishingdays_pvk_LB$rectangle)
  
  
  
  # ... tarkistus 
  if (nrow(fishingdays_pvk_LB)==length(unique(fishingdays_pvk_LB$IDENTIFIER))) {
    invisible() # ... PASS 
  } else {
    warning("Vuonna ", y, " kalastuspäivien tulosaineistossa puutteellinen IDENTIFIER")
  }
  
  
  # ... tehdään vastaavat muuttujat alkuperäiseen aineistoon linkkausta varten 
  LBORIG$SEADAYS_IDENTIFIER <- paste0(LBORIG$trip_id, LBORIG$gear, LBORIG$gear_mesh_size, LBORIG$fishing_area)
  
  LBORIG$FISHINGDAYS_IDENTIFIER <- paste0(LBORIG$trip_id, LBORIG$gear, 
                                          LBORIG$gear_mesh_size, LBORIG$fishing_area,
                                          LBORIG$rectangle)
  
  
  # ... meripäivät 
  LBORIG <- left_join(LBORIG, seadays_pvk_LB[c("IDENTIFIER", "days_at_sea")], 
                      by=join_by("SEADAYS_IDENTIFIER"=="IDENTIFIER"))
  
  
  # ... kalastuspäivät @HUOM Nyt myös rectangle! 
  LBORIG <- left_join(LBORIG, fishingdays_pvk_LB[c("IDENTIFIER", "fishing_days")], 
                      by=join_by("FISHINGDAYS_IDENTIFIER"=="IDENTIFIER"))
  
  
  
  # ... tehdään aineistoon HAULDIVIDER_DAYS_AT_SEA = jakaja
  LBORIG <- LBORIG %>%
    group_by(trip_id, fishing_area, gear, gear_mesh_size) %>%
    mutate(HAULDIVIDER_DAYS_AT_SEA = n_distinct(KKERTA_TUNNUS))
  
  
  # ... jaetaan meripäivät kalastuskerroille annettujen luokittelijoiden (fishing_area, gear, gear_mesh_size) mukaan
  LBORIG$MERIPAIVAT <- LBORIG$days_at_sea / LBORIG$HAULDIVIDER_DAYS_AT_SEA 
  
  
  # ... tehdään aineistoon HAULDIVIDER_FISHING_DAYS = jakaja
  LBORIG <- LBORIG %>%
    group_by(trip_id, fishing_area, gear, gear_mesh_size, rectangle) %>%
    mutate(HAULDIVIDER_FISHING_DAYS = n_distinct(KKERTA_TUNNUS))
  
  
  # ... jaetaan kalastuspäivät kalastuskerroille annettujen luokittelijoiden (fishing_area, gear, gear_mesh_size, rectangle) mukaan 
  LBORIG$KALASTUSPAIVAT <- LBORIG$fishing_days / LBORIG$HAULDIVIDER_FISHING_DAYS
  
  
  
  # ... palautetaan data frameksi
  LBORIG <- data.frame(LBORIG)
  
  
  # .. virhetarkistus 
  SeaDaysError <- c()
  FishDaysError <- c()
  
  j=1
  k=1
  
  # ... kaikki matkat vektoriin
  TRIPS <- unique(seadays_pvk_LB$trip_id)
  
  for (i in TRIPS) {
    
    origSeaDays <- round(sum(LBORIG[which(LBORIG$trip_id==i), c("MERIPAIVAT")], na.rm = T))
    fecRSeaDays <- round(sum(seadays_pvk_LB[which(seadays_pvk_LB$trip_id==i), "days_at_sea"]))
    
    origFishDays <- round(sum(LBORIG[which(LBORIG$trip_id==i), c("KALASTUSPAIVAT")], na.rm = T))
    fecRFishDays <- round(sum(fishingdays_pvk_LB[which(fishingdays_pvk_LB$trip_id==i), "fishing_days"]))
    
    if (!(origSeaDays==fecRSeaDays)) {
      SeaDaysError[j] <- i
      j = j + 1
      #cat("\n\n","MERIPÄIVÄT matka:",i, "Aineiston summa:", origSeaDays, "ja fecR summa:", fecRSeaDays)
    }
    
    if (!(origFishDays==fecRFishDays)) {
      FishDaysError[j] <- i
      k = k + 1
      #cat("\n\n","KALASTUSPÄIVÄT matka:",i, "Aineiston summa:", origFishDayss, "ja fecR summa:", fecRFishDays)
    }
    
    if (i==TRIPS[length(TRIPS)]) {
      
      if (j==1) {
        #print(j) #cat("\n\r Aineistoon jaoteltu MERIPAIVAT-muuttujan summa vastaa fecR-laskentafunktion summaa jokaisella kalastusmatkalla vuonna", y)
      }
      
      if (k==1) {
        #cat("\n\r Aineistoon jaoteltu KALASTUSPAIVAT-muuttujan summa vastaa fecR-laskentafunktion summaa jokaisella kalastusmatkalla vuonna", y)
        #print(k)
      }
      
      if (k>1 | j>1) {
        warning("Aineistoon jaetut paivien summat eivat vastaa fecR-tuloksia vuonna.", y, "Tarkasta virheelliset matkat vektoreista SeaDaysError ja FishDaysError")
      }
      
      
    }
    
  }
  
  
  
  
  
  
  
  
  
  # ... tallennetaan aineisto
  DataListLogBookWithEffort[[i]] <- LBORIG
  
  # # ... tuloste
  # cat("\n Vuonna", y, "yhteensä", "\n",
  #     "\t - MERIPÄIVIÄ", sum(LBORIG$MERIPAIVAT, na.rm=T), "(fecR summa", sum(seadays_pvk_LB$days_at_sea),")", "\n",
  #     "\t - KALASTUSPÄIVIÄ", sum(LBORIG$KALASTUSPAIVAT, na.rm=T), "(fecR summa", sum(fishingdays_pvk_LB$fishing_days),")")
  
  
} # END


if ((k+j)==2) {
  cat("\n-Aineiston MERIPAIVAT-muuttujan summa vastaa fecR-laskentafunktion summaa aikasarjan jokaisella kalastusmatkalla")
  cat("\n-Aineiston KALASTUSPAIVAT-muuttujan summa vastaa fecR-laskentafunktion summaa aikasarjan jokaisella kalastusmatkalla")
} else {
  invisible()
}


# ... palautetaan aikasarjaksi
pvk03 <- dplyr::bind_rows(DataListLogBookWithEffort)




# # ... kuva @TODO -> VIE PDF !!!
# meripaivat <- c()
# kalastuspaivat <- c()
# kalastusvuodet <- c()
# 
# 
# 
# for (y in fishYears) {
#   i <- y+1-min(fishYears)
#   
#   kalastusvuodet[i] <- y
#   meripaivat[i] <- sum(pvk03[which(pvk03$KALASTUSVUOSI==y), "MERIPAIVAT"], na.rm=T)
#   kalastuspaivat[i] <- sum(pvk03[which(pvk03$KALASTUSVUOSI==y), "KALASTUSPAIVAT"], na.rm=T)
#   
# }
# 
# 
# plot(kalastusvuodet, meripaivat, type = "b", ylab = "LKM", xlab="VUOSI", main = "Pyyntiponnistus - Kalastuspäiväkirja")
# mtext(paste0("Tiedot haettu kalatalouden keskusrekisterin tietokannasta ", kakeTimeStamp), line = 0.5)
# lines(kalastusvuodet, kalastuspaivat, type = "b", col="orange")
# legend("topright", legend = c("Meripäivät", "Kalastuspäivät"), lty = c(1,1), pch = 1, col = c("black", "orange"))
# 


# .... jakaumia
table(pvk03$KALASTUSVUOSI)


# ... lisämuuttujat 
pvk03$LOMAKE <- "KALASTUSPAIVAKIRJA"


# ... tallennetaan aineisto 
LOGBOOK <- pvk03
saveRDS(LOGBOOK, paste0(getwd(), "/PROD/DATA-PG/LOGBOOK.rds"))

cat("\n\nTallennetaan aikasarja-aineistot", paste(as.character(min(pvk03$KALASTUSVUOSI)), as.character(max(pvk03$KALASTUSVUOSI)) , sep="-"), "... \n\n")

# ... tyhjennetään ympäristö (paitsi välttämättömät ja tietokantaan vietävä data)
cleanup(except_these = c("ICES_AVE_KG_PRICES", "LOGBOOK"))


cat("\t\t* Aineisto PVK01-KALASTUSPAIVAKIRJA muodostettu [", paste0(getwd(), "/PROD/DATA-TEMP ]"))
cat("\n\t\t* Aineisto PVK02T-KALASTUSPAIVAKIRJA muodostettu [", paste0(getwd(), "/PROD/DATA-TEMP ]"))
cat("\n\t\t* Aineisto LOGBOOK muodostettu [", paste0(getwd(), "/PROD/DATA-PG ]"))


#.------------------------------------------------------------------------------
#                   8.2. fishing and seadays shore                          ####    
#.------------------------------------------------------------------------------

# in script 4
shorelogs_13_15$KALASTUSPAIVAT <- ifelse(shorelogs_13_15$PYYNTIPAIVAT==0, 1, shorelogs_13_15$PYYNTIPAIVAT)
shorelogs_13_15$MERIPAIVAT <- shorelogs_13_15$KALASTUSPAIVAT



#.------------------------------------------------------------------------------
#                   9. GT and KW days                                       ####    
#.------------------------------------------------------------------------------


#.------------------------------------------------------------------------------
#                   10. join datasets                                       ####    
#.------------------------------------------------------------------------------

# cheking logbook counter
logbook_13_15 |> select(KALASTUSVUOSI, KK, PYYNTIPV, KALASTUSKERTATUNNUS) |>
  arrange(KALASTUSVUOSI, KK, PYYNTIPV, KALASTUSKERTATUNNUS) |> View()

logbook_13_15$LOMAKE <- "KALASTUSPAIVAKIRJA"

# cheking logbook counter
shorelogs_13_15 <- shorelogs_13_15 |> arrange(KALASTUSVUOSI, KK, PYYNTIPV) |> 
  mutate(KALASTUSKERTATUNNUS = 1:n(),
         LOMAKE = "KKILMOITUS")

LOG <- logbook_13_15 |> select()
SHORE <- shorelogs_13_15 |> select()

D <- rbind(LOG, SHORE)

# cheking logbook counter
logbook_13_15 |> select(KALASTUSVUOSI, KK, PYYNTIPV, KALASTUSKERTATUNNUS) |>
  arrange(KALASTUSVUOSI, KK, PYYNTIPV, KALASTUSKERTATUNNUS) |> View()

# tehdään FT_REF- ja LE_ID -sarakkeet
# I think this needs ordering first
D <- D |> group_by(LOMAKE) |> mutate(LOMAKE_ID = paste0(LOMAKE, "_", row_number())) |> ungroup()
D <- D |> mutate(FT_REF = paste0("FT_REF_", ifelse(LOMAKE == "KALASTUSPAIVAKIRJA", paste0("KMATKA_", KALASTUSKERTATUNNUS), LOMAKE_ID)))
D <- D |> mutate(LE_ID = paste0("LE_ID_", ifelse(LOMAKE == "KALASTUSPAIVAKIRJA", paste0("KKERTA_", KKERTA_TUNNUS), LOMAKE_ID)))

#nimea trip_id uudelleen
D <- D |> rename(TRIP_ID_KAKE = trip_id)

# ... tallennetaan aineisto
KALASTUSAKTIVITEETTI_13_15 <- D


#.------------------------------------------------------------------------------
#                   11. add inactive vessels                                ####    
#.------------------------------------------------------------------------------










# join for table J
# Save this for J aktive vessels:
logbook_13_15_for_J <- logbook_13_15 |> selectt(YEAR=KALASTUSVUOSI, ULKOINENTUNNUS=ALUS, KALASTUSPAIVAT=TOTFISHDAYS, 
                                                MERIPAIVAT=TOTSEADAYS, PAAKONETEHO=TEHO, VETOISUUS, 
                                                FT_REF=FT, VESSEL_LENGTH, FISHING_TECH, 
                                                GEAR_TYPE, TARGET_ASSEMBLAGE, METIER=METIER6,
                                                
# need to create - check DCPROD R scrips 3 and 4
# FT_REF
# KALASTUSPAIVAT
# MERIPAIVAT

# from table the following variables are needed
# [1] "YEAR"              "ULKOINENTUNNUS"    "KALASTUSPAIVAT"    "MERIPAIVAT"        "PAAKONETEHO"       "VETOISUUS"        
# "FT_REF"            "VESSEL_LENGTH"     "FISHING_TECH"      "GEAR_TYPE"         "TARGET_ASSEMBLAGE"
# [13] "METIER"           "COUNTRY"           "QUARTER"           "MESH_SIZE_RANGE"   "METIER_7"          "SUPRA_REGION"     
# [61] "SUB_REGION"        "EEZ_INDICATOR"     "GEO_INDICATOR"     "SPECON_TECH"       "DEEP"





# saveRDS(logbook_13_15_for_J, file = paste0(path_der,"logbook_2013_15_for_J.rds"))

# wide to long table
logbook_13_15 <- tibble::rowid_to_column(logbook_13_15, "ID")

metier.fish <- logbook_13_15 |> select(ID, YEAR, ICES, SVT_KG_COD:SVT_KG_WHG) |>
                rename_with(~gsub("SVT_KG_", "", .x)) |>
                pivot_longer(!ID:ICES, names_to = "SPECIES", values_to = "KG")

# dim(metier.fish)

# get commercial value from DCPROD lookup table
commercial.value <- read.dbTable(schema=paste("2025-04-10", "-dcprod", sep = ""), 
                            table="keskihinnat_2025_05_06", dbname = "kake_siirto")

commercial.value <- commercial.value |> select(FAO_KOODI,VUOSI,ICES_ALUE,KESKIHINTA,KESKIHINTA_KOKO_MERIALUE) |>
                      mutate(FAO_KOODI = if_else(FAO_KOODI == "GAR", "PLE", FAO_KOODI)) |>
                      filter(FAO_KOODI %in% c("COD", "ELE", "FBM", "FBU", "FID", "FIN", "FLE", "FPE",
                                              "FPI", "FPP", "FRO", "FVE", "HER", "PLE", "PLN", "SAL",
                                              "SME", "SPR", "TRR", "TRS", "TUR", "WHG")) |> 
                      arrange(FAO_KOODI,VUOSI,ICES_ALUE)

commercial.value$KESKIHINTA[commercial.value$FAO_KOODI == "ELE" & 
                              (commercial.value$KESKIHINTA < 2 | is.na(commercial.value$KESKIHINTA)) ] <- 10

# create lookup of all combination years, ices, fish
# outer join to commericial.value
# fill in empty rows

# join area specific price
metier.fish <- metier.fish |> left_join(commercial.value,  
                            by=c("YEAR"="VUOSI", "ICES"="ICES_ALUE", "SPECIES"="FAO_KOODI"))

# average yearly prices (all ices)
tmp <- metier.fish |> group_by(YEAR,SPECIES) |> summarise(YEAR_AVG = mean(KESKIHINTA, na.rm=TRUE))

metier.fish <- metier.fish |> left_join(tmp, by=c("YEAR","SPECIES"))

# calculate value
metier.fish <- metier.fish |> mutate(VALUE = case_when(
          !is.na(KESKIHINTA) ~ KG*KESKIHINTA,
          is.na(KESKIHINTA) ~ KG*KESKIHINTA_KOKO_MERIALUE,
          is.na(KESKIHINTA) & is.na(KESKIHINTA_KOKO_MERIALUE) ~ KG*YEAR_AVG,
          TRUE ~ -999
))

# !!!HERE!!!
# majority of catches don't have KG

metier.fish |> filter(is.na(VALUE)) |> count(YEAR,ICES, SPECIES) |>
  adorn_totals("row") |> flextable() |> 
  set_caption("fish species without commercial value in certain year")

# tag <- metier.fish |> filter(is.na(VALUE)) |> count(YEAR,SPECIES)
# 
# test <- commercial.value |> filter(VUOSI %in% c(min(tag$YEAR)-1, unique(tag$YEAR), max(tag$YEAR)+1) &
#                         FAO_KOODI %in% tag$SPECIES) |> arrange(FAO_KOODI,VUOSI, ICES_ALUE)
  
  
# save value to table and add categories
metier.fish <- metier.fish |> select(ID, SPECIES, VALUE) |>
  pivot_wider(id_cols=ID, names_from = SPECIES, values_from = VALUE, names_prefix = "SVT_VALUE_") |> 
  mutate(SVT_VALUE_TOTAL = rowSums(across(SVT_VALUE_COD:SVT_VALUE_WHG)))

# add to logbook data
logbook_13_15 <- logbook_13_15 |> left_join(metier.fish, by="ID")


#.------------------------------------------------------------------------------
#                   10. matching akt1 column names                          ####    
#.------------------------------------------------------------------------------

logbook_13_15_akt1 <- logbook_13_15 |> mutate(C_SQUARE ="NA") |>
                        select(YEAR=KALASTUSVUOSI, ULKOINENTUNNUS=ALUS, KALASTUSPAIVAT=TOTFISHDAYS, 
                               MERIPAIVAT=TOTSEADAYS, PAAKONETEHO=TEHO, VETOISUUS, 
                               KALASTUSAIKAHH=???, FT_REF=FT, VESSEL_LENGTH, FISHING_TECH, 
                               GEAR_TYPE, TARGET_ASSEMBLAGE, METIER=METIER6, SVT_KG_HER, 
                               SVT_KG_SPR, SVT_KG_COD, SVT_KG_FLE, SVT_KG_TUR, SVT_KG_PLN, 
                               SVT_KG_SAL, SVT_KG_TRS, SVT_KG_SME, SVT_KG_FBM, SVT_KG_FID,
                               SVT_KG_FRO, SVT_KG_FPI, SVT_KG_FPE, SVT_KG_FPP, SVT_KG_FBU, SVT_KG_TRR,
                               SVT_KG_FVE, SVT_KG_ELE, SVT_KG_FIN, SVT_KG_TOTAL, SVT_VALUE_HER, SVT_VALUE_SPR,
                               SVT_VALUE_COD, SVT_VALUE_FLE, SVT_VALUE_TUR, SVT_VALUE_PLN, SVT_VALUE_SAL, 
                               SVT_VALUE_TRS, SVT_VALUE_SME, SVT_VALUE_FBM, SVT_VALUE_FID, SVT_VALUE_FRO, 
                               SVT_VALUE_FPI, SVT_VALUE_FPE, SVT_VALUE_FPP, SVT_VALUE_FBU, SVT_VALUE_TRR, 
                               SVT_VALUE_FVE, SVT_VALUE_ELE, SVT_VALUE_FIN, RECTANGLE, COUNTRY, QUARTER, 
                               MESH_SIZE_RANGE, METIER_7, SUPRA_REGION, SUB_REGION, EEZ_INDICATOR, 
                               GEO_INDICATOR, SPECON_TECH, DEEP, RECTANGLE_TYPE, C_SQUARE, LATITUDE, LONGITUDE)

grep("AIKA", names(logbook_13_15), value = TRUE)

# column names to match
# > names(akt1)
# [1] "YEAR"              "ULKOINENTUNNUS"    "KALASTUSPAIVAT"    "MERIPAIVAT"        "PAAKONETEHO"       "VETOISUUS"        
# [7] "KALASTUSAIKAHH"    "FT_REF"            "VESSEL_LENGTH"     "FISHING_TECH"      "GEAR_TYPE"         "TARGET_ASSEMBLAGE"
# [13] "METIER"            "SVT_KG_HER"        "SVT_KG_SPR"        "SVT_KG_COD"        "SVT_KG_FLE"        "SVT_KG_TUR"       
# [19] "SVT_KG_PLN"        "SVT_KG_SAL"        "SVT_KG_TRS"        "SVT_KG_SME"        "SVT_KG_FBM"        "SVT_KG_FID"       
# [25] "SVT_KG_FRO"        "SVT_KG_FPI"        "SVT_KG_FPE"        "SVT_KG_FPP"        "SVT_KG_FBU"        "SVT_KG_TRR"       
# [31] "SVT_KG_FVE"        "SVT_KG_ELE"        "SVT_KG_FIN"        "SVT_KG_TOTAL"      "SVT_VALUE_HER"     "SVT_VALUE_SPR"    
# [37] "SVT_VALUE_COD"     "SVT_VALUE_FLE"     "SVT_VALUE_TUR"     "SVT_VALUE_PLN"     "SVT_VALUE_SAL"     "SVT_VALUE_TRS"    
# [43] "SVT_VALUE_SME"     "SVT_VALUE_FBM"     "SVT_VALUE_FID"     "SVT_VALUE_FRO"     "SVT_VALUE_FPI"     "SVT_VALUE_FPE"    
# [49] "SVT_VALUE_FPP"     "SVT_VALUE_FBU"     "SVT_VALUE_TRR"     "SVT_VALUE_FVE"     "SVT_VALUE_ELE"     "SVT_VALUE_FIN"    
# [55] "RECTANGLE"         "COUNTRY"           "QUARTER"           "MESH_SIZE_RANGE"   "METIER_7"          "SUPRA_REGION"     
# [61] "SUB_REGION"        "EEZ_INDICATOR"     "GEO_INDICATOR"     "SPECON_TECH"       "DEEP"              "RECTANGLE_TYPE"   
# [67] "C_SQUARE"          "LATITUDE"          "LONGITUDE"# 


