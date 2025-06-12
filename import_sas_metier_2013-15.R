# import SAS metier tables for 2013 to 2015
# Authors: J. Demmler, Perttu
# Date: 10/06/2025
# Last revision: 11/06/2025

#- Clear workspace
rm(list=ls())

library(tidyverse)
library(haven)
library(openxlsx)
library(stringr)

source("db.r")

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths        ####              
#-------------------------------------------------------------------------------
# Common paths data call folders:

run.year = 2025

# Output folder
path_der <- paste0(getwd(), .Platform$file.sep, "der/", run.year,"/")

#-------------------------------------------------------------------------------
#                   1. get data from G or C drive                           ####              
#-------------------------------------------------------------------------------
# get data from G drive ####
          
for (i in 2013:2015){
  
  tmp <- read_sas(paste0("G:/Luke2/Stat_kala_merikalastus/Metier/Data/pvkarvo",i,"_metier.sas7bdat")) |>
                  mutate(KALASTUSVUOSI = i)
  
  assign(paste0("metier_",i), tmp)
  
} 


# alternative path ####
# 
# for (i in 2013:2015){
#   
#   tmp <- read_sas(paste0("orig/pvkarvo",i,"_metier.sas7bdat")) |>
#     mutate(KALASTUSVUOSI = i)
#   
#   assign(paste0("metier_",i), tmp)
#   
# } 

# combine into 1 table

metier_2013_15 <- bind_rows(metier_2013, metier_2014, metier_2015)

# filter is wanted
# metier_2013_15_FIN <- metier_2013_15 |> filter(grepl("FIN",alus))

# remove temporary files
rm(metier_2013, metier_2014, metier_2015, tmp)
invisible(gc())

#save to der folder
saveRDS(metier_2013_15, file = paste0(path_der,"metier_2013_15.rds"))

# connection and personal parameters to PG-database:
source("db.R")

# ... time stamp to latest Logbook DB source: YYYY-MM-DD
kakeTimeStamp <- "2025-04-10"

# Write combined metier datafiles 2013-2015 output data to Luke LOGBOOK database
dcprodschema <- paste0(kakeTimeStamp, "-dcprod")
invisible(write.dbTable(dcprodschema, "metier_sas_files_2013_15", metier_2013_15, overwrite = TRUE))


#-------------------------------------------------------------------------------
#                   2. add vars to 2013.-2015 metier table                  ####              
#-------------------------------------------------------------------------------

metier_2013_15 <- metier_2013_15 |> 
                  mutate(
                    COUNTRY = "FIN",
                    YEAR	= KALASTUSVUOSI,
                    QUARTER	= case_when(
                      kk %in% seq(1,3) ~ "1",
                      kk %in% seq(4,6) ~ "2",
                      kk %in% seq(7,9) ~ "3",
                      kk %in% seq(10,12) ~ "4"),
                    VESSEL_LENGTH =	vessel_length,  # 7368 unknown (both vessel_length and pituus), length: 8297 unknown
                    FISHING_TECH =	ft,
                    GEAR_TYPE	= stringr::str_sub(metier, 1,3), 
                    TARGET_ASSEMBLAGE	= stringr::str_sub(metier, 5,7), 
                    MESH_SIZE_RANGE	= "",      # METIER, SILMAKOKO
                    METIER = metier,
                    METIER_7 = "NA",
                    DOMAIN_DISCARDS = "",	     # later
                    DOMAIN_LANDINGS	= "",      # later
                    SUPRA_REGION	= "NAO",
                    SUB_REGION	= "",          # RECTANGLE - (CHECK LOOKUP)
                    EEZ_INDICATOR	= "NA",
                    GEO_INDICATOR	= "NGI",
                    NEP_SUB_REGION	= "NA",
                    SPECON_TECH	= "NA",
                    DEEP	= "NA",
                    SPECIES	= "",              # FISHNAME COL 20-41
                    TOTWGHTLANDG	= "",        # FISHNAME COL 20-41
                    TOTVALLANDG	= "",          # H+FISHNAME COL 20-41
                    DISCARDS	= "NA",
                    CONFIDENTIAL	= "")

# check new columns
# View(metier_2013_15[,130:152])

#-------------------------------------------------------------------------------
#                   3. add missing vessel lengths                           ####              
#-------------------------------------------------------------------------------

### JCD: get newest postgres schema date ####
table.list <- list.dbTable("kake_siirto")[,1] |> as.character(table)
table.list <- substr(table.list, 30, nchar(table.list)-3)
table.dates <- sort(unique(substr(table.list,1,10)))
# only keep dates
table.dates <- grep("\\d{4}-\\d{2}-\\d{2}", table.dates, value=TRUE)

# output choice here ####
schemadate <- max(table.dates)
message("Newest schema is from: ", schemadate)

# find the correct table name and date
tbl.list <- list.dbTable.tbl(dbname = "kake_siirto", schema=paste0(schemadate, "-dcprod"))
# find second table
tag <- grep("kapasiteetti", tbl.list$table)
tablename <- tbl.list$table[tag]
class(tablename) <- "character"
tablename <- unlist(strsplit(tablename, '"'))
tablename <- tablename[length(tablename)-1]

message(paste("Reading table:", tablename))

kapasiteetti <- read.dbTable(schema=paste(schemadate, "-dcprod", sep = ""), 
                             table=tablename, dbname = "kake_siirto")

ship.length <- metier_2013_15 |> select(KALASTUSVUOSI, alus, alusnimi, vessel_length) |> distinct() |>
  left_join(kapasiteetti |> select(VUOSI, ULKOINENTUNNUS, NIMI, VLENGTH_FDI), 
            by= c("KALASTUSVUOSI"="VUOSI","alus"="ULKOINENTUNNUS")) |>
  mutate(DIFF = if_else(vessel_length != VLENGTH_FDI, 1, 0))


# 2: 


# need lookup to RUUTU: ices subdivision code is not sufficient for subdivision 28
library(sf)
ices.regions <- st_read("orig/ices_grid.gpkg", layer="ices_areas")
ices.sub <- st_read("orig/ices_grid.gpkg", layer="ices_sub")

library(sf)

areas <- seq(22,32)

ewkb_to_sf <- function(data) {
  return(st_as_sfc(structure(data, class="WKB"), EWKB=T))
}

icesRectangle <- read.dbTable(schema='rek', table='ices_rectangle', dbname = "rktl")
icesRectangle <- icesRectangle[icesRectangle$statistical_area_name %in% areas,]
icesRectangle$geometry <- ewkb_to_sf(icesRectangle$geometry)

icesRectangle <- st_as_sf(icesRectangle)
ices.regions <- st_transform(ices.regions, epsg = 4326)

test <- ices.regions |> st_join(icesRectangle, join=st_intersects)



# 3. create metier lookup for Finland and correct metier if possible?

# 4. mesh size - can be calculated from SILMAKOKKO, but check against METIER
# passive gears - see annex 6 
# FPO_FWS_>0_0_0       
# FYK_ANA_>0_0_0       
# FYK_FWS_>0_0_0       
# FYK_SPF_>0_0_0       
# GNS_DEF_110-156_0_0  
# GNS_FWS_>0_0_0       
# GNS_SPF_16-109       
# GNS_SPF_16-109_0_0   
# LLD_ANA_0_0_0
#  LLS_FWS_0_0_0 
# MOBILE gears
# Diamond mesh <16 mm
# 00D16
# Diamond mesh >=16 mm and <32 mm
# 16D32
# Diamond mesh >=32 mm and <90 mm
# 32D90
# Diamond mesh >=90 mm and <105 mm
# 90D105
# Diamond mesh >=105 mm and <110 mm
# 105D110
# Diamond mesh >=110 mm
# 110DXX
# PASSIVE gears
# Diamond mesh <16 mm
# 00D16
# Diamond mesh >=16 mm and <32 mm
# 16D32
# Diamond mesh >=32 mm and <90 mm
# 32D90
# Diamond mesh >=90 mm and <110 mm
# 90D110
# Diamond mesh >=110 mm and <157 mm
# 110D157
# Diamond mesh >=157 mm
# 157DXX
# metier lookup https://github.com/ices-eg/RCGs/blob/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.xlsx      

# 5. rename fish species columns
# https://www.fao.org/fishery/en/collection/asfis
# lookup table for Finnish fish names














#First read in aktiviteetti and akt1 if not loaded:


lookup1 <- data.frame(COL_NO = seq(1:length(names(aktiviteetti))),
                     COLUMN_NAME = names(aktiviteetti),
                     DESCRIPTON = NA)

lookup2 <- data.frame(COL_NO = seq(1:length(names(akt1))),
                      COLUMN_NAME = names(akt1),
                      DESCRIPTON = NA)

lookup3 <- data.frame(COL_NO= seq(1:length(names(metier_2013_15))),
                      COLUMN_NAME = names(metier_2013_15),
                      DESCRIPTON = NA)

# save to Excel
wb <- createWorkbook()
addWorksheet(wb, sheetName = "aktiviteetti")
addWorksheet(wb, sheetName = "akt1")
addWorksheet(wb, sheetName = "SAS_metier")
writeDataTable(wb, sheet = 1, lookup1)
writeDataTable(wb, sheet = 2, lookup2)
writeDataTable(wb, sheet = 3, lookup3)
saveWorkbook(wb, "orig/lookup.xlsx")
