# import SAS metier tables for 2013 to 2015
# Authors: J. Demmler, Perttu
# Date: 10/06/2025
# Last revision: 12/06/2025

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

for (i in 2013:2015){
  
  tmp <- read_sas(paste0("G:/Luke2/Stat_kala_merikalastus/Metier/Data/pvkarvo",i,"_metier.sas7bdat")) |>
                  mutate(KALASTUSVUOSI = i)
  
  assign(paste0("metier_",i), tmp)
  
} 


## alternative path
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

#import species lookup table ASFIS_sp_2024.xlsx

species_lookup <- read.xlsx("orig/ASFIS_sp_2024.xlsx", sheet = "ASFIS_sp")
head(species_lookup)

# save it as .rds
saveRDS(species_lookup, file = paste0(path_der,"species_lookup.rds"))

source("db.r")

# ... time stamp to latest Logbook DB source: YYYY-MM-DD
table.list <- list.dbTable("kake_siirto")[,1] |> as.character(table)
table.list <- substr(table.list, 30, nchar(table.list)-3)
table.dates <- sort(unique(substr(table.list,1,10)))
# only keep dates
table.dates <- grep("\\d{4}-\\d{2}-\\d{2}", table.dates, value=TRUE)

# output choice here
schemadate <- max(table.dates)
message("Newest schema is from: ", schemadate)

# Write combined metier datafiles 2013-2015 output data to Luke LOGBOOK database
dcprodschema <- paste0(schemadate, "-dcprod")
invisible(write.dbTable(dcprodschema, "metier_sas_files_2013_15", metier_2013_15, overwrite = TRUE))

# Write species lookup table to Luke LOGBOOK database
dcprodschema <- paste0(schemadate, "-dcprod")
invisible(write.dbTable(dbname = "kake_siirto", dcprodschema, "species_lookup", species_lookup, overwrite = TRUE))


#.------------------------------------------------------------------------------
#                   2. add vars to 2013.-2015 metier table                  ####              
#.------------------------------------------------------------------------------

# metier_2013_15 <- readRDS(paste0(path_der,"metier_2013_15.rds"))

metier_2013_15 <- metier_2013_15 |> 
                  mutate(
                    COUNTRY = "FIN",
                    YEAR	= KALASTUSVUOSI,
                    QUARTER	= case_when(
                      kk %in% seq(1,3) ~ "1",
                      kk %in% seq(4,6) ~ "2",
                      kk %in% seq(7,9) ~ "3",
                      kk %in% seq(10,12) ~ "4"),
                    # VESSEL_LENGTH HERE!
                    FISHING_TECH =	ft,
                    GEAR_TYPE	= stringr::str_sub(metier, 1,3), 
                    TARGET_ASSEMBLAGE	= stringr::str_sub(metier, 5,7), 
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

# check new columns
# View(metier_2013_15[,130:152])

#.------------------------------------------------------------------------------
#                   3. correct vessel lengths                               ####              
#.------------------------------------------------------------------------------

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

kapasiteetti <- read.dbTable(schema=paste(schemadate, "-dcprod", sep = ""), 
                             table=tablename, dbname = "kake_siirto")

ship.length <- metier_2013_15 |> select(KALASTUSVUOSI, alus, alusnimi, vessel_length) |> distinct() |>
  left_join(kapasiteetti |> select(VUOSI, ULKOINENTUNNUS, NIMI, VLENGTH_FDI), 
            by= c("KALASTUSVUOSI"="VUOSI","alus"="ULKOINENTUNNUS")) |>
  mutate(DIFF = if_else(vessel_length != VLENGTH_FDI, 1, 0),
         VESSEL_LENGTH = case_when(
           is.na(vessel_length) ~ VLENGTH_FDI,
           vessel_length != VLENGTH_FDI ~ VLENGTH_FDI,
           .default = vessel_length
         )) 

# check 2014 GOLDEN ROSE - should now be NA, but vessel_length is correct

# link ship length lookup to table
metier_2013_15 <- metier_2013_15 |>
                    left_join(ship.length |> select(KALASTUSVUOSI, alus, VESSEL_LENGTH), 
                              by=c("KALASTUSVUOSI"="KALASTUSVUOSI",
                                   "alus"="alus")) |>
                    relocate(VESSEL_LENGTH, .after = QUARTER)

# checking records
metier_2013_15 |> count(VESSEL_LENGTH) |> flextable() |> 
  set_caption("Check VESSEL_LENGTH classes in metier_2013_15")
# metier_2013_15 |> select(c(1,2,130:152)) |> View()

rm(ship.length, kapasiteetti)
invisible(gc())


#.------------------------------------------------------------------------------
#                   4. metier lookup                                        ####    
#.------------------------------------------------------------------------------

metier.lookup <- read.csv(text=getURL("https://raw.githubusercontent.com/ices-eg/RCGs/refs/heads/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv"))


metier.lookup[metier.lookup$Metier_level6 == "PTM_SPF_16-31_0_0" & metier.lookup$RCG == "BALT",] <- 

# correct missing FIN tag in lookup table
metier.lookup <- metier.lookup |> 
              mutate(Used_by_country_in_RDB = case_when(
                Metier_level6 == "PTM_SPF_16-31_0_0" & RCG == "BALT" ~ paste0(Used_by_country_in_RDB,", FIN"),
                .default = as.character(Used_by_country_in_RDB)))

metier.lookup.fin <- metier.lookup |> filter(grepl("FIN", Used_by_country_in_RDB)) |>
                       select(!X:X.17)

# metier class PTM_SPF is missing
# reported issue on Github https://github.com/ices-eg/RCGs/issues/184
# metier.lookup.fin |> select(RCG, Metier_level5, Metier_level6, Used_by_country_in_RDB) |> 
#  kable(caption="Official metier classes for Finland")

# save to DB
invisible(write.dbTable(dcprodschema, "fin_metier_DC2024", metier.lookup.fin, overwrite = FALSE))

# overwrite MISSING metier for tag for one single vessel with one missing metier
metier_2013_15 <- metier_2013_15 |> 
  mutate(metier = if_else(metier == "MISSING", "GNS_FWS_>0_0_0", metier))

# compare metier classes
metier_2013_15 |> select(YEAR, QUARTER, alus, alusnimi, metier) |> 
  mutate(METIER5 = stringr::str_sub(metier, 1,7))  |> 
  left_join(metier.lookup.fin, by=c("METIER5"="Metier_level5")) |> 
  mutate(check = if_else(metier != Metier_level6, 1,0)) |> 
  filter(check == 1 | is.na(check)) |>
  select(metier, Metier_level6) |> distinct() |> 
  flextable() |> autofit() |>
  set_caption("Differences between SAS METIER and official metier level 6 names") |>
  add_footer_lines("*`PTM_SPF_16-31_0_0` is missing in the tags for Finland")


# join against lookup to produce METIER6
metier_2013_15 <- metier_2013_15 |> mutate(METIER5 = stringr::str_sub(metier, 1,7)) |>
  left_join(metier.lookup.fin |> select(Metier_level5, Metier_level6), 
            by=c("METIER5"="Metier_level5")) |> 
  rename(METIER6 = Metier_level6) |>
  relocate(METIER6, .after = TARGET_ASSEMBLAGE)

metier_2013_15 |> count(METIER6) |> flextable() |> autofit() |>
  set_caption("Metier 6 classes in metier_2013_15")

#save to der folder
saveRDS(metier_2013_15, file = paste0(path_der,"metier_2013_15.rds"))

#.------------------------------------------------------------------------------
#                   5. mesh size lookup                                     ####    
#.------------------------------------------------------------------------------

# metier_2013_15 <- readRDS(paste0(path_der,"metier_2013_15.rds"))

metier_2013_15 <- metier_2013_15 |> mutate(METIER4 = stringr::str_sub(METIER5, 1,3))

active.passive <- data.frame(TYPE = c(rep("passive",5), rep("active", 4)),
           METIER4 = c("FPO", "FYK", "GNS", "LLD", "LLS", "MIS", "OTB", "OTM", "PTM"))

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
                          TO   = c( 0,16,32,90,110,157, 0,16,32,90,105,110),
                          FROM = c(16,32,90,110,157,Inf,16,32,90,105,110,Inf),
                          CODE = c("00D16", "16D32", "32D90", "90D110", "110D157", "157DXX",
                                   "00D16", "16D32", "32D90", "90D105", "105D110", "110DXX"))

mesh.sizes |> flextable() |> autofit() |> hline(i=6) |> set_caption("Mesh size ranges for Baltic")

mesh.lookup <- active.passive |> left_join(mesh.sizes, by="TYPE")

mesh.lookup |> flextable() |> autofit() |> set_caption("all possible mesh size combinations")

# save to DB
invisible(write.dbTable(dcprodschema, "mesh_sizes_DC2024", mesh.lookup, overwrite = FALSE))

# 4. mesh size - can be calculated from SILMAKOKKO, but check against METIER

metier_2013_15 |> select(SILMAKOKO, METIER6, METIER5, METIER4) |> 
  left_join(mesh.lookup, join_by(METIER4, between(SILMAKOKO,TO,FROM))) |> 
  distinct() |> View()


#.------------------------------------------------------------------------------
#                   6. fish column names                                   ####    
#.------------------------------------------------------------------------------

# https://www.fao.org/fishery/en/collection/asfis
# lookup table for Finnish fish names














#.------------------------------------------------------------------------------
#                   XX. TO DO                                                ####    
#.------------------------------------------------------------------------------


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

icesRectangle <- st_as_sf(icesRectangle) |> st_transform(epsg = 4326)
ices.regions <- st_transform(ices.regions, epsg = 4326)

sf_use_s2(FALSE)

test <- ices.regions |> st_join(icesRectangle, join=st_intersects)
