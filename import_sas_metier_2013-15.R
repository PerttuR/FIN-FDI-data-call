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

source("db.r")

#.------------------------------------------------------------------------------
#                   0. set working directories to match folder paths        ####              
#.------------------------------------------------------------------------------
# Common paths data call folders:

run.year = 2025

# Output folder
path_der <- paste0(getwd(), .Platform$file.sep, "der/", run.year,"/")

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

# save to der folder
# saveRDS(metier_2013_15, file = paste0(path_der,"metier_2013_15.rds"))

#.------------------------------------------------------------------------------
#                   3. correct vessel lengths                               ####              
#.------------------------------------------------------------------------------

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

# save to der folder
# saveRDS(metier_2013_15, file = paste0(path_der,"metier_2013_15.rds"))

#.------------------------------------------------------------------------------
#                   4. metier lookup                                        ####    
#.------------------------------------------------------------------------------

metier.lookup <- read.csv(text=getURL("https://raw.githubusercontent.com/ices-eg/RCGs/refs/heads/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv"))

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
invisible(write.dbTable(dcprodschema, "fin_metier_DC2024", metier.lookup.fin, overwrite = FALSE)) # !!! PETRI TO ADD ####

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
# saveRDS(metier_2013_15, file = paste0(path_der,"metier_2013_15.rds"))

#.------------------------------------------------------------------------------
#                   5. mesh size lookup                                     ####    
#.------------------------------------------------------------------------------

# metier_2013_15 <- readRDS(paste0(path_der,"metier_2013_15.rds"))

metier_2013_15 <- metier_2013_15 |> mutate(METIER4 = stringr::str_sub(METIER5, 1,3))

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
                          TO   = c(16,32,90,110,157,Inf,16,32,90,105,110,Inf),
                          CODE = c("00D16", "16D32", "32D90", "90D110", "110D157", "157DXX",
                                   "00D16", "16D32", "32D90", "90D105", "105D110", "110DXX"))

mesh.sizes |> flextable() |> autofit() |> hline(i=6) |> set_caption("Mesh size ranges for Baltic")

mesh.lookup <- active.passive |> filter(MESH != "NO") |> left_join(mesh.sizes, by=c("TYPE"="TYPE"))

mesh.lookup |> flextable() |> autofit() |> set_caption("all possible mesh size combinations")

# save to DB
invisible(write.dbTable(dcprodschema, "mesh_sizes_DC2024", mesh.lookup, overwrite = FALSE)) # !!! PETRI TO ADD ####

# checking for LINE metiers with mesh sizes
metier_2013_15 |> filter(METIER4 %in% c("LLD","LLS") & !is.na(SILMAKOKO)) |> 
  select(YEAR, alus, alusnimi, SILMAKOKO, metier, METIER6) |> 
  flextable() |> autofit() |> set_caption("Enties for lines but giving mesh size")

# correct data - remove mesh size if line metier
metier_2013_15 <- metier_2013_15 |> mutate(SILMAKOKO = case_when(
  METIER4 %in% c("LLD","LLS") ~ NA,
  .default = as.numeric(SILMAKOKO)
))

# check missing mesh sizes in SAS data
metier_2013_15 |> filter(!METIER4 %in% c("LLD","LLS")) |>
  select(YEAR, PVM, alus, alusnimi, SILMAKOKO, METIER6, METIER5, METIER4) |> 
  left_join(mesh.lookup, join_by(METIER4, between(SILMAKOKO,FROM,TO))) |> 
  filter(is.na(CODE)) |>
  distinct() |> select(-c(METIER5,METIER4,TYPE,MESH,GEARS,TO,FROM)) |> 
  arrange(alus,YEAR,PVM, METIER6) |>
  flextable() |> autofit() |> set_caption("ships with missing net sizes")

# join mesh size range codes
metier_2013_15 <- metier_2013_15 |> left_join(mesh.lookup, join_by(METIER4, between(SILMAKOKO,FROM,TO)))

# assign CODE if KOKOSILMA IS NA
metier_2013_15 <- metier_2013_15 |> mutate(CODE = case_when(
  is.na(SILMAKOKO) & METIER6 == "FPO_FWS_>0_0_0" ~ "16D32",
  is.na(SILMAKOKO) & METIER6 == "FYK_ANA_>0_0_0" ~ "32D90",
  is.na(SILMAKOKO) & METIER6 == "FYK_FWS_>0_0_0" ~ "16D32",
  is.na(SILMAKOKO) & METIER6 == "FYK_SPF_>0_0_0" ~ "16D32",
  is.na(SILMAKOKO) & METIER6 == "GNS_DEF_110-156_0_0" ~ "110D157",
  is.na(SILMAKOKO) & METIER6 == "GNS_FWS_>0_0_0" ~ "32D90",
  is.na(SILMAKOKO) & METIER6 == "OTM_FWS_>0_0_0" ~ "16D32",
  is.na(SILMAKOKO) & METIER6 == "PTM_SPF_16-31_0_0" ~ "16D32",
  .default = as.character(CODE)
))

# FPO_FWS_>0_0_0      --> 16D32 # DONE
# FYK_ANA_>0_0_0      --> 32D90 # DONE
# FYK_FWS_>0_0_0      --> 16D32 # DONE
# FYK_SPF_>0_0_0      --> 16D32 # DONE
# GNS_DEF_110-156_0_0 --> 110D157 # DONE
# GNS_FWS_>0_0_0      --> 32D90 # DONE
# OTM_FWS_>0_0_0      --> 16D32 # DONE
# PTM_SPF_16-31_0_0   --> 16D32 # DONE

# check missing mesh sizes in SAS data
metier_2013_15 |> filter(!METIER4 %in% c("LLD","LLS")) |>
  select(YEAR, alus, alusnimi, SILMAKOKO, METIER6, CODE) |> 
  filter(is.na(SILMAKOKO)) |> 
  arrange(alus,YEAR,METIER6) |>
  flextable() |> autofit() |> set_caption("ships with missing net sizes")

# save to der folder
# saveRDS(metier_2013_15, file = paste0(path_der,"metier_2013_15.rds"))

rm(mesh.sizes, mesh.lookup, active.passive)
invisible(gc())

#.------------------------------------------------------------------------------
#                   6. ADD ICES AREAS                                       ####    
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
plot(icesRectangle["statistical_area_name"])

# fix ices full area codes for region 28
icesRectangle <- icesRectangle |> mutate(area_code_full = case_when(
  ices_name %in% c("45H2","45H3","45H4","44H2","44H3","44H4","43H2","43H3","43H4","42H3") ~ "27.3.d.28.1",
  rdb_area_code == "27.3.d.28" & !ices_name %in% c("45H2","45H3","45H4","44H2","44H3","44H4","43H2","43H3","43H4","42H3") ~ "27.3.d.28.2",
  .default = as.character(rdb_area_code)
))

icesRectangle$rktl_name <- as.numeric(icesRectangle$rktl_name)

# test full area
plot(icesRectangle["area_code_full"])

# join to sas data
metier_2013_15 <- metier_2013_15 |> 
  left_join(icesRectangle |> 
              select(rktl_name, area_code_full) |> st_drop_geometry(), 
            by=c("ruutu"="rktl_name"))

# save to der folder
# saveRDS(metier_2013_15, file = paste0(path_der,"metier_2013_15.rds"))

#.------------------------------------------------------------------------------
#                   7. fish column names                                   ####    
#.------------------------------------------------------------------------------

# https://www.fao.org/fishery/en/collection/asfis
# lookup table for Finnish fish names

