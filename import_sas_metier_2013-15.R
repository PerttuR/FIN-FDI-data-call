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

dim(metier_2013_15)

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
            by= c("KALASTUSVUOSI"="VUOSI","alus"="ULKOINENTUNNUS","alusnimi"="NIMI")) |>
  mutate(DIFF = if_else(vessel_length != VLENGTH_FDI, 1, 0),
         VESSEL_LENGTH = case_when(
           is.na(vessel_length) ~ VLENGTH_FDI,
           vessel_length != VLENGTH_FDI ~ VLENGTH_FDI,
           .default = vessel_length
         )) |> distinct()

# check 2014 GOLDEN ROSE - should now be NA, but vessel_length is correct

# link ship length lookup to table
metier_2013_15 <- metier_2013_15 |>
                    left_join(ship.length |> select(KALASTUSVUOSI, alus, alusnimi, VESSEL_LENGTH), 
                              by=c("KALASTUSVUOSI"="KALASTUSVUOSI",
                                   "alus"="alus",
                                   "alusnimi"="alusnimi")) |>
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
                .default = as.character(Used_by_country_in_RDB))) |> 
              select(RCG,Metier_level6,Old_code,Used_by_country_in_RDB,Metier_level5,Description)

metier.lookup.fin <- metier.lookup |> filter(grepl("FIN",Used_by_country_in_RDB))

# metier class PTM_SPF is missing
# reported issue on Github https://github.com/ices-eg/RCGs/issues/184
# metier.lookup.fin |> select(RCG, Metier_level5, Metier_level6, Used_by_country_in_RDB) |> 
#  kable(caption="Official metier classes for Finland")

metier_check <- metier_2013_15 |> 
  select(metier) |> distinct() |> 
  left_join(metier.lookup |> filter(RCG == "BALT"), by = c("metier" = "Metier_level6"), keep = T) |>
  filter(is.na(Metier_level6))

metier_check |> flextable()

# save to DB
invisible(write.dbTable(dcprodschema, "fin_metier_DC2024", metier.lookup.fin, overwrite = FALSE)) # !!! PETRI TO ADD ####

# overwrite MISSING metier for tag for one single vessel with one missing metier
# replace old codes with new ones

metier_2013_15 <- metier_2013_15 |> 
  mutate(metier = case_when(
    metier == "MISSING" ~ "GNS_FWS_>0_0_0",
    metier == "OTM_DEF_>=105_1_120" ~ "OTM_DEF_105-115_1_120",
    metier == "OTM_SPF_16-104_0_0" ~ "OTM_SPF_16-31_0_0",
    metier == "GNS_SPF_16-109" ~ "GNS_SPF_32-89_0_0",
    metier == "PTM_SPF_16-104_0_0" ~ "PTM_SPF_16-31_0_0",
    metier == "GNS_SPF_16-109_0_0" ~ "GNS_SPF_32-89_0_0",
    metier == "OTB_DEF_>=105_1_120" ~ "OTB_DEF_105-115_1_120",
    TRUE ~ metier))    

# compare metier classes
metier_2013_15 |> select(YEAR, QUARTER, alus, alusnimi, metier) |> 
  mutate(METIER5 = stringr::str_sub(metier, 1,7))  |> 
  left_join(metier.lookup.fin, by=c("METIER5"="Metier_level5")) |> 
  mutate(check = if_else(metier != Metier_level6, 1,0)) |> 
  #filter(check == 1 | is.na(check)) |>
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

# save to der folder
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
  left_join(mesh.lookup, join_by(METIER4, between(SILMAKOKO,TO,FROM))) |> 
  filter(is.na(CODE)) |>
  distinct() |> select(-c(METIER5,METIER4,TYPE,MESH,GEARS,TO,FROM)) |> 
  arrange(alus,YEAR,PVM, METIER6) |>
  flextable() |> autofit() |> set_caption("ships with missing net sizes")

# join mesh size range codes
metier_2013_15 <- metier_2013_15 |> left_join(mesh.lookup, join_by(METIER4, between(SILMAKOKO,TO,FROM)))

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
  flextable() |> autofit() |> set_caption("imputed codes for ships with missing net sizes")

# save to der folder
# saveRDS(metier_2013_15, file = paste0(path_der,"metier_2013_15.rds"))

rm(mesh.sizes, mesh.lookup, active.passive, metier_check, metier.lookup, metier.lookup.fin)
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
              select(rktl_name, ices_name, area_code_full) |> st_drop_geometry(), 
            by=c("ruutu"="rktl_name"))

# testing
metier_2013_15 |> count(area_code_full,ices_name) |> flextable()

# save to der folder
# saveRDS(metier_2013_15, file = paste0(path_der,"metier_2013_15.rds"))

rm(icesRectangle,areas,ewkb_to_sf)
invisible(gc())

#.------------------------------------------------------------------------------
#                   7. longitude / latitude                                 ####    
#.------------------------------------------------------------------------------

# metier_2013_15 <- readRDS(paste0(path_der,"metier_2013_15.rds"))

source("spatial.R")

midpoints <- latlon(metier_2013_15$ices_name, midpoint=TRUE)

metier_2013_15 <- tibble::rowid_to_column(metier_2013_15, "ID")
midpoints <- tibble::rowid_to_column(midpoints, "ID")

metier_2013_15 <- left_join(metier_2013_15, midpoints, copy = TRUE)

metier_2013_15 <- metier_2013_15 |> rename(LATITUDE = SI_LATI, LONGITUDE = SI_LONG) |> 
  select(-ID)

# save to der folder
# saveRDS(metier_2013_15, file = paste0(path_der,"metier_2013_15.rds"))

rm(midpoints)
invisible(gc())

#.------------------------------------------------------------------------------
#                   8. fish column names                                   ####    
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

metier_2013_15 <- metier_2013_15 |> mutate(
                        SVT_KG_COD = if_else(is.na(Turska),0,Turska),
                        SVT_KG_ELE = if_else(is.na(Ankerias),0,Ankerias),   
                        SVT_KG_FBM = if_else(is.na(Lahna),0,Lahna),
                        SVT_KG_FBU = if_else(is.na(Made),0,Made),
                        SVT_KG_FID = if_else(is.na(Sayne),0,Sayne),
                        SVT_KG_FIN = if_else(is.na(muu_kala),0,muu_kala),
                        SVT_KG_FLE = if_else(is.na(Kampela),0,Kampela),
                        SVT_KG_FPE = if_else(is.na(Ahven),0,Ahven),
                        SVT_KG_FPI = if_else(is.na(Hauki),0,Hauki),
                        SVT_KG_FPP = if_else(is.na(Kuha),0,Kuha),
                        SVT_KG_FRO = if_else(is.na(Sarki),0,Sarki),
                        SVT_KG_FVE = if_else(is.na(Muikku),0,Muikku),
                        SVT_KG_HER = if_else(is.na(Silakka),0,Silakka),
                        SVT_KG_PLE = if_else(is.na(Punakampela),0,Punakampela),
                        SVT_KG_PLN = if_else(is.na(Siika),0,Siika),
                        SVT_KG_SAL = if_else(is.na(Lohi_kg),0,Lohi_kg),
                        SVT_KG_SME = if_else(is.na(Kuore),0,Kuore),
                        SVT_KG_SPR = if_else(is.na(Kilohail),0,Kilohail),
                        SVT_KG_TRR = if_else(is.na(kirjoloh),0,kirjoloh),
                        SVT_KG_TRS = if_else(is.na(Taimen),0,Taimen),
                        SVT_KG_TUR = if_else(is.na(Pkamp),0,Pkamp),
                        SVT_KG_WHG = if_else(is.na(Valkoturska),0,Valkoturska),
                        SVT_KG_TOTAL = SVT_KG_COD+SVT_KG_ELE+SVT_KG_FBM+SVT_KG_FBU+
                          SVT_KG_FID+SVT_KG_FIN+SVT_KG_FLE+SVT_KG_FPE+SVT_KG_FPI+
                          SVT_KG_FPP+SVT_KG_FRO+SVT_KG_FVE+SVT_KG_HER+SVT_KG_PLE+
                          SVT_KG_PLN+SVT_KG_SAL+SVT_KG_SME+SVT_KG_SPR+SVT_KG_TRR+
                          SVT_KG_TRS+SVT_KG_TUR+SVT_KG_WHG)

# dim(metier_2013_15)

# wide to long table
metier_2013_15 <- tibble::rowid_to_column(metier_2013_15, "ID")

metier.fish <- metier_2013_15 |> select(ID, YEAR, ices, SVT_KG_COD:SVT_KG_WHG) |>
                rename_with(~gsub("SVT_KG_", "", .x)) |>
                pivot_longer(!ID:ices, names_to = "SPECIES", values_to = "KG")

# dim(metier.fish)

# get commercial value from DCPROD lookup table
commercial.value <- read.dbTable(schema=paste("2025-04-10", "-dcprod", sep = ""), 
                            table="keskihinnat_2025_05_06", dbname = "kake_siirto")

commercial.value <- commercial.value |> select(FAO_KOODI,VUOSI,ICES_ALUE,KESKIHINTA,KESKIHINTA_KOKO_MERIALUE) |>
                      filter(FAO_KOODI %in% c("COD", "ELE", "FBM", "FBU", "FID", "FIN", "FLE", "FPE",
                                              "FPI", "FPP", "FRO", "FVE", "HER", "PLE", "PLN", "SAL",
                                              "SME", "SPR", "TRR", "TRS", "TUR", "WHG")) |> 
                      arrange(FAO_KOODI,VUOSI,ICES_ALUE)

# create lookup of all combination years, ices, fish
# outer join to commericial.value
# fill in empty rows
# for eel use €10/kg as expert judgment for all years

# check
commercial.value |> filter(is.na(KESKIHINTA) & is.na(KESKIHINTA_KOKO_MERIALUE)) |> View()
commercial.value |> filter(FAO_KOODI == "ELE") |> fill(VUOSI,ICES_ALUE) |> View()

# join area specific price
metier.fish <- metier.fish |> left_join(commercial.value,  
                            by=c("YEAR"="VUOSI", "ices"="ICES_ALUE", "SPECIES"="FAO_KOODI"))

# calculate value
metier.fish <- metier.fish |> mutate(VALUE = case_when(
          !is.na(KESKIHINTA) ~ KG*KESKIHINTA,
          is.na(KESKIHINTA) ~ KG*KESKIHINTA_KOKO_MERIALUE,
          is.na(KESKIHINTA) & is.na(KESKIHINTA_KOKO_MERIALUE) ~ NA,
          TRUE ~ -999
))

# any weird ones?
# metier.fish |> filter(VALUE == -999) |> count(YEAR,SPECIES)

metier.fish |> filter(is.na(VALUE)) |> count(YEAR,SPECIES) |>
  adorn_totals("row") |> flextable() |> 
  set_caption("fish species without commercial value in certain year")

tag <- metier.fish |> filter(is.na(VALUE)) |> count(YEAR,SPECIES)

test <- commercial.value |> filter(VUOSI %in% c(min(tag$YEAR)-1, unique(tag$YEAR), max(tag$YEAR)+1) &
                        FAO_KOODI %in% tag$SPECIES) |> arrange(FAO_KOODI,VUOSI, ICES_ALUE)
  
  
# save to der folder
# saveRDS(metier_2013_15, file = paste0(path_der,"metier_2013_15.rds"))

#.------------------------------------------------------------------------------
#                   9. calculate fishing days                               ####    
#.------------------------------------------------------------------------------

# proc sort data=effort out=alus_saalis_sort;
# by alus pvmday descending saalis_yht;
# run;
# 
# data alus_saalis;
# set alus_saalis_sort;
# by alus pvmday;
# 
# if first.pvmday;
# run;
# 
# 
# PROC SUMMARY missing nway data=alus_saalis;
# class alus quarter length ft gear metier level5 ices specon_tech meshsizerange;
# var pyyntipv;
# output out=paivat2 sum=TOTFISHDAYS;
# run;


#.------------------------------------------------------------------------------
#                   10. calculate sea days                               ####    
#.------------------------------------------------------------------------------

# DATA meripv;
# set alus_saalis;
# meripaiva=(paluupvm-lahtopvm)/86400;
# run;
# 
# 
# DATA meripv;
# set meripv;
# if meripaiva=0 then do;
# meripaiva=1;
# end;
# keep alus lahtopvm paluupvm quarter length ft gear metier level5 ices specon_tech meshsizerange meripaiva;
# 
# PROC SORT nodupkey data=meripv out=meripaivat;
# by alus lahtopvm paluupvm quarter length ft gear metier level5 ices specon_tech meshsizerange meripaiva;
# run;
# 
# PROC SUMMARY missing nway data=meripaivat;
# class alus quarter length ft gear metier level5 ices specon_tech meshsizerange;
# var meripaiva;
# output out=meripaivat2 sum=TOTSEADAYS;
# run;


#.------------------------------------------------------------------------------
#                   11. matching akt1 column names                          ####    
#.------------------------------------------------------------------------------

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


