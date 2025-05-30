# ISSUE 1 ####

library(tidyverse)
library(flextable)
library(janitor)
library(officer)
library(openxlsx)

G_table <- read.csv("C:/Users/03269737/OneDrive - Valtion/Projects/FIN-FDI-data-call/orig/G_table_2013_2022_SAS.csv")
I_table <- read.csv("C:/Users/03269737/OneDrive - Valtion/Projects/FIN-FDI-data-call/orig/I_table_2013_2022_SAS.csv")

## check metiers where I bigger G ####
sum.G <- G_table |> filter(YEAR == 2013) |> 
  group_by(METIER) |> 
  summarise(TOTFISHDAYS.G = sum(TOTFISHDAYS, na.rm=TRUE)) |> 
  adorn_totals("row")

sum.G |>
  flextable() |> 
  set_caption("table_G") |> 
  hline(i=dim(sum.G)[1]-1, border=fp_border(width=1.5))
  

sum.I <- I_table |> filter(YEAR == 2013) |> 
  group_by(METIER) |> 
  summarise(TOTFISHDAYS.I = sum(TOTFISHDAYS, na.rm=TRUE)) |> 
  adorn_totals("row")

sum.I |>
  flextable() |> 
  set_caption("table_I") |> 
  hline(i=dim(sum.G)[1]-1, border=fp_border(width=1.5))


join.IG <- sum.G |> full_join(sum.I) |> 
            mutate(DIFF = TOTFISHDAYS.I-TOTFISHDAYS.G,
            DIFF_PCT = round(DIFF/if_else(TOTFISHDAYS.I>TOTFISHDAYS.G, TOTFISHDAYS.I, TOTFISHDAYS.G)*100,3))

join.IG |> flextable() |> set_caption("Difference I/G") |>
  highlight(i=which(join.IG$DIFF >0), color="yellow") |>
  hline(i=dim(join.IG)[1]-1, border=fp_border(width=1.5))

## check individual records ####

recs.G <- G_table |> filter(YEAR == 2013) |> 
  filter(METIER %in% c("FYK_FWS_>0_0_0","GNS_FWS_>0_0_0","GNS_SPF_16-109_0_0","LLS_FWS_0_0_0","NK")) |>
  select(-c(COUNTRY, YEAR, SUPRA_REGION, EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP, 
            TOTSEADAYS, TOTKWDAYSATSEA, TOTGTDAYSATSEA, TOTKWFISHDAYS, TOTGTFISHDAYS, HRSEA,
            KWHRSEA, GTHRSEA, TOTVES)) |>
  rename_at(vars(-QUARTER, -VESSEL_LENGTH, -FISHING_TECH, -GEAR_TYPE, -TARGET_ASSEMBLAGE, -MESH_SIZE_RANGE, 
                 -METIER, -SUB_REGION, -CONFIDENTIAL), ~ paste0(., '.G')) |> 
  adorn_totals("row")

recs.I <- I_table |> filter(YEAR == 2013) |> 
  filter(METIER %in% c("FYK_FWS_>0_0_0","GNS_FWS_>0_0_0","GNS_SPF_16-109_0_0","LLS_FWS_0_0_0","NK")) |>
  select(-c(COUNTRY, YEAR, SUPRA_REGION, EEZ_INDICATOR, GEO_INDICATOR, SPECON_TECH, DEEP, RECTANGLE)) |>
  rename_at(vars(-QUARTER, -VESSEL_LENGTH, -FISHING_TECH, -GEAR_TYPE, -TARGET_ASSEMBLAGE, -MESH_SIZE_RANGE, 
                 -METIER, -SUB_REGION, -CONFIDENTIAL), ~ paste0(., '.I')) |>
  group_by(QUARTER, VESSEL_LENGTH, FISHING_TECH, GEAR_TYPE, TARGET_ASSEMBLAGE, MESH_SIZE_RANGE, 
           METIER, SUB_REGION, CONFIDENTIAL) |>
  summarise(TOTFISHDAYS.I = sum(TOTFISHDAYS.I, na.rm=TRUE)) |>
  adorn_totals("row")

comp.IG <- recs.G |> full_join(recs.I, by=c("QUARTER", "VESSEL_LENGTH", "FISHING_TECH", "GEAR_TYPE", "TARGET_ASSEMBLAGE",
                                 "MESH_SIZE_RANGE", "METIER", "SUB_REGION", "CONFIDENTIAL")) |> 
  mutate(DIFF = TOTFISHDAYS.I-TOTFISHDAYS.G,
         DIFF_PCT = round(DIFF/if_else(TOTFISHDAYS.I>TOTFISHDAYS.G, TOTFISHDAYS.I, TOTFISHDAYS.G)*100,3))

write.xlsx(comp.IG, file="orig/25.issue.1.xlsx")

comp.IG |> filter(DIFF > 0) |> flextable()

# ISSUE 2 ####


