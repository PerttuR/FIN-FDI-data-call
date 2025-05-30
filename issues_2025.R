# ISSUE 1 ####

library(tidyverse)
library(flextable)
library(janitor)
library(officer)

G_table <- read.csv("C:/Users/03269737/OneDrive - Valtion/Projects/FIN-FDI-data-call/orig/G_table_2013_2022_SAS.csv")
I_table <- read.csv("C:/Users/03269737/OneDrive - Valtion/Projects/FIN-FDI-data-call/orig/I_table_2013_2022_SAS.csv")


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

# check quarters

quarter.G <- G_table |> filter(YEAR == 2013) |> 
  filter(METIER %in% c("FYK_FWS_>0_0_0","GNS_FWS_>0_0_0","GNS_SPF_16-109_0_0","LLS_FWS_0_0_0","NK")) |>
  select(QUARTER, VESSEL_LENGTH, GEAR_TYPE, METIER, TOTFISHDAYS) |>
  mutate(TOTFISHDAYS.G = TOTFISHDAYS,
         GEAR_TYPE.G = GEAR_TYPE) |>
  select(-TOTFISHDAYS, -GEAR_TYPE) |>
  adorn_totals("row")

quarter.I <- I_table |> filter(YEAR == 2013) |> 
  filter(METIER %in% c("FYK_FWS_>0_0_0","GNS_FWS_>0_0_0","GNS_SPF_16-109_0_0","LLS_FWS_0_0_0","NK")) |>
  select(QUARTER, VESSEL_LENGTH, GEAR_TYPE, METIER, TOTFISHDAYS) |>
  mutate(TOTFISHDAYS.I = TOTFISHDAYS,
         GEAR_TYPE.I = GEAR_TYPE) |>
  select(-TOTFISHDAYS, -GEAR_TYPE) |>
  adorn_totals("row")

quarter.G |> full_join(quarter.I) |> 
  mutate(DIFF = TOTFISHDAYS.I-TOTFISHDAYS.G,
         DIFF_PCT = round(DIFF/if_else(TOTFISHDAYS.I>TOTFISHDAYS.G, TOTFISHDAYS.I, TOTFISHDAYS.G)*100,3)) |>
  filter(DIFF >0) |>
  View()

# ISSUE 2 ####


