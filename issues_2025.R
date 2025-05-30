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
            mutate(DIFF = abs(TOTFISHDAYS.G-TOTFISHDAYS.I),
                                               DIFF_PCT = round(DIFF/if_else(TOTFISHDAYS.G>TOTFISHDAYS.I, TOTFISHDAYS.G, TOTFISHDAYS.I)*100,3))

join.IG |> flextable() |> set_caption("Difference I/G") |>
  highlight(i=which(join.IG$DIFF_PCT >2), color="yellow") |>
  hline(i=dim(join.IG)[1]-1, border=fp_border(width=1.5))


# ISSUE 2 ####