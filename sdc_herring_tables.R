# baltic summaries of capacity tables
# https://stecf.ec.europa.eu/data-dissemination/fdi_en

library(tidyverse)
library(flextable)
# devtools::install_github("adletaw/captioner")
library(captioner)
library(officer)

table_nums <- captioner(prefix = "Table")

capacity <- read_csv("orig/FDI Capacity by country.csv")

capacity |> count(Country) |> print(n=Inf)
  
baltic <- c("Denmark", "Estonia", "Finland", "Germany", "Latvia", "Lithuania", "Sweden")

capacity <- capacity |> filter(Country %in% baltic)

# number of vessels by vessel length category for each Baltic country ####
table_nums("num_vessels", "Number of vessels by vessel length category for each Baltic country.")

ftab <- capacity |> filter(`Vessel Length Category` != "NK") |>
  select(Country, `Vessel Length Category`, `Total vessels` ) |>
  group_by(Country, `Vessel Length Category`) |> 
  summarise(sum.vessels = sum(`Total vessels`, na.rm=TRUE)) |>
  pivot_wider(names_from = `Vessel Length Category`, values_from = sum.vessels) |> 
  flextable() |> autofit() |>
  set_caption(as_paragraph(
    as_chunk(table_nums("num_vessels"), props = fp_text_default(font.family = "Arial", bold=TRUE))
  ), word_stylename = "Table Caption")
    
print(ftab, preview = "docx")   

# avg age of vessels by vessel length category for each Baltic country ####
table_nums("age_vessels", "Average age of vessels by vessel length category for each Baltic country.")

capacity |> filter(`Vessel Length Category` != "NK") |>
  select(Country, `Vessel Length Category`, `Average age` ) |>
  group_by(Country, `Vessel Length Category`) |> 
  summarise(mean.age = mean(`Average age`, na.rm=TRUE)) |>
  pivot_wider(names_from = `Vessel Length Category`, values_from = mean.age) |> 
  flextable() |> 
  set_caption(table_nums("age_vessels"))

# avg length of vessels by vessel length category for each Baltic country ####
table_nums("length_vessels", "Average length of vessels by vessel length category for each Baltic country.")

capacity |> filter(`Vessel Length Category` != "NK") |>
  select(Country, `Vessel Length Category`, `Average length` ) |>
  group_by(Country, `Vessel Length Category`) |> 
  summarise(mean.length = mean(`Average length`, na.rm=TRUE)) |>
  pivot_wider(names_from = `Vessel Length Category`, values_from = mean.length) |> 
  flextable() |> 
  set_caption(table_nums("length_vessels"))

