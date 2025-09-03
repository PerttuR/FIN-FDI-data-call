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
years <- sort(unique(capacity$Year))

# number of vessels by vessel length category for each Baltic country ####

by <- capacity |> filter(`Vessel Length Category` != "NK") |>
  select(Country, Year, `Vessel Length Category`, `Total vessels` ) |>
  group_by(Country, Year, `Vessel Length Category`) |> 
  summarise(sum.vessels = sum(`Total vessels`, na.rm=TRUE)) |>
  pivot_wider(names_from = `Vessel Length Category`, values_from = sum.vessels) |>
  ungroup() |> select(Country, Year, VL0008,VL0010,VL0812,VL1012,VL1218,VL1824,VL2440,VL40XX) |> group_split(Year)


for (i in 1:length(by)){

table_nums(paste0("num_vessels_",i), 
           paste("Number of vessels by vessel length category for each Baltic country in", years[i], "."))
  
ftab <- by[[i]] |> select(-Year) |>
  flextable() |> autofit() |>
  set_caption(as_paragraph(
    as_chunk(table_nums(paste0("num_vessels_",i)), props = fp_text_default(font.family = "Arial", bold=TRUE))
  ), word_stylename = "Table Caption")

print(ftab)   
# print(ftab, preview = "docx")   

}


# avg age of vessels by vessel length category for each Baltic country ####

by <- capacity |> filter(`Vessel Length Category` != "NK") |>
  select(Country, Year, `Vessel Length Category`, `Average age`) |>
  group_by(Country, Year, `Vessel Length Category`) |> 
  summarise(mean.age = round(mean(`Average age`, na.rm=TRUE),2)) |>
  pivot_wider(names_from = `Vessel Length Category`, values_from = mean.age) |>
  ungroup() |> select(Country, Year, VL0008,VL0010,VL0812,VL1012,VL1218,VL1824,VL2440,VL40XX) |> group_split(Year)


for (i in 1:length(by)){
  
  table_nums(paste0("age_vessels_",i), 
             paste("Average age of vessels by vessel length category for each Baltic country in", years[i], "."))
  
  ftab <- by[[i]] |> select(-Year) |>
    flextable() |> autofit() |>
    set_caption(as_paragraph(
      as_chunk(table_nums(paste0("age_vessels_",i)), props = fp_text_default(font.family = "Arial", bold=TRUE))
    ), word_stylename = "Table Caption")
  
  print(ftab)   
  # print(ftab, preview = "docx")   
  
}



# avg length of vessels by vessel length category for each Baltic country ####

by <- capacity |> filter(`Vessel Length Category` != "NK") |>
  select(Country, Year, `Vessel Length Category`, `Average length`) |>
  group_by(Country, Year, `Vessel Length Category`) |> 
  summarise(mean.length = round(mean(`Average length`, na.rm=TRUE),2)) |>
  pivot_wider(names_from = `Vessel Length Category`, values_from = mean.length) |>
  ungroup() |> select(Country, Year, VL0008,VL0010,VL0812,VL1012,VL1218,VL1824,VL2440,VL40XX) |> group_split(Year)


for (i in 1:length(by)){
  
  table_nums(paste0("length_vessels_",i), 
             paste("Average length of vessels by vessel length category for each Baltic country in", years[i], "."))
  
  ftab <- by[[i]] |> select(-Year) |>
    flextable() |> autofit() |>
    set_caption(as_paragraph(
      as_chunk(table_nums(paste0("length_vessels_",i)), props = fp_text_default(font.family = "Arial", bold=TRUE))
    ), word_stylename = "Table Caption")
  
  print(ftab)   
  # print(ftab, preview = "docx")   
  
}



