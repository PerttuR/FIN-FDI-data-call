rm(list=ls())

# install rnaturalearthhires
# install.packages("remotes")
# install.packages("devtools")
# devtools::install_github("ropensci/rnaturalearthhires")


# load libraries ####
library(tidyverse)
library(sf)
library(readxl)
library(ggplot2)
library(sf)
library(rnaturalearth)
# library(rnaturalearthhires)
library(rnaturalearthdata)
library(dplyr)

# Define countries ####
countries <- c("Finland", "Sweden", "Norway", "Denmark", "Russia", "Germany",
               "Estonia", "Latvia", "Lithuania", "Belarus", "Poland")

# Get country borders as sf object
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin %in% countries)

# get ices gpkg ####
st_layers("maps/ices_grid.gpkg")

# ices rectangles ####
ices <- read_sf("maps/ices_grid.gpkg", 
                     layer="ices_rectangles_eco")

ices <- ices |> select(-ID)

# ggplot(ices) +
#   geom_sf(aes(fill = ICESNAME))

# get table I ####

TABLE_I <-  read_excel("results/2025/FIN_TABLE_I_EFFORT_BY_RECTANGLE.xlsx") # where are the rectangles???

# add lon/lat to ices spatial layers ####
source("spatial.R")

midpoints <- latlon(ices$ICESNAME,midpoint=TRUE)

ices <- tibble::rowid_to_column(ices, "ID")
midpoints <- tibble::rowid_to_column(midpoints, "ID")

ices <- left_join(ices, midpoints, copy = TRUE)

ices <- ices %>% rename(LATITUDE = SI_LATI, LONGITUDE = SI_LONG) %>% select(-ID)


# testing
effort <- ices |> select(ICESNAME, LONGITUDE, LATITUDE) |> left_join(TABLE_I)

# combine all years of data
effort2 <- effort |> group_by(ICESNAME,YEAR) |>
  summarise(annual.EFFORT = sum(TOTFISHDAYS, na.rm=TRUE)) |> 
  ungroup() |> 
  group_by(ICESNAME) |>
  summarise(EFFORT = mean(annual.EFFORT, na.rm=TRUE))

effort2$text_color <- ifelse(effort2$EFFORT > 5000, "white", "black")

effort2$EFFORT_BIN <- factor(cut(effort2$EFFORT,
                          breaks = c(1, 100, 500, 1000, 5000, 10000, Inf),
                          labels = c("1–100", "100–500", "500–1000", "1000–5000", "5000–10000", ">10000"),
                          include.lowest = TRUE))
                          
                          
# viridisLite::viridis(n = 10, option = "mako", direction=-1)

# Plot effort by ices ####
ggplot() +
  geom_sf(data = effort2, aes(fill = EFFORT_BIN)) +
  geom_sf(data = world_sf, fill = "grey", color = "black", size = 1, alpha=0.8) +
  geom_sf_text(data = effort2, aes(label = ICESNAME), size = 2, color = effort2$text_color) +
  scale_fill_viridis_d(option = "mako", name = "Fishing Effort", na.value = "transparent", direction=-1) +
  coord_sf(xlim = c(10, 30), ylim = c(54, 65), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Mean of annual fishing days by rectangle for 2013-2024") +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = NA))

ggsave("results/2025/effort.png", width = 20, height = 20, units = "cm", dpi=300)

# combine all years of data
effort.tech <- effort |> group_by(ICESNAME,FISHING_TECH, YEAR) |>
  summarise(annual.EFFORT = sum(TOTFISHDAYS, na.rm=TRUE)) |> 
  ungroup() |> group_by(ICESNAME, FISHING_TECH) |>
  summarise(EFFORT = mean(annual.EFFORT, na.rm=TRUE)) |>
  filter(!is.na(FISHING_TECH))

effort.tech$text_color <- ifelse(effort.tech$EFFORT > 5000, "white", "black")

effort.tech$EFFORT_BIN <- factor(cut(effort.tech$EFFORT,
                                 breaks = c(1, 100, 500, 1000, 5000, 10000, Inf),
                                 labels = c("1–100", "100–500", "500–1000", "1000–5000", "5000–10000", ">10000"),
                                 include.lowest = TRUE))

# Plot effort by ices and fishing_ tech ####
ggplot() +
  geom_sf(data = effort.tech, aes(fill = EFFORT_BIN)) +
  geom_sf(data = world_sf, fill = "grey", color = "black", size = 1, alpha=0.8) +
  geom_sf_text(data = effort.tech, aes(label = ICESNAME, color = text_color), size = 2) +
  scale_fill_viridis_d(option = "mako", name = "Fishing Effort", na.value = "transparent", direction=-1) +
  scale_color_identity() +
  facet_wrap(~FISHING_TECH) +
  coord_sf(xlim = c(10, 30), ylim = c(54, 65), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Mean of annual fishing days by fisching tech and rectangle for 2013-2024") +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = NA),
        panel.spacing = unit(1.5, "lines"))

ggsave("results/2025/effort.tech.png", width = 40, height = 20, units = "cm", dpi=300)

# get table H ####

TABLE_H <-  read_excel("results/2025/FIN_TABLE_H_LANDINGS_BY_RECTANGLE.xlsx")

# testing
weight <- ices |> select(ICESNAME, LONGITUDE, LATITUDE) |> left_join(TABLE_H)

# combine all years of data
weight2 <- weight |> group_by(ICESNAME) |>
  mutate(TOTVALLANDG = case_when(
    TOTVALLANDG == "NK" ~ NA,
    .default = as.numeric(TOTVALLANDG))) |> 
  summarise(WEIGHT = mean(TOTWGHTLANDG, na.rm=TRUE),
            VALUE = mean(TOTVALLANDG, na.rm=TRUE))

# Plot weight by ices ####
ggplot() +
  geom_sf(data = weight2, aes(fill = WEIGHT)) +
  geom_sf(data = world_sf, fill = "grey", color = "black", size = 1, alpha=0.8) +
  geom_sf_text(data = weight2, aes(label = ICESNAME), size = 2, color = "black") +
  scale_fill_viridis_c(name = "Total weight", na.value = "transparent", direction=-1, option = "mako") +
  coord_sf(xlim = c(10, 30), ylim = c(54, 65), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Mean landing weights 2013-2024") +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = NA))

ggsave("results/2025/weight.png", width = 20, height = 20, units = "cm", dpi=300)

# Plot value by ices ####
ggplot() +
  geom_sf(data = weight2, aes(fill = VALUE)) +
  geom_sf(data = world_sf, fill = "grey", color = "black", size = 1, alpha=0.8) +
  geom_sf_text(data = weight2, aes(label = ICESNAME), size = 2, color = "black") +
  scale_fill_viridis_c(name = "Total value", na.value = "transparent", direction=-1, option = "mako") +
  coord_sf(xlim = c(10, 30), ylim = c(54, 65), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Mean landing values 2013-2024") +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = NA))

ggsave("results/2025/value.png", width = 20, height = 20, units = "cm", dpi=300)

# combine all years of data
weight.tech <- weight |> group_by(ICESNAME, FISHING_TECH) |>
  mutate(TOTVALLANDG = case_when(
    TOTVALLANDG == "NK" ~ NA,
    .default = as.numeric(TOTVALLANDG))) |> 
  summarise(WEIGHT = mean(TOTWGHTLANDG, na.rm=TRUE),
            VALUE = mean(TOTVALLANDG, na.rm=TRUE)) |>
  filter(!is.na(FISHING_TECH))


# Plot weight by ices and fishing_ tech ####
ggplot() +
  geom_sf(data = weight.tech, aes(fill = WEIGHT)) +
  geom_sf(data = world_sf, fill = "grey", color = "black", size = 1, alpha=0.8) +
  geom_sf_text(data = weight.tech, aes(label = ICESNAME), size = 2, color = "black") +
  scale_fill_viridis_c(name = "Total weight", na.value = "transparent", direction=-1, option = "mako") +
  facet_wrap(~FISHING_TECH) +
  coord_sf(xlim = c(10, 30), ylim = c(54, 65), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Mean landing weights 2013-2024") +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = NA),
        panel.spacing = unit(1.5, "lines"))

ggsave("results/2025/weight.tech.png", width = 40, height = 20, units = "cm", dpi=300)

# Plot value by ices and fishing_ tech ####
ggplot() +
  geom_sf(data = weight.tech, aes(fill = VALUE)) +
  geom_sf(data = world_sf, fill = "grey", color = "black", size = 1, alpha=0.8) +
  geom_sf_text(data = weight.tech, aes(label = ICESNAME), size = 2, color = "black") +
  scale_fill_viridis_c(name = "Total value", na.value = "transparent", direction=-1, option = "mako") +
  facet_wrap(~FISHING_TECH) +
  coord_sf(xlim = c(10, 30), ylim = c(54, 65), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Mean landing value 2013-2024") +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = NA),
        panel.spacing = unit(1.5, "lines"))

ggsave("results/2025/value.tech.png", width = 40, height = 20, units = "cm", dpi=300)