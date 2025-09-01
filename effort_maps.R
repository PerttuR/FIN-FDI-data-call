library(tidyverse)
library(sf)
library(readxl)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

# Define countries ####
countries <- c("Finland", "Sweden", "Norway", "Denmark", "Russia", "Germany",
               "Estonia", "Latvia", "Lithuania", "Belarus", "Poland")

# Get country borders as sf object
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin %in% countries)

# get ices gpkg ####
st_layers("C:/Users/03269737/OneDrive - Valtion/Projects/VMS/data/ices_grid.gpkg")

# too detailed an slow!!!
ices <- read_sf("C:/Users/03269737/OneDrive - Valtion/Projects/VMS/data/ices_grid.gpkg", 
                     layer="ices_rectangles_eco")

ices <- ices |> select(-ID)

ggplot(ices) +
   geom_sf(aes(fill = ICESNAME))

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
test <- ices |> select(ICESNAME, LONGITUDE, LATITUDE) |> left_join(TABLE_I)

# combine all years of data
test2 <- test |> group_by(ICESNAME) |>
  summarise(EFFORT = mean(TOTFISHDAYS, na.rm=TRUE))



# Plot
ggplot() +
  geom_sf(data = test2, aes(fill = EFFORT)) +
  geom_sf(data = world_sf, fill = "grey", color = "black", size = 1, alpha=0.8) +
  geom_sf_text(data = test2, aes(label = ICESNAME), size = 2, color = "black") +
  scale_fill_viridis_c(na.value = "transparent", direction=-1) +
  coord_sf(xlim = c(10, 30), ylim = c(54, 65), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Mean fishing effort 2013-2024") +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = NA))



# combine all years of data
test2 <- test |> group_by(ICESNAME) |>
  summarise(EFFORT = mean(TOTFISHDAYS, na.rm=TRUE))

# Define countries of interest
countries <- c("Finland", "Sweden", "Norway", "Denmark", "Russia", "Germany",
               "Estonia", "Latvia", "Lithuania", "Belarus", "Poland")

# Get country borders as sf object
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin %in% countries)

# Plot effort by ices and fishing_ tech ####
ggplot() +
  geom_sf(data = test2, aes(fill = EFFORT)) +
  geom_sf(data = world_sf, fill = "grey", color = "black", size = 1, alpha=0.8) +
  geom_sf_text(data = test2, aes(label = ICESNAME), size = 2, color = "black") +
  scale_fill_viridis_c(na.value = "transparent", direction=-1) +
  coord_sf(xlim = c(10, 30), ylim = c(54, 65), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Mean fishing effort 2013-2024") +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = NA))


# 