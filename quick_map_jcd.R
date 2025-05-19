# grab map data for Baltic states
p <- c("Finland","Sweden","Norway","Russia","Denmark","Germany",
       "Estonia","Latvia","Lithuania","Belarus","Poland")

world <- map_data("world", region=p)

baltic <- world |>
  st_as_sf(coords =c("long","lat"), crs=4326)

# subset to Baltic
# st_layers("data/ices_grid.gpkg")
baltic_ices <- st_read("/orig/ices_grid.gpkg", layer="ices_rectangles_eco")
effort <- read_excel("/orig/FIN_TABLE_I_EFFORT_BY_RECTANGLE.xlsx")
# make spatial
effort <- effort |>
  st_as_sf(coords =c("LONGITUDE","LATITUDE"), crs=4326)
# spatial join
baltic_ices2 <- baltic_ices |> st_join(effort, join=st_intersects)

ggplot() +
  geom_map(data = baltic, map = world, aes(group = group, map_id = region),
           fill = "lightgrey", colour="black") +
  geom_sf(baltic_ices2 |> filter(!is.na(RECTANGLE_TYPE)), mapping = aes(group=ICESNAME), fill="green", alpha=0.8) +
  geom_sf_text(data = baltic_ices2 |> filter(!is.na(RECTANGLE_TYPE)), aes(label = ICESNAME), label.size=0.5, size=2) + 
  theme_classic() +
  xlim(10, 31) + ylim(54,66) +
  labs(x = NULL, y = NULL) +
  coord_sf(expand=FALSE) +
  guides(fill="none") +
  theme(#axis.text = element_blank(),
    #axis.ticks = element_blank(),
    #axis.ticks.length = unit(0, "pt"),
    #axis.title=element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.background = element_rect(fill = "lightblue"))