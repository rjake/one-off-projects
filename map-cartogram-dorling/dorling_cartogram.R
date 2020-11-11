library(tidyverse)
library(cartogram)
library(urbnmapr) # devtools::install_github("UrbanInstitute/urbnmapr")
library(sf)
library(tmap)

# data ----
county_pop <-
  urbnmapr::countydata %>%
  select(
    county_fips,
    pop_2015 = hhpop
  )

counties_sf <-
  urbnmapr::get_urbn_map(map = "counties", sf = TRUE) %>%
  left_join(county_pop) %>%
  drop_na(pop_2015)


county_dorling <-
  cartogram_dorling(
    x = counties_sf,
    weight = "pop_2015",
    k = 0.5,
    itermax = 100
  )


# tmap ----
tmap_mode("view")

map_limits <- st_bbox(county_dorling)

tm_basemap() +
  tm_shape(county_dorling, bbox = map_limits) +
  tm_polygons(
    "pop_2015",
    style = "fisher",
    border.col = "grey60"
  )


ggplot(county_dorling, aes(fill = pop_2015)) +
  geom_sf(color = "grey80", show.legend = FALSE) +
  scale_fill_viridis_b(direction = -1, option = "C", trans = "log10") +
  theme_void() +
  labs(title = "US Population")



#save ----
county_dorling %>%
  select(county_fips:state_name, pop_2015) %>%
  saveRDS("map_dorling_cartogram.Rds")

county_dorling %>%
  st_transform(4326) %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  st_set_geometry(NULL) %>%
  write_csv("map_dorling_cartogram.csv")
