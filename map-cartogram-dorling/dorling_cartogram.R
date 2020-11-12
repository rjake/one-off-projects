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


county_dorling <- # takes a while
  cartogram_dorling(
    x = counties_sf,
    weight = "pop_2015",
    k = 0.45,
    itermax = 500
  )

dorling_wgs84 <-
  county_dorling %>%
  st_transform(4326)

# tmap ----
tmap_mode("view")

map_limits <- st_bbox(dorling_wgs84 )

tm_basemap() +
  tm_shape(dorling_wgs84 , bbox = map_limits) +
  tm_polygons(
    "state_abbv",
    #"pop_2015",
    #style = "fisher",
    border.col = "grey60"
  )


ggplot() +
  # geom_sf(data = counties_sf, color = "grey80", fill = "white") +
  geom_sf(
    data = urbnmapr::get_urbn_map(sf = TRUE), fill =  NA, color = "grey60"
  ) +
  geom_sf(
    data = dorling_wgs84,
    aes(fill = pop_2015),
    color = "grey80"
  ) +
  scale_fill_viridis_b(
    direction = -1,
    option = "C", begin = 0.1, end = 0.7,
    labels = scales::number_format(big.mark = ","),
    trans = "log10"
  ) +
  theme_void() +
  labs(title = "US Population", fill = "2015 Census")



#save ----
saveRDS(dorling_wgs84, "geospatial/cartogram_county_pop.Rds")

dorling_wgs84 %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  st_set_geometry(NULL) %>%
  write_csv("geospatial/cartogram_county_pop.csv")

# geojson
dorling_wgs84 %>%
  st_write(
    dsn = "geospatial/cartogram_county_pop.geojson",
    driver = "GeoJSON",
    delete_dsn = TRUE
  )

urbnmapr::get_urbn_map(sf = TRUE) %>%
  st_transform(4326) %>%
  st_write(
    dsn = "geospatial/basemap_state.geojson",
    driver = "GeoJSON",
    delete_dsn = TRUE
  )

counties_sf %>%
  st_transform(4326) %>%
  st_write(
    dsn = "geospatial/basemap_county.geojson",
    driver = "GeoJSON",
    delete_dsn = TRUE
  )
