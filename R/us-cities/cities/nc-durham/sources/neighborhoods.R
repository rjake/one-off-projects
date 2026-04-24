setwd("~/github/one-off-projects/R/us-cities/cities/nc-durham/")
library(sf)
library(tidyverse)
library(concaveman)
library(mapview)
library(nngeo)

"----------------------------------------------------------------------
view: 
https://github.com/PhillipBost/durham-hoods-geojson/blob/master/durham-hoods.geojson
------------------------------------------------------------------------"
setwd(dirname(.rs.api.getSourceEditorContext()$path))
library(geojsonsf)

neighborhoods <-
  file.path(
    "https://github.com/PhillipBost/durham-hoods-geojson",
    "raw/refs/heads/master",
    "durham-hoods.geojson"
  ) |> 
  geojsonsf::geojson_sf()


sf::write_sf(neighborhoods, "neighborhoods.geojson")


parcels <- 
  st_read("layers/Parcels/Parcels_NEW.shp") |> 
  rename_all(tolower) |> 
  rename(neighborhood = neighborho)

neighborhoods_sf <- 
  parcels |>
  filter(
    !str_detect(neighborhood, "SPECIAL USE"),
    !str_detect(neighborhood, "DUKE")
  ) |> 
  #head(10000) |> 
  #filter(neighborhood |> str_detect("NORTHGATE")) |> 
  st_zm() |>
  filter(!is.na(neighborhood), neighborhood != "") |>
  group_by(neighborhood) |>
  summarise(geometry = st_union(geometry)) |>
  ungroup()


consolidate_sf <- 
  neighborhoods_sf |>
  #st_cast("POLYGON") #|> 
  st_buffer(60) |>                         # expand to fill road gaps
  group_by(neighborhood) |>
  summarise(geometry = st_union(geometry)) |>
  ungroup() |> 
  st_buffer(-15) |> 
  st_make_valid() |>
  st_remove_holes()
  #st_cast("POLYGON") |>
  # rowwise() |>
  # mutate(
  #   geometry = st_polygon(
  #     list(concaveman(st_coordinates(geometry)[, 1:2], concavity = 2))
  #   ) |>
  #     st_sfc(crs = st_crs(parcels)) |>
  #     st_buffer(75) |> # expand to fill road gaps (units match CRS)
  #     st_buffer(-20)   # shrink back to smooth edges
  # ) |>
  # ungroup() |>
  # group_by(neighborhood) |>
  # summarise(geometry = st_union(geometry)) |>
  # ungroup() |>
  # st_as_sf()

mapview(consolidate_sf, zcol = "neighborhood", layer.name = "Neighborhood", legend = FALSE)

mapview(neighborhoods_sf, zcol = "neighborhood", layer.name = "Neighborhood", legend = FALSE)




library(sf)
library(dplyr)
library(nngeo)

MIN_AREA <- 50000      # sq feet - tune this
BUFFER_OUT <- 60       # feet out to fill road gaps
BUFFER_IN <- 30        # feet back in

# dissolve parcels to neighborhood polygons
neighborhoods_sf <- 
  parcels |>
  st_zm() |>
  filter(!is.na(neighborhood), neighborhood != "") |>
  group_by(neighborhood) |>
  summarise(geometry = st_union(geometry)) |>
  ungroup()

# split small vs large neighborhoods
large <- neighborhoods_sf |> filter(as.numeric(st_area(geometry)) >= MIN_AREA)
small <- neighborhoods_sf |> filter(as.numeric(st_area(geometry)) <  MIN_AREA)

# assign small neighborhoods to nearest large neighbor
small_reassigned <- 
  small |>
  mutate(
    nearest_idx = st_nearest_feature(geometry, large),
    neighborhood = large$neighborhood[nearest_idx]
  ) |>
  select(-nearest_idx)

# recombine, buffer, fill holes
consolidate_sf <- bind_rows(large, small_reassigned) |>
  group_by(neighborhood) |>
  summarise(geometry = st_union(geometry)) |>
  ungroup() |>
  st_buffer(BUFFER_OUT) |>
  group_by(neighborhood) |>
  summarise(geometry = st_union(geometry)) |>
  ungroup() |>
  st_buffer(-BUFFER_IN) |>
  st_make_valid() |>
  st_remove_holes()

mapview(consolidate_sf, zcol = "neighborhood", layer.name = "Neighborhood", legend = FALSE)





library(tidyverse)
library(sf)
library(osmdata)

# 1. Get major roads for Durham to act as "break lines"
durham_roads <- readRDS("cache-data/roads.Rds")

# 2. Use st_polygonize to create areas bounded by these roads
# This creates the 'skeleton' of the neighborhoods
neighborhood_skeletons <- 
  st_union(durham_roads$osm_lines) |> 
  st_polygonize() |> 
  st_as_sf()

ggplot(neighborhood_skeletons) +
  geom_sf()




parcels_clean <- st_simplify(final_property, dTolerance = 0.1)

# 2. Buffer slightly (e.g., 0.5 meters) to ensure overlap
# and union everything into one giant object
island_union <- 
  parcels_clean %>%
  st_buffer(dist = 0.5) %>%
  st_union()

# 3. Explode the giant object into separate 'islands'
# Each disconnected block becomes its own row
neighborhood_islands <- 
  st_cast(island_union, "POLYGON") %>%
  st_as_sf() %>%
  mutate(island_id = row_number())

# 4. Join the ID back to your original parcels
# This tags every house with its specific 'island' ID
parcels_with_islands <- 
  final_property %>%
  st_join(neighborhood_islands, join = st_intersects)

island_stats <- parcels_with_islands %>%
  st_drop_geometry() %>%
  group_by(island_id) %>%
  summarize(
    parcel_count = n(),
    pct_30_year_owners = mean(tenure > 30, na.rm = TRUE),
    avg_year_built = mean(actual_year_built, na.rm = TRUE)
  ) %>%
  filter(parcel_count > 5) # Ignore tiny clusters
