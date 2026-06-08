# Workspace ----
library(tidyverse)
library(sf)
library(tmap)
setwd(dirname(.rs.api.getSourceEditorContext()$path))

relevant_areas <- 
  read_sf("input/census-boundaries/relevant-areas.shp") |> 
  st_set_crs(4326)

# durham ----
classify_road <- function(x) {
  type <- tolower(x)
  
  case_when(
    str_detect(type, "^(int|us)|freeway|interstate|hwy|ncbus|ooc|ramp") ~ "highway",
    str_detect(type, "arterial|collector|state") ~ "arterial",
    str_detect(type, "city|local|private") ~ "local",
    .default = "local"
  )
}

  
d <- read_sf("input/roads/durham.shp") |> rename_all(tolower)
r <- read_sf("input/roads/raleigh.shp") |> rename_all(tolower)

d_prep <-
  d |> 
  st_transform(4326) |> 
  select(
    road_id = objectid,
    road_name = strname,
    speed_limit = speed_lmt,
    n_lanes = lanes,
    road_width = width,
    road_length = shapestlen,
    road_type = func_class,
    facility_id = facility_1
  ) |> 
  mutate(
    road_type =
      road_type |> 
      str_remove("/.*") |> 
      str_replace("([a-z])([A-Z])", "\\1 \\2") |> 
      str_replace("(?i) road.*", "") |> 
      tolower(),
    class = classify_road(road_type)
  )
  
d_prep |> st_drop_geometry() |> count(class, road_type)

r_prep <-
  r |> 
  st_transform(4326) |> 
  transmute(
    road_id = objectid,
    road_name = cartoname,
    speed_limit = speed,
    n_lanes = 0,
    road_width = 0,
    road_length = shapelen,
    road_type = case_when(
      carto_leve == "HWY" ~ "hwy",
      carto_leve == "MAJOR" ~ "arterial",
      .default = classname
    ),
    facility_id = street_id
  ) |> 
  mutate(
    class = classify_road(road_type)
  )

r_prep |> filter(str_detect(road_name, "Wade Ave")) |> mapview::mapview(zcol = "class")

all_roads <-
  bind_rows(
    durham = d_prep,
    raleigh = r_prep,
    .id = "county"
  )

relevant_roads <-
  all_roads |> 
  st_intersection(relevant_areas)

relevant_roads |> filter(county == "durham") |> mapview::mapview(zcol = "class")
relevant_roads |> filter(county != "durham") |> mapview::mapview(zcol = "class")

saveRDS(roads, "cache-data/roads.Rds")
