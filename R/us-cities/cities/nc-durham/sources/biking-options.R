# Workspace ----
library(tidyverse)
library(sf)
library(tmap)
setwd(dirname(.rs.api.getSourceEditorContext()$path))

relevant_areas <- 
  read_sf("input/census-boundaries/relevant-areas.shp") |> 
  st_set_crs(4326)

# durham ----
d_trail <- read_sf("input/biking/durham-proposed.shp") |> rename_all(tolower)
d_bike_lane <- read_sf("input/biking/durham-current.shp") |> rename_all(tolower)
d_proposed_bike_lane <- read_sf("input/biking/durham-future.shp") |> rename_all(tolower)

prep_d_trail <-
  d_trail |>
  st_transform(4326) |>
  filter(
    mtbcycle == "Yes" 
    | roadcycle == "Yes"
    | (
      trailtype == "Multi Use Path" 
      & surftype == "Asphalt"
    )
  ) |> 
  transmute(
    path_name = systemname,
    surface = surftype,
    path_type = "trail",
    path_subtype = trailtype,
    status = tolower(status)
  )


prep_d_bike_lane <-
  d_bike_lane |>
  st_transform(4326) |>
  transmute(
    path_name = on_road,
    surface = "Asphalt",
    path_type = "bike lane",
    path_subtype = facility_t,
    status = "existing"
  )
  

prep_d_proposed_bike_lane <-
  d_proposed_bike_lane |>
  st_transform(4326) |>
  transmute(
    path_name = project_na,
    surface = "Asphalt",
    path_type = "bike lane",
    path_subtype = simplified,
    status = "proposed"
  )
  
durham <-
  bind_rows(
    prep_d_bike_lane,
    prep_d_proposed_bike_lane,
    prep_d_trail
  )

# raleigh ----
r_lanes <- read_sf("input/biking/raleigh-existing-and-planned.shp") |> rename_all(tolower)
r_trails <- read_sf("input/biking/raleigh-trails.shp") |> rename_all(tolower)


prep_r_lanes <-
  r_lanes |> 
  st_transform(4326) |>
  transmute(
    path_name = name,
    surface = "Asphalt",
    path_type = case_when(
      str_detect(existingfa, "LANE|SHARROW") ~ "bike lane",
      str_detect(existingfa, "PATH") ~ "trail"
    ),
    path_subtype = existingfa,
    status = "existing"
  )

prep_r_trails <-
  r_trails |> 
  st_transform(4326) |>
  transmute(
    path_name = location,
    surface = material,
    path_type = "trail",
    path_subtype = type,
    status = "existing"
  )


raleigh <-
  bind_rows(
    prep_r_lanes,
    prep_r_trails
  ) 

all_paths <-
  bind_rows(
    durham = durham,
    raleigh = raleigh,
    .id = "county"
  )

relevant_paths <-
  all_paths |> 
  st_intersection(relevant_areas)

relevant_paths |> filter(county == "durham") |> mapview::mapview(zcol = "status")
relevant_paths |> filter(county != "durham") |> mapview::mapview(zcol = "path_type")

saveRDS(relevant_paths, "cache-data/bike-trails.Rds")  




# analysis ---- 
points_of_interest <-
  data.table::fread(
    "location, x, y
    home, -78.9257, 35.9810
    gym , -78.9245, 35.9507"
  )

prep_road |> 
  filter(
    str_detect(road_name, "James"),
    #n_lanes == 2,
    #road_width < 40,
    #speed_limit <= 35
  ) |> 
  ggplot() +
  geom_sf(
    aes(color = as.factor(road_length))
  )
# +
#     geom_point(
#     data = points_of_interest,
#     aes(x, y),
#     size = 4,
#     color = "red"
#   )
  

final_road <-
  prep_road |> 
  mutate(
    is_eligible =  (
          road_type == "Local Roads"
          & road_width >= 20
          & speed_limit <= 25                   
        )
  )

ggplot() +
  theme_void() +
  geom_sf(
    data = final_road,
    aes(
      alpha = is_eligible
    ),
    show.legend = FALSE
  ) +
  geom_point(
    data = points_of_interest,
    aes(x, y),
    size = 4,
    color = "red"
  ) +
  geom_sf(data = prep_proposed_bike_lane, aes(color = "proposed bike lane")) +
  geom_sf(data = prep_bike_lane, aes(color = "bike lane")) +
  geom_sf(data = prep_trail, aes(color = "trail")) +
  scale_alpha_continuous(
    range = c(0.2, 0.8)
  ) +
  coord_sf() +
  scale_color_manual(
    values = c(
      "trail" = "green",
      "bike lane" = "blue",
      "proposed bike lane" = "orange"
    )
  )

ggsave("")


ggplot() +
  #theme_gray() +
  theme_void() +
  geom_sf(
    data = prep_road,
    aes(
      color = road_type
    ),
    show.legend = "point"
  )
