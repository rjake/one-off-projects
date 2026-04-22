# Workspace ----
library(tidyverse)
library(sf)
library(tmap)

map_limits <- 
  st_bbox(
    c(
      xmin = -78.95, xmax = -78.90, 
      ymax =  35.986, ymin =  35.937
    ), 
    crs = st_crs(4326)
  )

shp_road <- read_sf("input/Roads/Roads.shp") |> rename_all(tolower)
shp_trail <- read_sf("input/Existing_and_Proposed_Trails/Existing_and_Proposed_Trails.shp") |> rename_all(tolower)
shp_bike_lane <- read_sf("input/Existing_Bike_Facilities/Existing_Bike_Facilities.shp") |> rename_all(tolower)
shp_proposed_blike_lane <- read_sf("input/Future_Bike_Facilities/Future_Bike_Facilities.shp") |> rename_all(tolower)


shp_road |> 
  filter(
    str_detect(strname, "James"),
  ) |> 
  view()

prep_road <-
  shp_road |>
  filter(
    objectid != 75616,
    str_detect(surface, "(?i)asphalt|concrete")
  ) |> 
  st_transform(4326) |>
  st_crop(map_limits) |> 
  select(
    road_id = objectid,
    road_name = strname,
    speed_limit = speed_lmt,
    n_lanes = lanes,
    road_width = width,
    road_length = shapestlen,
    maintained_by = maint,
    road_type = func_class,
    facility_id = facility_1,
    lowest_no = blk_lo_eve,
    highest_no = blk_hi_odd
  ) |> 
  mutate(
    road_type =
      road_type |> 
      str_replace("Road$", "Roads") |> 
      str_replace("([a-z])([A-Z])", "\\1 \\2")

  )


prep_trail <-
  shp_trail |>
  st_transform(4326) |>
  st_crop(map_limits) |> 
  filter(
    mtbcycle == "Yes" | roadcycle == "Yes"
  ) |> 
  select(
    trail_id = objectid,
    path_name = name,
    path_width = width,
    path_length = length,
    surf_type = surftype,
    trail_type = trailtype,
    system_name = systemname,
    facility_id = facilityid
  )


prep_bike_lane <-
  shp_bike_lane |>
  st_transform(4326) |>
  st_crop(map_limits) |>
  select(
    object_id = objectid,
    path_name = on_road,
    path_type = simplified    
  )
  

prep_proposed_bike_lane <-
  shp_proposed_blike_lane |>
  st_transform(4326) |>
  st_crop(map_limits) |> 
  select(
    path_name = project_na,
    project_status = phase_stat,
    path_type = simplified,
    object_id = objectid
  )
  
rm(list = grep("shp_", ls(), value = TRUE))

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
