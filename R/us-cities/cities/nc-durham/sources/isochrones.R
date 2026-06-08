#https://rpubs.com/jdsimkin04/963023

setwd(dirname(.rs.api.getSourceEditorContext()$path))

library(tidyverse)
library(sf)
library(osrm)
library(tmap)

gym <-
  data.table::fread(
    "location, x, y
    salvage yard, -78.6171, 35.8100 
    durham,       -78.9245, 35.9507"
  ) |> 
  st_as_sf(coords = c("x", "y"), crs = 4326)


iso_map <-
  osrmIsochrone(
    loc = gym,
    breaks = seq(from = 0, to = 25, 5),
    smooth = FALSE,
    n = 500 # min posible
  )
# if ERROR check: https://downforeveryoneorjustme.com/router.project-osrm.org

write_rds(iso_map, "output/sf-isochrone-gym.Rds")

iso_map %>% 
  rename(drive_time = isomax) |> 
  tm_shape() +
  tm_polygons("drive_time",
              palette = "viridis",
              alpha = 0.8)
