# set the folder location
folder_path <- "output/n-2000"

# basically runs from here
setwd(dirname(.rs.api.getSourceEditorContext()$path))
library(tidyverse)
library(sf)

hospitals <- # all hospitals nation wide
  read_csv("https://raw.githubusercontent.com/synthetichealth/synthea/HEAD/src/main/resources/providers/hospitals.csv")

orgs <- # organizations from synthea generated files
  file.path(folder_path, "csv/organizations.csv") |> 
  read_csv() |>
  rename_all(tolower) |>
  st_as_sf(coords = c("lon","lat"), crs = 4326, remove = FALSE)

attr(hospitals, "spec") <- NULL
attr(orgs, "spec") <- NULL

# prep data
ed_hospitals <- # find philly hospitals / ED locations
  hospitals |>
  filter(
    emergency == TRUE
    | str_detect(name, "TEMPLE|MAIN LINE|NAZARETH")
  ) |>
  filter(
    lat < max(orgs$lat),
    lat > min(orgs$lat),
    lon < max(orgs$lon),
    lon > min(orgs$lon),
  ) |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  ) |>
  print()


closest_hospital <- # match org to closest hospital
  orgs |>
  select(id) |>
  st_join(ed_hospitals, join = st_nearest_feature) |>
  st_set_geometry(NULL)

# export to: output/n-xxx/closest_hospital.csv
write_csv(
  closest_hospital,
  file.path(
    getwd(),
    folder_path,
    "closest_hospital.csv"
  )
)
