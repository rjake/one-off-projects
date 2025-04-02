# Workspace ----
setwd(dirname(.rs.api.getSourceEditorContext()$path))

library(tidyverse)
library(tigris)
library(sf)

options(tigris_use_cache = TRUE)

county_map <- 
  tigris::counties(
    cb = TRUE,
    resolution = "20m"
  ) 

county_geo <- 
  county_map |>
  st_centroid() |> 
  transmute(
    state_name = STATE_NAME,
    county_name = NAME,
    county_fips = GEOID,
    sqmi_land = ALAND * 0.386102 / 1e6,
    sqmi_water = AWATER * 0.386102 / 1e6, 
    x = st_coordinates(geometry)[,1], 
    y = st_coordinates(geometry)[,2]
  ) |> 
  st_drop_geometry() |> 
  as_tibble()


get_subcounty <- function(state_name, overwrite = FALSE) {
  # state_name <- "Delaware"
  file_name <- 
    glue(
      x = state_name |> janitor::make_clean_names(),
      "input/census/county-subdivision-geo-info/{x}.csv"
    )
  
  if (file.exists(file_name) & !overwrite) {
    return(invisible())
  }
  
  df <- 
    tigris::county_subdivisions(
      state_name,
      cb = TRUE
    )
  
  prep_df <- 
    df |> 
    st_centroid() |> 
    transmute(
      state_name = STATE_NAME,
      county_name = NAMELSADCO,
      subcounty_name = NAME,
      county_fips = paste0(STATEFP, COUNTYFP),
      subcounty_fips = GEOID,
      sqmi_land = ALAND * 0.386102 / 1e6,
      sqmi_water = AWATER * 0.386102 / 1e6, 
      x = st_coordinates(geometry)[,1], 
      y = st_coordinates(geometry)[,2]
    ) |> 
    st_drop_geometry()
  
  write_csv(prep_df, file_name)
}
  

# Download all ----
if (FALSE) {
  map(
    unique(county_geo$state_name) |> sort(),
    possibly(~get_subcounty(.x, overwrite = TRUE), "error")
  )
}

# Aggregate data ----
all_subcounty <- 
  list.files("input/census/county-subdivision-geo-info/", full.names = TRUE) |> 
  read_csv()

# Save ----
write_csv(county_geo, "output/county-geo.csv")
write_csv(all_subcounty, "output/subcounty-geo.csv")
