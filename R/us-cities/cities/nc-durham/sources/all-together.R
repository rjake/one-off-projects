library(tidyverse)
library(sf)
library(mapview)
library(glue)
setwd(dirname(.rs.api.getSourceEditorContext()$path))

map_limits <- 
  st_bbox(
    c(
      xmin = -78.85, xmax = -78.99, 
      ymin =  35.89, ymax =  36.05
    ), 
    crs = st_crs(4326)
  )

points_of_interest <-
  data.table::fread(
    "location, x, y
    home, -78.9257, 35.9810
    gym , -78.9245, 35.9507"
  )|> 
  st_as_sf(coords = c("x", "y"), crs = 4326)


property        <- read_rds("output/sf-property-metadata.Rds")
iso_map         <- read_rds("output/sf-isochrone-gym.Rds")
block_groups    <- read_rds("output/sf-block-groups.Rds")

census_poverty  <- read_csv("output/census-poverty.csv", col_types = c(geoid = "c")) |> mutate(estimate = round(estimate, 2))
census_demo     <- 
  read_csv("output/census-demo.csv", col_types = c(geoid = "c")) |> 
  left_join(
    census_poverty |> select(geoid, pop = total, income_ratio = estimate)
  ) |> 
  relocate(pop, income_ratio, .after = geoid)

map_home <-
  mapview(
    points_of_interest,
    color = "black", 
    alpha.regions = 1,
    col.regions = "orange"
  )

fill_scale <-
  list(
    c = RColorBrewer::brewer.pal(4, "PuBu"),
    d = RColorBrewer::brewer.pal(4, "RdBu")
  )

neighborhood <-
  property |> 
  st_drop_geometry() |> 
  count(geoid, neighborhood) |> 
  #filter(geoid == "370630002001") |> 
  group_by(geoid) |> 
  #arrange(desc(n)) |>
  fill(neighborhood, .direction = "downup") |> 
  slice_max(order_by = n, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  mutate(
    neighborhood = str_extract(neighborhood, "^[^;]+") |> str_trunc(20)
  )
  
my_map <- function(df, var, fill_type = NULL, ...) {
  df |> 
    mapview(
      zcol = var, 
      layer.name = var,
      ...,
      col.regions = fill_scale[[fill_type]]
    ) +
    map_home
}

census_demo |> 
  inner_join(block_groups) |> 
  st_as_sf() |> 
  #st_crop(map_limits) |> 
  left_join(neighborhood) |> 
  st_as_sf() |> 
  my_map("income_ratio", "c") +
  map_home
#
#  

##############

#

property_metrics <-
  property |> 
  mutate(
    years_owned = round((today() - deed_date) / 365.25, 1) |> as.integer(),
    recently_sold_ind = if_else(years_owned <= 1, 1, 0),
    life_change_ind = as.integer(between(years_owned, 5, 8)),
    generational_ind = as.integer(years_owned > 25),
    bldg_age = year(today()) - actual_year_built
  )

gentrifying_areas <-
  census_demo |> 
  filter(
    gentrifying == 1 | black_pct >= 60
  )

ideal_property <-
  property_metrics |> 
  left_join(census_demo) |> 
  mutate(
    ideal_ind = (
        between(acreage, 0.15, 0.3)
        & cost_total_value < 400000
        & bldg_sqft < 1600
        #& f_of_bedrooms == 2
        & geoid %in% non_gentrifying_areas$geoid
      ) 
    )
      
  

p <-
  ggplot() +
  stat_summary_hex(
    data = ideal_property,
    aes(x, y, z = ideal_ind),
    fun = sum, bins = 20
  ) +
  scale_fill_binned(type = "viridis", n.breaks = 7)

p

library(ggmap)
# basemap <- get_map(location = c(lon = -78.9, lat = 35.97), zoom = 12, maptype = 'roadmap')
ggmap(basemap) +
  stat_summary_hex(
    data = ideal_property |> filter(ideal_ind == 1),
    aes(x, y, z = ideal_ind),
    fun = sum, bins = 20, alpha = 0.5
  ) +
  scale_fill_binned(type = "viridis", n.breaks = 4)



library(mapview)
ideal_property_sf <-
  ideal_property |> 
  st_centroid()
  #st_as_sf(coords = c("x", "y")) |> 
 # st_set_crs(st_crs(4326))


mapviewOptions(
  basemaps = c("OpenStreetMap", "CartoDB.Positron", "Esri.WorldImagery","OpenTopoMap")
)



m <-
  ideal_property_sf |> 
  filter(total_prop_value < 400000) |> 
  mutate(
    cat =
      case_when(
        gentrifying == 1 ~ "gentrifying",
        income_ratio < 2 ~ "poverty",
        black_pct > 40 ~ "black",
        .default = "other"
      )
  ) |> 
  select(
    parcel_id,
    geoid,
    cat,
    black_pct,
    gentrifying,
    gentrification_shift,
    neighborhood,
    address = full_address,
    years_owned,
    f_of_bedrooms,
    state_of_repair_code,
    total_prop_value,
    bldg_sqft,
    desc_built_use,
    acreage,
    actual_year_built,
    bldg_age
  ) |> 
  mutate(
      neighborhood = str_trunc(neighborhood, 20),
      zillow = 
        glue(
          link = glue("https://www.zillow.com/homes/{address}, Durham, NC_rb/"),
          '<a href="{link}" target="_blank">{address}</a>'
        ),
      google =
        glue(
          address_search = str_replace_all(address, " ", "+"),
          '<a href="https://www.google.com/search?q={address_search}+durham+nc" target="_blank">{address}</a>'
        )    
  ) |> 
  mapview(
    zcol = "cat",
    layer.name = "cat",
    map.types =  c("OpenStreetMap","CartoDB.Positron", "Esri.WorldImagery","OpenTopoMap")
  ) +
  mapview(
    points_of_interest,    
    color = "black", 
    alpha.regions = 1,
    col.regions = "orange"
  )

m
census_demo |> filter(geoid == "370630014002") |> .show_n()
iso_map |> st_crop(map_limits) |> mapview(alpha.regions = 0, color = "black", lwd = 1.5) + m
m@map |> leaflet::setView(lng = -78.915, lat = 35.97, zoom = 12)

relevant_data |> 
  filter(phyaddr_zi == "27517") |>
  select(phyaddr_zi) |> 
  plot()

