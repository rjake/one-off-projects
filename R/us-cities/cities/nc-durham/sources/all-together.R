# workspace ----
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(glue)
library(htmltools)
library(htmlwidgets)
setwd(dirname(.rs.api.getSourceEditorContext()$path))

map_limits <- 
  st_bbox(
    c(
      # xmin = -78.86, xmax = -78.95, 
      # ymin =  35.95, ymax =  36.00
      xmin = -78.85, xmax = -79.10,
      ymin =  35.90, ymax =  36.06
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

roads <- read_rds("output/roads.Rds")
property <- read_rds("output/sf-property-metadata.Rds")
iso_map <- read_rds("output/sf-isochrone-gym.Rds")
block_groups <- read_rds("output/sf-block-groups.Rds")
blocks <- read_rds("output/sf-blocks.Rds")

census_race  <- read_csv("output/census-race-blocks.csv", col_types = c(block_geoid = "c", geoid = "c"))
census_poverty  <- read_csv("output/census-poverty.csv", col_types = c(geoid = "c")) |> mutate(estimate = round(estimate, 2))
census_shift    <- read_csv("output/census-demo.csv", col_types = c(geoid = "c")) 

census_demo <-
  census_race |> 
  select(
    block_geoid,
    geoid, block_id,
    total_pop,
    starts_with("pct")
  ) |> 
  mutate(
    # block_white_pct = round(total_white / (total_black + current_white) * 100),
    # block_black_pct = round(total_black / (total_black + current_white) * 100)
  ) |> 
  left_join(
    census_shift |> select(geoid, shift_white, shift_black)
  ) |> 
  left_join(
    census_poverty |> select(geoid, income_ratio = estimate)
  ) |> 
  mutate(
    gentrifying = 
      as.integer(
        shift_white > 50 & shift_black < -50 & pct_black > 40
      )
      ,
    gentrification_shift = ifelse(gentrifying == 1, shift_white + abs(shift_black), 0),
    cat =
      case_when(
        income_ratio > 1.25 & income_ratio < 2 & pct_white > 50 ~ "working class white?",
        gentrifying == 1 ~ "gentrifying",
        pct_black > 60 ~ "historically black",
        income_ratio < 2 ~ "higher poverty",
        .default = "other"
      )
  ) |> 
  left_join(blocks) |> 
  st_as_sf() |> 
  st_transform(crs = 4326)



# Combine and create line
census_demo |> filter(geoid == "370630006003") |> .show_n()

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

export_map <- function(mv, html_name) {
  # buffer_dist <- 200 # meters
  # 
  # buffered_bbox <-
  #   map_limits |>
  #   st_as_sfc() |>
  #   st_transform(3857) |>
  #   st_buffer(dist = buffer_dist) |>
  #   st_transform(4326) |>
  #   st_bbox() |> 
  #   as.list()
  # 
  leaflet_map <- 
    mv@map |>
    # fitBounds(
    #   lng1 = map_limits["xmin"], lat1 = map_limits["ymin"],
    #   lng2 = map_limits["xmax"], lat2 = map_limits["ymax"]
    # ) |>
    # setMaxBounds(
    #   lng1 = buffered_bbox["xmin"], lat1 = buffered_bbox["ymin"],
    #   lng2 = buffered_bbox["xmax"], lat2 = buffered_bbox["ymax"]
    # ) #|>
    appendContent(htmltools::HTML(paste(readLines("www/geo.js"), collapse = "\n")))
  
  htmlwidgets::saveWidget(leaflet_map, html_name, selfcontained = TRUE)
}


crop_census <-
  census <- 
  census_demo |> 
  #filter(geoid  |> str_detect("37063000600")) |> 
  #filter(cat == "other") |> 
  st_crop(map_limits)

demo_colors <- 
  mapviewColors(
    x=crop_census,
    zcol = "district", 
    colors = c(
      "#4B0055", 
      "#944500", 
      "#007094",
      "#c5c5c5",
      "#FDE333"
    ),
    at = c(
      "gentrifying",
      "higher poverty", 
      "historically black",
      "other",
      "working class white?"
    )
  )


# map_census ----
crop_census |> 
  mapView(
    zcol = "cat", 
    alpha.regions = 0.5,
    color = "white",
    lwd = 0.5,
    layer.name = "demo",
    col.regions = demo_colors
  )

map_census <- .Last.value

if (FALSE) {
  census_block_info <-
    census_demo |> 
    filter(geoid  |> str_detect("37063000600")) |> 
    st_transform(4326) |> 
    st_crop(map_limits) |> 
    left_join(neighborhood) |> 
    st_as_sf()
  
  mapview(
    census_block_info,
    zcol = "geoid", 
    alpha.regions = 0.5, 
    color = "red", 
    layer.name = "demographics"
  )
  .Last.value |> 
    leafem::addStaticLabels(
      label = census_block_info$block_id,
      noHide = TRUE,
      direction = 'top',
      textOnly = TRUE,
      textsize = "20px"
    )
}


local_roads <-
  roads$osm_lines |> 
  filter(
    highway != "tertiary"
  ) |> 
  select(osm_id, name, type = highway, lanes) 


extra_road <-
  tibble(
    type = "motorway",
    geometry = 
      rbind(
        st_point(c(-78.999, 35.951)), 
        st_point(c(-78.966, 35.967))
      ) |> 
      st_linestring() |> 
      st_sfc(crs = 4326) # Match the CRS of your other table
  ) |>
  bind_rows(
    tibble(
      type = "secondary",
      geometry = 
        rbind(
          st_point(c(-78.905, 36.02)), 
          st_point(c(-78.905, 36.05))
        ) |> 
        st_linestring() |> 
        st_sfc(crs = 4326) # Match the CRS of your other table
    )
  ) |> 
  bind_rows(
    tibble(
      type = "secondary",
      geometry = 
        rbind(
          st_point(c(-78.903, 36.059)), 
          st_point(c(-78.905, 36.05))
        ) |> 
        st_linestring() |> 
        st_sfc(crs = 4326) # Match the CRS of your other table
    ) 
  ) |> 
  st_as_sf()

prep_roads <- 
  bind_rows(
    local_roads,
    extra_road
  ) |> 
  st_transform(crs = 32618) |>  # project to meters first
  mutate(
    buffer_width = 
      case_when(
        #is.na(osm_id) ~ 30,
        type == "motorway" ~ 75,
        type == "primary" ~ 50,
        TRUE ~ 20
      )
  ) |>
  (\(x) st_buffer(x, dist = x$buffer_width))() |> 
  group_by(type) |> 
  summarise(
    buffer_width = mean(buffer_width)
  ) |> 
  ungroup() |> 
  st_simplify(preserveTopology = TRUE, dTolerance = 1) |>  # 1 meter
  st_transform(crs = 4326) |>
  st_crop(map_limits) 
  

map_roads <- mapview(prep_roads, col.regions = "black", alpha.regions = 1)

map_roads

map_census +
  map_roads +
  map_home

m_census <- .Last.value
export_map(m_census, "output/census-category.html")
#mapshot(m_census, url = "output/census-category.html", selfcontained = TRUE)
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
    gentrifying == 1 | pct_black >= 60
  )

mapview(gentrifying_areas)

ideal_property <-
  property_metrics |> 
  left_join(
    census_demo |>
      select(block_geoid, gentrifying, cat) |>
      st_drop_geometry()
  ) |>
  mutate(
    ideal_ind = as.integer(
        between(acreage, 0.15, 0.3)
        & cost_total_value < 400000
        & bldg_sqft < 1600
        #& f_of_bedrooms == 2
        & !geoid %in% gentrifying_areas$geoid
      ) 
    )
      
ggplot() +
  stat_summary_hex(
    data = ideal_property,
    aes(x, y, z = ideal_ind),
    fun = sum, bins = 20
  ) +
  scale_fill_binned(type = "viridis", n.breaks = 7)

# 
# library(ggmap)
# # basemap <- get_map(location = c(lon = -78.9, lat = 35.97), zoom = 12, maptype = 'roadmap')
# ggmap(basemap) +
#   stat_summary_hex(
#     data = ideal_property |> filter(ideal_ind == 1),
#     aes(x, y, z = ideal_ind),
#     fun = sum, bins = 20, alpha = 0.5
#   ) +
#   scale_fill_binned(type = "viridis", n.breaks = 4)

ideal_property_sf <-
  ideal_property |> 
  st_centroid()
  #st_as_sf(coords = c("x", "y")) |> 
 # st_set_crs(st_crs(4326))


prep_map <-
  ideal_property_sf |> 
  select(
    parcel_id,
    geoid,
    cat,
    #pct_black,
    ideal_ind,
    gentrifying,
    #gentrification_shift,
    neighborhood,
    address = full_address,
    total_prop_value,
    deed_date,
    years_owned,
    f_of_bedrooms,
    state_of_repair_code,
    bldg_sqft,
    desc_built_use,
    acreage,
    actual_year_built,
    bldg_age,
    block_geoid
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
  )

mapviewOptions(
  # see examples: https://leaflet-extras.github.io/leaflet-providers/preview/
  basemaps = 
    c(
      #"Thunderforest.Transport",
      #"Thunderforest.MobileAtlas",
      #"Jawg.Streets",
      #"Stadia.StamenToner"
      "CartoDB.Positron", 
      "OpenStreetMap",
      "Esri.WorldImagery",
      "OpenTopoMap"
    )
)




prep_map |>
  filter(
    geoid == "370630006003",
    bldg_sqft < 1300,
    total_prop_value > 0,
    total_prop_value < 500000,
    actual_year_built < 2000,
    years_owned < 3
  ) |> 
  mutate(
    color = between(month(deed_date), 4, 7)
  ) |> #view()
  ggplot(
    aes(
      x = floor_date(deed_date, "month"), 
      total_prop_value, 
      color = color,
      alpha = total_prop_value < 350000
    )
  ) +
  geom_hline(yintercept = 350000) +
  #geom_col(position = "dodge", width = 1) +
  geom_point(aes(size = bldg_sqft)) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%m\n%y"
  ) +
  scale_alpha(range = c(0.3, 1))

prep_map |>
  filter(geoid |> str_detect("37063000600")) |> 
  #filter(total_prop_value < 400000) |>   
  mutate(
    ideal_ind = (
      total_prop_value > 200000
      & total_prop_value < 400000
      & between(acreage, 0.15, 0.3)
      & bldg_sqft < 1600
      #& f_of_bedrooms == 2
      & !block_geoid %in% gentrifying_areas$block_geoid
    )
  ) |> 
  select(ideal_ind, deed_date, years_owned, total_prop_value, address, bldg_sqft, bldg_age, zillow, f_of_bedrooms) |> 
  mapview(
    zcol = "ideal_ind",
    #zcol = "cat",
    layer.name = "ideal"    
  ) +
  mapview(
    points_of_interest[1,],
    color = "black", 
    alpha.regions = 1,
    col.regions = "orange"
  )

my_neighborhood <- .Last.value
export_map(my_neighborhood, "output/my-neighborhood.html")


prep_map |>
  mutate(
    ideal_ind = (
      total_prop_value > 200000
      & total_prop_value < 400000
      & between(acreage, 0.15, 0.3)
      & bldg_sqft < 1600
      #& f_of_bedrooms == 2
      & !block_geoid %in% gentrifying_areas$block_geoid
    )
  ) |> 
  # filter(geoid == "370630006003") |> 
  #filter(total_prop_value < 400000) |>   
  filter(
    ideal_ind == 1,
    !str_detect(cat, "gent|pov|black"),
    total_prop_value > 200000
    #between(years_owned, 5, 7) | 
    #  years_owned > 20
  ) |> 
  st_crop(map_limits) |> 
  mapview(
    zcol = "total_prop_value",
    #zcol = "cat",
    layer.name = "total_prop_value"    
  ) +
  map_home

m <- .Last.value
map_census + m
census_demo |> filter(geoid == "370630006003") |> .show_n()

iso_map |> 
  filter(isomax <= 20) |> 
  st_crop(map_limits) |> 
  mapview(
    alpha.regions = 0,#0.25, 
    color = "orange", 
    lwd = 3, 
    alpha = 0.5,
    zcol = NULL, #"isomin",
    layer.name = "time to gym"
  )

map_iso <- .Last.value
map_census +
  map_iso +
  m

full_map <- .Last.value
export_map(full_map, "output/my-map.html")

m@map |> leaflet::setView(lng = -78.915, lat = 35.97, zoom = 12)

relevant_data |> 
  filter(phyaddr_zi == "27517") |>
  select(phyaddr_zi) |> 
  plot()

