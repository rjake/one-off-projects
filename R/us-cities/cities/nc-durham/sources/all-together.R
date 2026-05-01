# Workspace ----
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(glue)
library(htmltools)
library(htmlwidgets)
setwd(dirname(.rs.api.getSourceEditorContext()$path))

## map_limits ----
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


# Data ----
## shp ----
roads <- read_rds("output/roads.Rds")
property <- 
  read_rds("output/sf-property-metadata.Rds") |> 
  rename(
    style = desc_built_use
  )

iso_map <- read_rds("output/sf-isochrone-gym.Rds")
block_groups <- read_rds("output/sf-block-groups.Rds")
blocks <- read_rds("output/sf-blocks.Rds")

## csv ----
census_block_demo <- read_csv("output/census-race-blocks.csv", col_types = c(block_geoid = "c", geoid = "c"))
census_block_group_demo <- read_csv("output/census-demo.csv", col_types = c(geoid = "c")) 

# Prep ----
## Roads ----
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

# Functions ----
my_map <- function(df, overlay_groups = NULL) {
  #df <- neighborhood
  df |> 
    leaflet() |> 
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron") |> 
    addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") |> 
    addLayersControl(
      baseGroups = c("CartoDB.Positron", "OpenTopoMap"),
      overlayGroups = overlay_groups,
      options = layersControlOptions(collapsed = TRUE) # Keep menu open
    ) |> 
    #addProviderTiles(providers$CartoDB.Positron) |> 
    setView(lng = -78.9257, lat = 35.981, zoom = 13)
}
# my_map <- function(df, var, fill_type = NULL, n_col = 4, ...) {
#   #df <- neighborhood
#   df |> 
#     leaflet() |> 
#     addTiles(providers$CartoDB.Positron)
#   
#   fill_scale <-
#     list(
#       c = RColorBrewer::brewer.pal(n_col, "PuBu"),
#       d = RColorBrewer::brewer.pal(n_col, "RdBu")
#     )
#   df |> 
#     leaflet()
#   addProviderTiles(providers$CartoDB.Positron) %>%
#     setView(lng = -0.09, lat = 51.505, zoom = 13)
#   
#   df |> 
#     mapview(
#       zcol = var, 
#       layer.name = var,
#       ...,
#       col.regions = fill_scale[[fill_type]]
#     ) +
#     map_home
# }

export_map <- function(mv, html_name) {
  # leaflet_map <-
  #   mv@map |>
  #   fitBounds(
  #     lng1 = map_limits[["xmin"]], lat1 = map_limits[["ymin"]],
  #     lng2 = map_limits[["xmax"]], lat2 = map_limits[["ymax"]]
  #   ) |>
  #   setMaxBounds(
  #     lng1 = map_limits[["xmin"]], lat1 = map_limits[["ymin"]],
  #     lng2 = map_limits[["xmax"]], lat2 = map_limits[["ymax"]]
  #   ) |>
  m <-  
    mv |> 
    appendContent(htmltools::HTML(paste(readLines("www/geo.html"), collapse = "\n")))
  
  htmlwidgets::saveWidget(m, html_name, selfcontained = TRUE)
}

## census_demo ----
census_demo <-
  census_block_demo |> 
  select(
    block_geoid,
    geoid, block_id,
    total_pop,
    biggest_group,
    biggest_pct,
    pct_white,
    pct_black,
    pct_latino
  ) |> 
  left_join(
    census_block_group_demo |> 
      select(geoid, shift_white, shift_black, est_poverty_ratio, med_hh_income)
  ) |> 
  left_join(blocks) |> 
  st_as_sf() |> 
  st_transform(crs = 4326)



# Combine and create line
census_demo |> filter(geoid == "370630006003") |> .show_n()

census_demo |> 
  ggplot(aes(x = biggest_pct, color = biggest_group)) +
  geom_density()

census_demo |> 
  #filter(geoid == "370630006003") |> 
  st_crop(map_limits) |> 
  ggplot() +
  geom_sf(aes(fill = biggest_group, alpha = biggest_pct), color = NA) +
  geom_sf(data = prep_roads, color = "black", fill = "black") +
  theme_void() +
  guides()

library(leaflet)
library(scales)

# 1. pre-calculate hex colors with alpha embedded
prep_census_color <-
  census_demo |> 
  st_crop(map_limits) |> 
  mutate(
    # generate base colors for the groups
    base_col = case_when(
      biggest_group == "latino" ~ "#1b9e77",
      biggest_group == "black" ~ "#7570b3",
      biggest_group == "asian" ~ "#d95f02",
      .default = "#b2b2b2"
    ),
    # combine base color with alpha (biggest_pct)
    fill_rgba = alpha(base_col, biggest_pct /100)
  )

popup_strings <- 
  prep_census_color |> 
  select(
    -c(base_col, fill_rgba),
    -one_of("popup_text")
  ) |> 
  st_drop_geometry() |> 
  apply(1, function(x) {
    paste0(
      "<b>", names(x), ":</b> ", as.character(x), 
      collapse = "<br/>"
    )
  })

# 2. convert to a list of individual HTML objects
prep_census_color$popup_text <- map_chr(popup_strings, HTML) |> unname()

# 2. build the leaflet map
leaflet() |> 
  addProviderTiles("CartoDB.Positron") |> 
  addPolygons(
    data = prep_census_color,
    fillColor = ~fill_rgba,
    fillOpacity = 1, # use 1 because transparency is now baked into the hex code
    weight = 0,
    popup = ~popup_text,
    group = "demo"
  ) |> 
  addPolygons(
    data = prep_roads,
    color = "black",
    fillColor = "black",
    weight = 1,
    fillOpacity = 1,
    group = "roads"
  ) |> 
  addControl(
    html = "Durham Census Blocks by biggest race group", 
    position = "bottomleft"
  ) |> 
  addLayersControl(
    overlayGroups = c("demo", "roads"),
    options = layersControlOptions(collapsed = FALSE)
  )

export_map(.Last.value, html_name = "output/race-group.html")


census_demo |> 
  #st_crop(map_limits) |> 
  mapview( 
    zcol = "biggest_group", 
    alpha.regions = census_demo$biggest_pct/100, 
    layer.name = "census demographics",
    lwd = 0
  ) + 
  mapview(
    prep_roads, 
    #color = "black", 
    alpha.regions = 1,
    col.regions = "black", 
    layer.name = "roads"
  )
  # scale_fill_manual(
  #   values = c(
  #     "asian" = "purple",
  #     "black",
  #     "latino",
  #     "white"
  #   )
  # )

# Map Features ----
## points of interest ----
points_of_interest <-
  data.table::fread(
    "location, x, y
    home, -78.9257, 35.9810
    gym , -78.9245, 35.9507"
  )|> 
  st_as_sf(coords = c("x", "y"), crs = 4326)

## map_home ----
map_home <-
  mapview(
    points_of_interest,
    color = "black", 
    alpha.regions = 1,
    col.regions = "orange"
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
  


# Make Maps ----

## crop_census ----
crop_census <-
  census <- 
  census_demo |> 
  #filter(geoid  |> str_detect("37063000600")) |> 
  #filter(cat == "other") |> 
  st_crop(map_limits) 


block_housing_price <-
  property |> 
  st_drop_geometry() |> 
  filter(cost_total_value > 0 & bldg_sqft > 0) |> 
  summarize(
    .by = block_geoid,
    x = mean(x),
    y = mean(y),
    n = n(),
    housing_p25 = quantile(cost_total_value, probs = 0.25, na.rm = TRUE),
    housing_med = median(cost_total_value, na.rm = TRUE),
    housing_mean = mean(cost_total_value, na.rm = TRUE),
    housing_p75 = quantile(cost_total_value, probs = 0.75, na.rm = TRUE),
    avg_acreage = median(acreage, na.rm = TRUE),
    avg_sqft = median(bldg_sqft, na.rm = TRUE),
    avg_price_foot = median(price_per_foot, na.rm = TRUE),
    # Blight Flag: % of parcels where the building is worth very little per foot
    pct_distressed = mean(bldg_val_per_foot < 75, na.rm = TRUE),
    # Tear-down Flag: % of parcels where the land is worth more than 70% of the total
    pct_land_heavy = mean(improvement_ratio < 0.3, na.rm = TRUE)
  )


census_metrics <-
  crop_census |> 
  left_join(
    block_housing_price |> select(block_geoid, housing_p25, housing_med)
  ) |> 
  mutate(
    # Gentrification: Combining your shift logic with an "eligibility" check (low starting income)
    gentrifying = as.integer(
      shift_white > 20 
      & shift_black < -50 
      & pct_black > 40 
      #& med_hh_income < 90000
    )
  ) |> 
  # https://share.google/aimode/8ZJxRjPA16SXmO6mQ  <---- GEMINI
  # https://share.google/aimode/KpteXp85Cungdon2s
  mutate(
    price_to_income = housing_med / med_hh_income,
    cat =
      case_when(
        gentrifying == 1 ~ "gentrifying",
        pct_black >= 60 | (pct_black > 40 & shift_black < -200) ~ "historically black",
        housing_p25 >= 500000 ~ "high income",
        est_poverty_ratio < 2.1 | med_hh_income <= 75000 ~ "higher poverty",
        #est_poverty_ratio > 1.25 & income_ratio < 2 ~ "working class?",
        .default = "other"
      )
  )

mapview(census_metrics, zcol = "cat")# + map_roads + map_home
mapview(census_metrics, zcol = "housing_stability") + map_home

census_metrics |> st_drop_geometry() |>  count(housing_stability, cat)
  # mutate(
  #   # A wide gap (Median >> p25) suggests diversity; 
  #   # A narrow gap (Median ≈ p25) suggests the "floor" has been raised (Gentrification)
  #   price_spread = housing_med - housing_p25,
  #   
  #   cat = case_when(
  #     pct_black >= 60 ~ "Historically Black",
  #     # gentrifying == 1 & housing_p25 > 350000 ~ "Gentrified / High Turnover",
  #     gentrifying == 1 ~ "Gentrifying", # & housing_med > 350000/ High Turnover",
  #     housing_med >= 600000 ~ "Upper Middle Class",
  #     med_hh_income < 55000 | est_poverty_ratio < 1.5 ~ "HIgh Poverty",
  #     med_hh_income < 75000 & housing_med > 400000 ~ "Emerging / Market Gap",
  #     housing_med >= 400000 ~ "Established Middle Class",
  #     .default = "Working Class"
  #   )
  # )
source("~/.active-rstudio-document", echo = TRUE)

census_metrics |> mapview(zcol = "cat", layer.name = "x")

  # mutate(
  #   gentrifying = 
  #     as.integer(
  #       shift_white > 50 & shift_black < -50 & pct_black > 40
  #     ),
  #   gentrification_shift = ifelse(gentrifying == 1, shift_white + abs(shift_black), 0),
  #   cat =
  #     case_when(
  #       gentrifying == 1 ~ "gentrifying",
  #       pct_black >= 60 ~ "historically black",
  #       housing_p25 >= 500000 ~ "high income",
  #       #est_poverty_ratio > 1.25 & est_poverty_ratio < 2 ~ "working class?",
  #       #est_poverty_ratio < 2.1 & housing_med < 300000 ~ "higher poverty",
  #       med_hh_income < 50000 | est_poverty_ratio < 2 ~ "deep poverty",
  #       med_hh_income < 75000 ~ "some poverty",
  #       .default = "other"
  #     )
  # )

census_metrics |> 
  filter(str_detect(block_geoid, "37063000600")) |> 
  # my_map(
  #   overlay_groups = "x"
  # ) |> 
  # leaflet::addPolygons(
  #   group = "x",
  #   color = "white",
  #   weight = 0.5,
  #   opacity = 1,
  #   fillColor = "blue"
  # )
  mapview(zcol = "housing_p25", layer.name = "p25")

## demo_colors ----
demo_colors <- 
  mapviewColors(
    x=census_metrics,
    zcol = "cat", 
    colors = c(
      "black",
      "#FDE333",
      "#944500", 
      "#007094",
      "#4B0055", 
      "#c5c5c5"
    ),
    at = c(
      "gentrifying",
      "high income",
      "higher poverty",
      "historically black",
      "some poverty",
      "other"
    )
  )


## > map_census ----
census_metrics |> 
  #filter(str_detect(block_geoid, "37063000600|3706300130")) |>
  mapView(
    zcol = "cat", 
    alpha.regions = 0.5,
    color = "white",
    lwd = 0.5,
    layer.name = "demo",
    col.regions = demo_colors
  )

map_census <- .Last.value
map_census + map_home

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

ideal_areas <-
  census_metrics |> 
  filter(
    !str_detect(cat, "gentrif|hist|poverty") 
  )

mapview(ideal_areas)

ideal_property <-
  property_metrics |> 
  left_join(
    census_metrics |>
      select(block_geoid, housing_p25, gentrifying, cat) |>
      st_drop_geometry()
  ) |>
  mutate(
    ideal_ind = as.integer(
        between(acreage, 0.15, 0.3)
        & cost_total_value < 400000
        & bldg_sqft < 1500
        #& f_of_bedrooms == 2
        & geoid %in% ideal_areas$geoid
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
    cost_total_value,
    deed_date,
    years_owned,
    f_of_bedrooms,
    craftsmanship,
    bldg_sqft,
    style,
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




 
local_geoid <-
  block_groups |> 
  filter(
    as.logical(
      st_distance(
        geometry, 
        st_sfc(st_point(c(-78.9257, 35.981)), crs = 4326)
      ) <= units::set_units(1, "miles")
      | st_distance(
        geometry, 
        st_sfc(st_point(c(-78.9245, 35.9507)), crs = 4326)
      ) <= units::set_units(1, "miles")
    )
  ) |> 
  pull(geoid)
#mapview(legend = FALSE)

# breakdown of sqft by # of rooms
property$improvement_ratio[property$f_of_bedrooms == 2] |> 
  quantile(1:10/10, na.rm = TRUE)

ideal_only <-
  prep_map |>
  #filter(geoid %in% local_geoid) |> 
  #filter(geoid |> str_detect("3706300060")) |> 
  mutate(
    ideal_ind = (
      between(cost_total_value, 200000, 400000)
      & between(acreage, 0.15, 0.3)
      & between(bldg_sqft, 900, 1500)
      #& f_of_bedrooms == 2
      & block_geoid %in% ideal_areas$block_geoid
      & replace_na(actual_year_built, 1900) < 1990
    )
  ) |> 
  filter(
    ideal_ind == 1
  ) |> 
  mutate(
    month_bought = month(deed_date),
    type_abbr = 
      case_when(
        str_detect(style, "CAPE") ~ "D",
        str_detect(style, "COLONIAL") ~ "L",
        str_detect(style, "CONTEMP") ~ "T",
        str_detect(style, "RANCH") ~ "R"
      ),
    age_color = ifelse(years_owned < 10, '5-8', '20+'),
    house_no = str_extract(address, "^\\d+")
  )




ideal_only |> 
  filter(
    (
      between(years_owned, 5, 8)
      | years_owned > 20
    )
  ) |> 
  filter(
    geoid %in% local_geoid
  ) |> 
  select(
    deed_date, 
    age_color,
    years_owned, 
    cost_total_value, 
    house_no,
    address,
    bldg_sqft, 
    acreage,
    f_of_bedrooms,
    zillow, 
    style
  ) |> 
  mapview(
    zcol = "age_color",
    layer.name = "years_owned",
    col.regions =  c("#944500", "#007094")
  ) +
  mapview(
    points_of_interest,
    color = "black", 
    alpha.regions = 1,
    col.regions = "orange"
  )



my_neighborhood <- .Last.value
export_map(my_neighborhood, "output/my-neighborhood.html")

ideal_only |> 
  st_drop_geometry() |> 
  filter(years_owned < 5) |> 
  mutate(
    year = year(deed_date),
    color = between(month_bought, 4, 7)
  ) |> #view()
  summarise(
    .by = c(year, month_bought),
    n = n()
  ) |> 
  summarise(
    .by = month_bought,
    avg= mean(n)
  ) |> 
  arrange(month_bought)


property_metrics |> 
  st_drop_geometry() |> 
  select(
    years_owned,
    actual_year_built,
    cost_total_value,
    acreage,
    bldg_sqft
  ) |> 
  summarise_all(~sum(is.na(.x))) |> 
  ggplot(
    aes(
      x = month_bought, 
      y = n, 
      fill = as.factor(year)
    )
  ) +
  geom_col() #+facet_grid(rows = "year")


prep_map |>
  mutate(
    ideal_ind = (
      cost_total_value > 200000
      & cost_total_value < 400000
      & between(acreage, 0.15, 0.3)
      & bldg_sqft < 1600
      #& f_of_bedrooms == 2
      & !block_geoid %in% gentrifying_areas$block_geoid
    )
  ) |> 
  # filter(geoid == "370630006003") |> 
  #filter(cost_total_value < 400000) |>   
  filter(
    ideal_ind == 1,
    !str_detect(cat, "gent|pov|black"),
    cost_total_value > 200000
    #between(years_owned, 5, 7) | 
    #  years_owned > 20
  ) |> 
  st_crop(map_limits) |> 
  mapview(
    zcol = "cost_total_value",
    #zcol = "cat",
    layer.name = "cost_total_value"    
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
  map_roads +
  map_iso +
  m

full_map <- .Last.value
full_map@map |> leaflet::setView(lng = -78.915, lat = 35.97, zoom = 12)
export_map(full_map, "output/my-map.html")


relevant_data |> 
  filter(phyaddr_zi == "27517") |>
  select(phyaddr_zi) |> 
  plot()

# NOTES #######################
## * craftsmanship / state_of_repair ----
"
The hierarchy typically follows this pattern from highest to lowest quality:
XX / XX-: Excellent+ / Custom Luxury. These are exceptionally high-end, custom-built homes with premium materials (e.g., heavy slate roofs, extensive masonry, high-end architectural details).
X / X+ / X-: Extra / Luxury. Higher than 'A' grade, these properties feature superior construction and many custom architectural features.
A+ / A / A-: Excellent. High-quality construction, often seen in upscale developments or high-end custom homes.
B+ / B / B-: Good. Better than average materials and workmanship. This is common in many modern professional subdivisions.
C+ / C / C-: Average. The standard for most mass-produced or 'tract' housing. 'C' represents the base level for average construction quality in the region.
D+ / D / D-: Below Average / Fair. Economy-grade construction with basic materials and little to no architectural detail.
E+ / E: Poor. Minimal construction quality, often associated with very old or basic utility structures. 
"
