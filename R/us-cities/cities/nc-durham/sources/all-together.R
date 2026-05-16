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
      # xmin = -78.86, xmax = -78.95, ymin =  35.95, ymax =  36.00
      # xmin = -78.85, xmax = -79.10, ymin =  35.90, ymax =  36.06
      xmin = -79.01, xmax = -78.82, ymin =  35.89, ymax =  36.064
    ), 
    crs = st_crs(4326)
  )


# Data ----
## shp ----
roads <- read_rds("output/roads.Rds")
property <- 
  read_rds("output/sf-property-metadata.Rds") |> 
  mutate(
    full_address =
      full_address |>
      str_replace_all(
        c(
          "\\bE\\b" = "EAST",
          "\\bW\\b" = "WEST",
          " HWY" = ""
        )
      )
  )


iso_map <- read_rds("output/sf-isochrone-gym.Rds")
block_groups <- read_rds("output/sf-block-groups.Rds")
blocks <- read_rds("output/sf-blocks.Rds")

# https://www.redfin.com/city/4909/NC/Durham/filter/sort=lo-days,property-type=house+other,max-price=400k,max-sqft=1.5k-sqft,min-lot-size=4.5k-sqft,viewport=36.16242:35.81573:-78.70434:-79.20216,no-outline
# redfin <- 
#   read_csv("input/realestate/redfin_2026-05-09-04-29-47.csv") |> 
#   rename_with(
#     ~str_replace(.x, "^URL.*", "url") |> tolower()
#   ) |> 
#   janitor::clean_names() |> 
#   drop_na(address) |> 
#   st_as_sf(coords = c("longitude", "latitude")) |> 
#   st_set_crs(st_crs(4326)) |> 
#   st_join(blocks, join = st_within)


encode_img <- function(path, width = 200) {
  if (!file.exists(path)) return('<i>No photo available</i>')
  ext <- tools::file_ext(path)
  uri <- str_c("data:image/", ext, ";base64,", base64enc::base64encode(path))
  str_glue('<img src="{uri}" width="{width}">')
}

raw_mls <- 
  read_csv("output/mls-clean.csv") |> 
  mutate(
    photo_path = str_c("input/realestate/photos/", mls_number, ".jpg"),
    popup_html = str_c(
      str_glue("<b>{mls_number}</b><br>"),
      map_chr(photo_path, encode_img)
    )
  )

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
  bind_rows(
    tibble(
      type = "primary",
      geometry = 
        rbind(
          st_point(c(-78.854, 35.976)), 
          st_point(c(-78.837, 35.949))
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
      "USGS.USImagery",
      "Esri.WorldImagery",
      "OpenTopoMap"
    )
)


map_roads <- mapview(prep_roads, col.regions = "black", alpha.regions = 1)

map_roads

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

export_map <- function(x, html_name) {
  if (inherits(x, "mapview")) {
    use_map <- x@map
  } else {
    use_map <- x
  }
  leaflet_map <-
    use_map |>
    appendContent(htmltools::HTML(paste(readLines("www/geo.html"), collapse = "\n")))
  
  htmlwidgets::saveWidget(leaflet_map, html_name, selfcontained = TRUE)
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
  ) |> 
  left_join(
    census_block_group_demo |> 
      select(geoid, shift_white, shift_black, est_poverty_ratio, med_hh_income)
  ) |> 
  left_join(blocks) |> 
  st_as_sf() |> 
  st_transform(crs = 4326) |> 
  st_crop(map_limits)

if (FALSE) {
  census_demo |> 
    #filter(geoid == "370630006003") |> 
    st_crop(map_limits) |> 
    ggplot() +
    geom_sf(aes(fill = biggest_group, alpha = biggest_pct), color = NA) +
    geom_sf(data = prep_roads, color = "black", fill = "black") +
    theme_void() +
    guides()
  
}

library(leaflet)
library(scales)

# 1. pre-calculate hex colors with alpha embedded
popup_strings <- 
  census_demo |> 
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
census_demo$popup_text <- map_chr(popup_strings, HTML) |> unname()

# 2. build the leaflet map
leaflet() |> 
  addProviderTiles("CartoDB.Positron") |> 
  addPolygons(
    data = census_demo,
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


# Map Features ----
## points of interest ----
points_of_interest <-
  data.table::fread(
    "location, x, y
    home, -78.9257, 35.9810
    gym , -78.9245, 35.9507
    ww_park, -78.9234, 36.0471
    rq_park, -78.9000, 36.0311"
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
  census_demo |> 
  #filter(geoid  |> str_detect("37063000600")) |> 
  #filter(cat == "other") #|> 
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
  census_demo |> 
  #filter(block_geoid == "370630017054002") |> 
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
        est_poverty_ratio < 2 | med_hh_income <= 75000 ~ "higher poverty",
        #est_poverty_ratio > 1.25 & income_ratio < 2 ~ "working class?",
        .default = "other"
      )
  )

#mapview(census_metrics, zcol = "cat")# + map_roads + map_home
#mapview(census_metrics, zcol = "housing_stability") + map_home

#census_metrics |> st_drop_geometry() |>  count(housing_stability, cat)
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

#census_metrics |> mapview(zcol = "cat", layer.name = "x")

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

# census_metrics |> 
#   filter(str_detect(block_geoid, "37063000600")) |> 
#   mapview(zcol = "housing_p25", layer.name = "p25")

## demo_colors ----
demo_colors <- 
  mapviewColors(
    x=census_metrics,
    zcol = "cat", 
    colors = c(
      "#4B0055", 
      "#FDE333",
      "#944500", 
      "#007094",
      # "black",
      "#c5c5c5"
    ),
    at = c(
      "gentrifying",
      "high income",
      "higher poverty",
      "historically black",
      #"some poverty",
      "other"
    )
  )


## > map_census ----
census_metrics |> 
  select(-popup_text) |> 
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

# mapview(ideal_areas)

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
      ),
    eligible_ind =
      ideal_ind == 1 & owner_far == 1 & years_owned > 5
  )
      
# ggplot() +
#   stat_summary_hex(
#     data = ideal_property,
#     aes(x, y, z = eligible_ind),
#     fun = sum, bins = 20
#   ) +
#   scale_fill_binned(type = "viridis", n.breaks = 7)

ideal_property_sf <-
  ideal_property |> 
  st_centroid()
  #st_as_sf(coords = c("x", "y")) |> 
 # st_set_crs(st_crs(4326))


prep_map <-
  ideal_property_sf |> 
  inner_join(
    raw_mls |>
      select(parcel_id, mls_number, list_price, status, popup_html)
  ) |> 
  select(
    parcel_id,
    mls_number, list_price, status,
    geoid,
    cat,
    #pct_black,
    ideal_ind,
    eligible_ind,
    gentrifying,
    #gentrification_shift,
    neighborhood,
    address = full_address,
    zip,
    cost_total_value,
    deed_date,
    years_owned,
    craftsmanship,
    bldg_sqft,
    style,
    acreage,
    actual_year_built,
    bldg_age,
    owner_far,
    block_geoid,
    popup_html
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

ideal_only <-
  prep_map |>
  filter(
    !mls_number %in% c(
      # from google sheet where grade == X
      #data.table::fread("") |> pull(V1) |> glue_collapse(", ")
      626698, 629343, 637296, 650408, 661312, 687877, 718175, 718485, 718820, 721502, 730262, 733489, 768936, 776086, 857743, 874763, 876333, 957076, 983902, 1003391, 1607434, 1667497, 1703623, 1792502, 1815244, 1931988, 1942221, 2195618, 2288167, 2326380, 2331663, 2373314, 10101390, 10145359
    ) 
  ) |> 
  #filter(geoid %in% local_geoid) |> 
  # filter(geoid |> str_detect("3706300060")) |>
  mutate(
    ideal_ind = (
      block_geoid %in% ideal_areas$block_geoid
      # & between(cost_total_value, 200000, 400000)
      & acreage>= 0.15
      # & between(bldg_sqft, 900, 1500)
      # & replace_na(actual_year_built, 1900) < 1990
    )
  ) |>
  # filter(
  #   ideal_ind == 1
  # ) |>
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
  select(
    mls_number, list_price, status,
    cost_total_value,
    #deed_date, 
    #age_color,
    years_owned, 
    owner_far,
    cost_total_value, 
    #house_no,
    #address,
    bldg_sqft, 
    acreage,
    # zillow, 
    google,
    zip,
    style,
    popup_html
  ) |> 
  mapview(
    zcol = "owner_far",
    alpha.regions = 1,
    layer.name = "owner_far",
    col.regions =  c("#944500", "#007094")
    # ,
    # popup = ideal_only$popup_html |> map(htmltools::HTML)
      # leafpop::popupImage(
      #   ideal_only$popup_html |> map(htmltools::HTML),
      #   src = "local"
      # )
  )

m_mls <- .Last.value
export_map(m_mls, "output/expired-homes.html")

#my_neighborhood <- .Last.value
# export_map(my_neighborhood, "output/my-neighborhood.html")

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


ideal_property |> 
  st_drop_geometry() |> 
  filter(
    ideal_ind == 1,
    deed_year > 2020
  ) |> 
  count(deed_month, deed_year) |> 
  ggplot(
    aes(
      x = deed_month, 
      y = n, 
      fill = as.factor(deed_year)
    )
  ) +
  geom_col() +facet_grid(rows = "deed_year")


# prep_map |>
#   # filter(geoid == "370630006003") |> 
#   #filter(cost_total_value < 400000) |>   
#   filter(
#     # ideal_ind == 1,
#     #!str_detect(cat, "gent|pov|black"),
#     #cost_total_value > 200000
#     #between(years_owned, 5, 7) | 
#     #  years_owned > 20
#   ) |> 
#   st_crop(map_limits) |> 
#   mapview(
#     zcol = "cost_total_value",
#     #zcol = "cat",
#     layer.name = "cost_total_value"    
#   ) +
#   map_home

#m <- .Last.value
#map_census + m
#census_demo |> filter(geoid == "370630006003") |> .show_n()

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
  m_mls + 
  map_home +
  map_iso

export_map(.Last.value, "output/my-map.html")



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


prep_redfin <-
  redfin |> 
  st_join(
    census_metrics |> select(biggest_group, shift_white, est_poverty_ratio, cat), 
    join = st_within
  ) |>
  filter(
    is.na(cat) | str_detect(cat, "other|high income") | favorite == "Y",
    interested == "Y"
  ) |> 
  mutate(
    url = glue('<a href="{url}" target="_blank">{address}</a>')
  ) |> 
  select(
    url,
    cat,
    price,
    favorite,
    square_feet,
    lot_size,
    year_built,
    days_on_market,
    hoa_month,
    biggest_group, shift_white, est_poverty_ratio
  ) |> 
  relocate(
    geometry, .after = everything()
  )

mapview(prep_redfin, zcol = "price", layer.name = "redfin - favorite", alpha.regions = 1)
map_redfin <- .Last.value

map_census +
  map_roads +
  map_iso +
  map_home + map_redfin


full_map <- .Last.value
#full_map@map |> leaflet::setView(lng = -78.915, lat = 35.97, zoom = 12)
export_map(.Last.value, "output/my-map.html")
