library(tidyverse)
library(sf)
library(glue)
library(mapview)
setwd(dirname(.rs.api.getSourceEditorContext()$path))

blocks <- read_rds("output/sf-blocks.Rds")

raw_parcel_info <-
  st_read("input/Parcels/Parcels_NEW.shp") |> 
  rename(neighborhood = NEIGHBORHO) |> 
  rename_all(tolower) |> 
  filter(
    land_class == "RES/ 1-FAMILY",
    city == "DURHAM"
  )

raw_property_info <-
  read_csv(
    "input/Data_Academy_-_Real_Property_List_2017.csv",
    #n_max = 100,
    col_select = c(
      "ObjectId",
      "PARCEL_REF",
      "PIN",
      "STREET__",
      "STREET_NAME",
      "PLAT_BOOK",
      "PLAT_PAGE",
      "ACTUAL_YEAR_BUILT",
      "DESC_BUILT_USE",
      "ATTACHED_GARAGE",
      "DESC_HEAT",
      "DESC_SUBDIVISION",
      "F__OF_BATHROOMS",
      "F__OF_BEDROOMS",
      "STATE_OF_REPAIR_CODE",
      "FIREPLACE",
      "BASEMENT",
      "ATTACHED_GARAGE"
    )
  ) |> 
  janitor::clean_names()
  

relevant_data <-
  raw_parcel_info |> 
  st_transform(crs = st_crs(4326))

relevant_cols |> 
  st_drop_geometry() |> 
  select(
    where(is.numeric)
  ) |> 
  head()

relevant_cols <-
  relevant_data |> 
  transmute(
    parcel_id = objectid_1,
    parcel_ref = as.numeric(reid),
    address = paste(phyaddr_st, phyaddr__1),
    full_address = paste(location_a),
    neighborhood,
    land_class = land_class,
    deed_date = ymd(deed_date),
    deed_year = year(deed_date),
    deed_month = month(deed_date),
    acreage,
    bldg_sqft = heated_are,
    cost_building_value = total_bldg,
    #cost_land_value = total_land,
    cost_total_value = cost_total
  ) |> 
  mutate(
    price_per_foot = cost_total_value / bldg_sqft,
    bldg_val_per_foot = cost_building_value / bldg_sqft,
    # Improvement Ratio: < 0.3 often means the house is a liability/tear-down
    improvement_ratio = cost_building_value / cost_total_value
  ) |> 
  relocate(geometry, .after = everything())

relevant_cols |> 
  st_drop_geometry() |> 
  select(
    where(is.numeric)
  ) |> 
  head(15) |> 
  arrange(desc(bldg_sqft))

as_points <-
  relevant_cols |> 
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[,1],
    y  = st_coordinates(.)[,2]
  ) |> 
  st_drop_geometry() |> 
  as_tibble()


parcel_cols <-
  as_points |> 
  add_count(address, name = "n_address")

property_cols <-
  raw_property_info |>
  mutate(
    .keep = "unused",
    property_id = object_id,
    street_no = street,
    street_name,
    address = paste(street, street_name),
    craftsmanship = state_of_repair_code
  ) |>
  relocate(property_id, address, street_no, street_name) |> 
  add_count(address, name = "n_address")

parcel_xref <-
  parcel_cols |> 
  as_tibble() |> 
  filter(!parcel_ref %in% raw_property_info$parcel_ref) |> 
  filter(n_address == 1) |> 
  select(
    parcel_id, 
    parcel_ref, 
    address
  ) |> 
  inner_join(
    property_cols |> 
      filter(n_address == 1) |> 
      select(address, property_id)
  ) |> 
  bind_rows({
    parcel_cols |> 
      st_drop_geometry() |> 
      as_tibble() |> 
      select(
        parcel_id, 
        parcel_ref, 
        address
      ) |> 
      inner_join(
        property_cols |> select(parcel_ref, property_id)
      ) 
  }) |> 
  select(-address)


join_all_metadata <-
  parcel_cols |> 
  select(-n_address) |> 
  left_join(parcel_xref) |> 
  left_join(property_cols |> select(-c(address, n_address))) |> 
  mutate(
    .after = parcel_ref,
    extra_metadata_ind = as.integer(!is.na(property_id)) 
  ) |> 
  relocate(property_id, .after = parcel_ref) |> 
  left_join(
    relevant_cols |> select(parcel_id, geometry)
  ) |> 
  st_as_sf()

property_metadata <-
  join_all_metadata |> 
  # st_transform(crs = 4326) |> 
  st_join(blocks, join = st_within) |>
  relocate(
    geometry, .after = everything()
  )

saveRDS(property_metadata, "output/sf-property-metadata.Rds")

### STOP ############################################################

###
grid_sf <- 
  ideal_property_sf |> 
  st_make_grid(square = !TRUE) |> 
  st_as_sf() |>  
  mutate(id = row_number())


intersect_data <- st_intersects(grid_sf, ideal_property_sf)

# Calculate summary (e.g., count and sum of 'value')
grid_summary <- 
  grid_sf  |> 
  mutate(
    n = lengths(intersect_data),
    neighborhood = sapply(
      intersect_data, 
      function(x) 
        unique(ideal_property_sf$neighborhood[x]) |>
        str_sub(1, 10) |> 
        paste(collapse = "; ")
    ),
    avg_price = sapply(
      intersect_data, 
      function(x) median(ideal_property_sf$total_prop_value[x], na.rm = TRUE)
    )
  )

grid_summary |> 
  filter(n > 0) |> 
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_binned(type = "viridis", n.breaks = 5)

grid_summary |> 
  filter(n > 0) |> 
  mapview::mapview(
    zcol = "n",
    alpha.regions = 0.2
  )

##############################################################

df <- read_csv("input/Data_Academy_-_Real_Prsoperty_List_2017.csv")

#mean(df$ObjectId %in% parcels$objectid_1)
#mean(df$ObjectId %in% relevant_data$feature_ke)
#mean(df$PARCEL_REF %in% parcels$objectid_1)
#mean(df$PARCEL_REF %in% parcels$feature_ke)
parcel_cols <-
  relevant_data |> 
  #filter(deed_date <="2016-01-01") |> 
  select(
    objectid_1,
    feature_ke,
    reid,
    pin,
    pin_ext,
    deed_date,
    street = phyaddr_st,
    street_name = phyaddr__1,
    plat_book,
    plat_page
  ) |> 
  mutate(
    .keep = "unused",
    address = paste(street, street_name),
    parcel_ref = as.numeric(reid),
    plat_id = 
      str_pad(plat_book, 6, pad = "0") |> 
      paste(str_pad(plat_page, 6, pad = "0"))
  ) |> 
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[,1],
    y  = st_coordinates(.)[,2]
  ) |> 
  add_count(address, name = "n_address")











mean(parcel_cols$objectid_1 %in% property_cols$object_id)
mean(parcel_cols$objectid_1 %in% property_cols$parcel_ref)
mean(parcel_cols$feature_ke %in% property_cols$object_id)
mean(parcel_cols$feature_ke %in% property_cols$parcel_ref)
mean(parcel_cols$parcel_ref %in% property_cols$object_id)
mean(parcel_cols$parcel_ref %in% property_cols$parcel_ref) # <---
mean(parcel_cols$address %in% property_cols$address)

if (FALSE) {
  inner_join(parcel_cols, property_cols, join_by(parcel_ref)) |> select(objectid_1, object_id, feature_ke, parcel_ref, address.x, address.y, contains("pin"))
  anti_join(parcel_cols, property_cols, join_by(parcel_ref))
  left_join(parcel_cols, property_cols, join_by(parcel_ref)) |> 
    mutate(has_info = !is.na(plat_id.y)) |> 
    filter(!has_info) |> 
    mapview::mapview(zcol = "has_info")
  
  parcels |> filter(objectid_1 == 329779) |> .show_n()
  parcel_cols   |> filter(address |> str_detect("1025 CORN")) # reid == parcel_ref
  property_cols |> filter(address |> str_detect("1025 CORN")) # reid == parcel_ref
  property_info |> filter(object_id == 13624) |> .show_n()
  parcels |> filter(location_a |> str_detect("^2218 FERRELL")) |> select(location_a, deed_date) |> st_drop_geometry() |> arrange(location_a, deed_date)

  
  inner_join(parcel_cols, property_cols, join_by(objectid_1 == object_id)) |> select(objectid_1, feature_ke, parcel_ref.x, parcel_ref.y, address.x, address.y)
  property_cols |> filter(address == "920 SEDGEFIELD") # parcel_ref
  
  inner_join(parcel_cols, property_cols, join_by(feature_ke == object_id)) |> select(objectid_1, feature_ke, parcel_ref.x, parcel_ref.y, address.x, address.y)
  property_cols |> filter(address == "920 SEDGEFIELD") # parcel_ref
  
  
  parcel_cols, property_cols
    objectid_1 == object_id
    or feature_ke == object_id
    parcel_ref == parcel_ref
}



prep_data |> 
  select(
    object_id,
    feature_ke,
    reid,
    pin,
    location_addr,
    mailing_address
  )



grid_sf <- 
  as_points |> 
  st_make_grid(square = !TRUE) |> 
  st_as_sf() |>  
  mutate(id = row_number())

prep_data |> 
  st_drop_geometry() |> 
  pull(years_owned) |>
  boxplot(horizontal = TRUE)
  
age_threshold <- 20
#joined_data <- st_join(grid_sf, as_points)
older_houses <- 
  as_points |> 
  filter(
    years_owned > age_threshold,
    
  )

intersect_data <- 
  st_intersects(
    grid_sf, 
    older_houses
  )

# Calculate summary (e.g., count and sum of 'value')
grid_summary <- 
  grid_sf  |> 
  mutate(
    n = lengths(intersect_data),
    avg_age = sapply(
      intersect_data, 
      function(x) mean(
        older_houses$years_owned[x])
      )
  )

grid_summary |> 
  filter(n > 0) |> 
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_binned(type = "viridis", n.breaks = 5)

mapview::mapview(
  grid_summary,
  zcol = "n",
  alpha.regions = 0.2
)

#############3

avg_age <-
  prep_data |>
  group_by(neighborhood) |> 
  summarise(
    avg_age = mean(years_owned)
  ) 

avg_age |> 
  ggplot(aes(fill = avg_age)) +
  geom_sf() +
  scale_fill_binned(type = "viridis", n.breaks = 7)



as_points |> 
  ggplot(aes(x, y, z = years_owned)) +
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_binned(type = "viridis", n.breaks = 7)


as_points |> 
  filter(years_owned > 10) |> 
  ggplot(aes(x, y)) +
  geom_hex(bins = 20) +
  scale_fill_binned(type = "viridis", n.breaks = 7)

 prep_data


#############



properties <- 
  read_csv("input/Parcels_NEW_4547873805091360708.csv") |> 
  rename_all(tolower)



library(tidyverse)
library(tidycensus)
library(sf)

# 1. Get Census Data (e.g., Median Income) for Durham County
# You will need a Census API key for this
durham_census <- get_acs(
  geography = "tract",
  variables = "B19013_001", # Median Household Income
  state = "NC",
  county = "Durham",
  geometry = TRUE,
  year = 2022
) %>%
  # Keep column names lowercase per your preference
  rename(median_income = estimate) %>%
  st_transform(st_crs(parcels)) # Ensure CRS matches your parcel data

durham_census_small <-
  durham_census |> 
  st_transform(crs = st_crs(4326)) |> 
  st_crop(map_limits)

# 2. Spatial Join: Assign Census data to each Parcel
# This uses st_join to find which census tract each parcel 'intersects'
parcels_with_demographics <- 
  model_data |> 
  st_join(durham_census_small, join = st_intersects)

# 3. Analyze Turnover by Income Brackets
turnover_analysis <- 
  parcels_with_demographics %>%
  mutate(income_bracket = ntile(median_income, 5)) %>% # Create 5 income groups
  group_by(income_bracket) %>%
  summarize(
    avg_years_owned = mean(years_owned, na.rm = TRUE),
    turnover_rate = mean(sold_recently, na.rm = TRUE)
  )

ggplot() +
  geom_sf(data = turnover_analysis, aes(color = turnover_rate))

ggplot(model_data, aes(x = x, y = price, z = depth)) +
  stat_summary_hex(fun = mean, bins = 25) +
  scale_fill_continuous(type = "viridis")

#


library(httr2)
library(sf)
library(mapview)

# durham_neighborhoods <- "https://webgis2.durhamnc.gov/server/rest/services/PublicWorksServices/NewFY24EquityNeighborhoodsMS/MapServer/7/query" |>
#   request() |>
#   req_url_query(
#     where       = "1=1",
#     outFields   = "*",
#     f           = "geojson",
#     outSR       = "4326"   # reproject to WGS84 on the server side
#   ) |>
#   req_perform() |>
#   resp_body_string() |>
#   st_read(quiet = TRUE)
# 
# mapview(durham_neighborhoods, zcol = "NAME", layer.name = "Neighborhood")



# library(osmdata)
# library(sf)
# 
# durham_neighborhoods <- 
#   opq(bbox = "Durham, NC") |>
#   add_osm_feature(key = "place", value = "neighbourhood") |>
#   osmdata_sf()
# 
# # The polygons are in $osm_polygons, named boundaries in $osm_multipolygons
# neighborhoods_sf <- 
#   bind_rows(
#     durham_neighborhoods$osm_polygons,
#     durham_neighborhoods$osm_multipolygons |> st_cast("POLYGON")
#   ) |>
#   select(osm_id, name, geometry)
# 
# mapview(neighborhoods_sf, zcol = "name", layer.name = "Neighborhood")



  #geom_hex(bins = 10)


as_points |> 
  ggplot() +
  geom_histogram(aes(x = deed_date))





neighborhoods_sf <- 
  parcels |>
  select(neighborhood, geometry) |> 
  filter(
    !str_detect(neighborhood, "SPECIAL USE")
  ) |> 
  st_zm() |>
  filter(!is.na(neighborhood), neighborhood != "") |>
  group_by(neighborhood) |>
  summarise(geometry = st_union(geometry)) |>
  ungroup()

consolidate_sf <-
  neighborhoods_sf |>
  head(10) |>
  rowwise() |>
  mutate(hull = list(
    concaveman(st_coordinates(geometry)[, 1:2], concavity = 2) |>
      # st_geometry() |>
      # st_set_crs(st_crs(parcels))
  )) |>
  ungroup()

consolidate_sf |>
  group_by(neighborhood) |>
  summarise(geometry = st_union(geometry)) |>
  ungroup()

mapview(consolidate_sf, zcol = "neighborhood", layer.name = "Neighborhood")





library(leaflet)
library(dplyr)
#Creates data
data("breweries91",package="leaflet")
#set.seed(1);
breweries91$goodbear<-sample(as.factor(c("terrific","marvelous","culparterretaping")),nrow(breweries91),replace=T)
#Colors
joliepalette<-c("red","green","blue")[1:nlevels(breweries91$goodbear)]
getColor <- function(breweries91) {joliepalette[breweries91$goodbear]}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(breweries91)
)

#Generate the javascript

jsscript3<-
  paste0(
    "function(cluster) {
const groups= [",paste("'",levels(breweries91$goodbear),"'",sep="",collapse=","),"];
const colors= {
groups: [",paste("'",joliepalette,"'",sep="",collapse=","),"],
center:'#ddd',
text:'black'
};
const markers= cluster.getAllChildMarkers();

const proportions= groups.map(group => markers.filter(marker => marker.options.group === group).length / markers.length);
function sum(arr, first= 0, last) {
return arr.slice(first, last).reduce((total, curr) => total+curr, 0);
}
const cumulativeProportions= proportions.map((val, i, arr) => sum(arr, 0, i+1));
cumulativeProportions.unshift(0);

const width = 2*Math.sqrt(markers.length);
const radius= 15+width/2;

const arcs= cumulativeProportions.map((prop, i) => { return {
x   :  radius*Math.sin(2*Math.PI*prop),
y   : -radius*Math.cos(2*Math.PI*prop),
long: proportions[i-1] >.5 ? 1 : 0
}});
const paths= proportions.map((prop, i) => {
if (prop === 0) return '';
else if (prop === 1) return `<circle cx='0' cy='0' r='${radius}' fill='none' stroke='${colors.groups[i]}' stroke-width='${width}' stroke-alignment='center' stroke-linecap='butt' />`;
else return `<path d='M ${arcs[i].x} ${arcs[i].y} A ${radius} ${radius} 0 ${arcs[i+1].long} 1 ${arcs[i+1].x} ${arcs[i+1].y}' fill='none' stroke='${colors.groups[i]}' stroke-width='${width}' stroke-alignment='center' stroke-linecap='butt' />`
});

return new L.DivIcon({
html: `
<svg width='60' height='60' viewBox='-30 -30 60 60' style='width: 60px; height: 60px; position: relative; top: -24px; left: -24px;' >
<circle cx='0' cy='0' r='15' stroke='none' fill='${colors.center}' />
<text x='0' y='0' dominant-baseline='central' text-anchor='middle' fill='${colors.text}' font-size='15'>${markers.length}</text>
${paths.join('')}
</svg>
`,
className: 'marker-cluster'
});
}")

# Generates the map.
leaflet() %>%
  addTiles() %>%
  addAwesomeMarkers(data=breweries91,
                    group=~goodbear,
                    icon = icons,
                    clusterOptions = markerClusterOptions(
                      iconCreateFunction =
                        JS(jsscript3)))
