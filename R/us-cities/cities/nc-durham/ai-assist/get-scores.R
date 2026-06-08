library(tidyverse)
library(sf)
library(osmdata)
library(tidygeocoder)  # geocode addresses
library(httr2)

seed_addresses <- tribble(
  ~label,               ~address,
  "candidate_1",  "709 Brighton Rd, Raleigh, NC 27610",
  "candidate_2",  "1500 Valley Run, Durham, NC 27707",
  "current_home", "Ward St and James St, Durham, NC"
)

# --- 1. geocode ---
seeds <- seed_addresses |>
  geocode(address, method = "osm", lat = lat, lon = lon) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# --- 2. buffer (quarter mile ~ comfortable walk) ---
buffers <- seeds |>
  st_transform(32119) |>          # NC state plane, meters
  st_buffer(dist = 400) |>        # ~quarter mile
  st_transform(4326)

# --- 3. walk score per address ---
get_walk_score <- function(lat, lon, address) {
  request("https://api.walkscore.com/score") |>
    req_url_query(
      lat      = lat, lon = lon,
      address  = address,
      transit  = 1, bike = 1,
      wsapikey = Sys.getenv("WALKSCORE_KEY"),
      format   = "json"
    ) |>
    req_perform() |>
    resp_body_json() |>
    _[c("walkscore", "transit", "bike")]
}

walk_scores <- seeds |>
  st_drop_geometry() |>
  mutate(coords = map(geometry, st_coordinates)) # etc

# --- 4. greenspace from OSM ---
get_greenspace <- function(buffer_bbox) {
  opq(bbox = buffer_bbox) |>
    add_osm_feature(key = "leisure",
                    value = c("park", "nature_reserve", "garden")) |>
    osmdata_sf()
}

greenspace_counts <- buffers |>
  rowwise() |>
  mutate(
    bbox        = list(st_bbox(geometry)),
    gs_data     = list(get_greenspace(bbox)),
    gs_polygons = list(gs_data$osm_polygons),
    gs_area_m2  = if (!is.null(gs_polygons) && nrow(gs_polygons) > 0)
                    sum(st_area(st_intersection(gs_polygons, geometry)))
                  else 0
  )

# --- 5. parcel data (Durham example) ---
# download from: https://opendata.dc.arcgis.com  or
# https://www.dconc.gov/government/departments-f-z/tax-administration/gis-maps
parcels <- st_read("durham_parcels.gpkg") |>
  select(pin, lot_size_sqft, year_built, num_stories, sale_price, sale_date)

parcel_summary <- buffers |>
  rowwise() |>
  mutate(
    nearby_parcels = list(
      parcels |>
        st_filter(geometry) |>
        st_drop_geometry() |>
        filter(lot_size_sqft > 4000, num_stories <= 1.5,
               sale_price < 350000, sale_date >= "2023-01-01")
    ),
    qualifying_n      = nrow(nearby_parcels),
    median_price      = median(nearby_parcels$sale_price, na.rm = TRUE),
    median_lot_sqft   = median(nearby_parcels$lot_size_sqft, na.rm = TRUE)
  )

# --- 6. crime incidents (Durham open data) ---
# https://live-durhamnc.opendata.arcgis.com/datasets/durham-police-incidents
crime <- st_read("durham_police_incidents.gpkg") |>
  filter(report_date >= Sys.Date() - 365)

crime_summary <- buffers |>
  rowwise() |>
  mutate(crime_incidents_1yr = nrow(st_filter(crime, geometry)))

# --- 7. road noise proxy from OSM ---
get_road_noise_proxy <- function(buffer) {
  roads <- opq(bbox = st_bbox(buffer)) |>
    add_osm_feature(key = "highway",
                    value = c("motorway", "trunk", "primary", "secondary")) |>
    osmdata_sf()
  
  if (is.null(roads$osm_lines) || nrow(roads$osm_lines) == 0) return(0)
  
  roads$osm_lines |>
    st_intersection(buffer) |>
    mutate(
      noise_weight = case_match(highway,
        "motorway"  ~ 4,
        "trunk"     ~ 3,
        "primary"   ~ 2,
        "secondary" ~ 1
      )
    ) |>
    summarise(noise_score = sum(as.numeric(st_length(geometry)) * noise_weight)) |>
    pull(noise_score)
}