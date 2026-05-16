setwd(dirname(.rs.api.getSourceEditorContext()$path))
library(tidyverse)

street_clean <- function(x) {
  x |> 
    toupper() |> 
    str_replace_all(
      c(
        "\\bE\\b" = "EAST",
        "\\bN\\b" = "NORTH",
        "\\bS\\b" = "SOUTH",
        "\\bW\\b" = "WEST",
        " HWY" = ""
      )
    ) |> 
    str_replace_all(
      c(
        "\\bAVENUE\\b" = "AVE",
        "\\bBOULEVARD\\b" = "BLVD",
        "\\bCIRCLE\\b" = "CIR",
        "\\bCOURT\\b" = "CT",
        "\\bDRIVE\\b" = "DR",
        "\\bLANE\\b" = "LN",
        "\\bPLACE\\b" = "PL",
        "\\bROAD\\b" = "RD",
        "\\bSAINT\\b" = "ST",
        "\\bSTREET\\b" = "ST",
        "\\bTERRACE\\b" = "TER"
      )
    )
}

raw_property <- read_rds("output/parcel-metadata.Rds")
raw_redfin <-
  bind_rows(
    active = read_csv("input/realestate/redfin-for-sale-2026-05-16-05-14-11.csv"),
    sold = read_csv("input/realestate/redfin-recently-sold.csv"),
    .id = "redfin_status"
  ) |> 
  janitor::clean_names() |> 
  select(
    address,
    zip = zip_or_postal_code,
    price,
    beds,
    baths,
    square_feet,
    lot_size,
    year_built,
    redfin_status,
    sold_date,
    mls_number,
    favorite,
    interested,
    x = longitude,
    y = latitude
  ) |> 
  drop_na(address)


raw_mls <-
  read_csv("input/realestate/mls-expired-2026-05-14.csv") |> 
  # readxl::read_xlsx("input/realestate/Riley - MLS Prospecting Targets v3.xlsx") |> 
  janitor::clean_names()

clean_property <- 
  raw_property |> 
  filter(
    .by = full_address,
    deed_date == max(deed_date)
  ) |> 
  transmute(
    parcel_id,
    parcel_ref,
    deed_date,
    deed_year,
    full_address = 
      full_address |> 
      street_clean(),
    zip,
    long = x,
    lat = y
  )

clean_redfin <-
  raw_redfin |> 
  mutate(
    .before = everything(),
    redfin_id = row_number(),
    full_address =
      address |> 
      street_clean() |> 
      paste0(", DURHAM, NC, ", zip)
  )

redfin_census <-
  clean_redfin |> 
  select(redfin_id, full_address) |> 
  #slice(1:2) |> 
  tidygeocoder::geocode(
    address = full_address, 
    method = "census",
    limit = 1,
    return_addresses = TRUE,
    full_results = TRUE,
    return_input = TRUE,
    verbose = TRUE
  )


clean_mls <-
  raw_mls |> 
  filter(
    # units 123A, etc
    !str_detect(address, "^\\d+[a-zA-Z]")
  ) |> 
  transmute(
    mls_number,
    zip = as.character(postal_code),
    address,
    full_address = 
      address |> 
      street_clean() |>
      str_replace_all(
        c(
          "\\bST(\\.)? ST\\b" = "ST",
          "\\bAVE AVE\\b" = "AVE",
          "\\bTER TER\\b" = "TER",
          "SHAFTSBERRY" = "SHAFTSBURY",
          "(ELLERBEE|LAKELAND|MAIN|SEDGEFIELD)$" = "\\1 ST",
          "(COLLIER) DR" = "\\1 RD",
          "(ACADIA) CT" = "\\1 ST",
          "(HOMESTEAD)" = "\\1 RD",
          "(TEAGUE)" = "\\1 PL",
          "(PENDERGRASS|ROCKWAY) AVE" = "\\1 ST",
          "^(2\\d{3}) CARVER\\b.*" = "\\1 W CARVER ST",
          "(ROSEHILL ST)" = "ROSEHILL AVE"
        )
      ) |> 
      paste0(", DURHAM, NC, ", replace_na(zip, "")) |> 
      trimws(),
    list_price = str_remove_all(list_price, "\\$|,") |> as.integer(),
    status
  ) |> # 544
  filter( # 390
    .by = c(full_address, zip),
    mls_number == max(mls_number)
  ) |> 
  left_join(
    clean_property |> select(full_address, parcel_id, parcel_ref, lat, long)
  )

Sys.setenv(GOOGLEGEOCODE_API_KEY = Sys.getenv("google_geocoding"))

missing_mls <-
  clean_mls |> 
  filter(is.na(parcel_id)) |> 
  transmute(
    mls_number,
    full_address
  )

matches_census <-
  missing_mls |> 
  #slice(1:2) |> 
  tidygeocoder::geocode(
    address = full_address, 
    method = "census",
    limit = 1,
    return_addresses = TRUE,
    full_results = TRUE,
    return_input = TRUE,
    verbose = TRUE
  )

matches_google <-
  missing_mls |> 
  anti_join(matches_census |>drop_na(lat) |> select(mls_number)) |>
  #slice(1) |> 
  tidygeocoder::geocode(
    address = full_address, 
    method = "google",
    limit = 1,
    return_addresses = TRUE,
    full_results = TRUE,
    return_input = TRUE,
    verbose = TRUE
  )


together <-
  clean_mls |> 
  drop_na(parcel_id) |> 
  transmute(mls_number, full_address, src = "address", lat, long, matched_address = full_address) |> 
  bind_rows({
    matches_census |> 
      drop_na(lat) |> 
      transmute(mls_number, full_address, src = "census", lat, long, matched_address)
  }) |> 
  bind_rows({
    matches_google |> 
      drop_na(lat) |> 
      transmute(mls_number, full_address, src = "google", lat, long, matched_address = formatted_address)
  }) |> 
  mutate(
    matched_address =
      matched_address |> 
      str_remove(", USA$") |> 
      street_clean() |> 
      str_replace(", NC ", ", NC, ")
  )

write_csv(together, "input/realestate/clean-mls-2026-05-15.csv")
together <- read_csv("input/realestate/clean-mls-2026-05-15.csv")


final_df <-
  together |>
  inner_join(
    clean_property |> 
    #  filter(deed_year < 2024) |> 
      select(
        parcel_id, 
        parcel_ref, 
        matched_address = full_address,
        zip
      )
  ) |> 
  inner_join(clean_mls |> select(mls_number, list_price, status)) |> 
  left_join(
    clean_redfin |> 
      select(full_address, redfin_id, redfin_status, redfin_price = price, sold_date, favorite, interested)
    #redfin_census |> select(full_address = matched_address, redfin_id)
  )

final_df |> 
  filter(
    !str_detect(replace_na(sold_date, ""), "202[56]"),
    replace_na(interested, "") != "N" # hidden
  ) |> 
#  view()
  transmute(
    mls_number,
    matched_address,
    street_no = str_extract(full_address, "^\\w+"),
    street_name = str_remove_all(full_address, "^\\w+ |, DURHAM, .*"),
    zip,
    list_price,
    lat,
    long,
    status,
    redfin_id, redfin_status, redfin_price,
    parcel_id,
    parcel_ref
  ) |> 
  write_csv("output/mls-clean.csv")

# Download photos ----
library(tidyverse)
library(curl)

dest_dir <- "input/realestate/photos"
dir.create(dest_dir, showWarnings = FALSE)

photos <- 
  raw_mls |>
  filter(
    mls_number %in% final_df$mls_number,
    !str_detect(photo, "NoPhotoAvailable")
  ) |> 
  select(
    id = mls_number,
    url = photo
  ) |> 
  mutate(
    dest = file.path(dest_dir, id) |> paste0(".jpg")
  )

results <- 
  multi_download(
    urls      = photos$url,
    destfiles = photos$dest,
    resume    = TRUE,
    progress  = TRUE,
    timeout   = 60
  ) |>
  as_tibble()

# What happened
results |>
  mutate(ok = success & status_code < 400) |>
  count(ok)

# Failures to inspect / retry
results |>
  filter(!success | status_code >= 400) |>
  select(url)
