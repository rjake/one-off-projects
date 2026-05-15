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

raw_mls <-
  read_csv("input/realestate/mls-expired-2026-05-14.csv") |> 
  # readxl::read_xlsx("input/realestate/Riley - MLS Prospecting Targets v3.xlsx") |> 
  janitor::clean_names()

clean_property <- 
  raw_property |> 
  transmute(
    parcel_id,
    parcel_ref,
    deed_date,
    deed_year,
    full_address = 
      full_address |> 
      street_clean() |> 
      paste0(", DURHAM, NC, ", zip),
    long = x,
    lat = y
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

Sys.setenv(GOOGLEGEOCODE_API_KEY = "AIzaSyDnLw2Z6H3G2257GZh5-zxiSMBUY2NkHh8")

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

#together <- read_csv("input/realestate/clean-mls-2026-05-15.csv")
write_csv(together, "input/realestate/clean-mls-2026-05-15.csv")


final_df <-
  together |>
  inner_join(
    clean_property |> 
      filter(deed_year < 2024) |> 
      select(
        parcel_id, 
        parcel_ref, 
        matched_address = full_address
      )
  ) |> 
  inner_join(clean_mls |> select(mls_number, list_price, status))

final_df |> write_csv("output/mls-clean.csv")
