"-----------------------------------------------------------------
https://www.aaronsmithagecon.com/download-us-weather-data
------------------------------------------------------------------"

# Workspace ----
setwd(dirname(.rs.api.getSourceEditorContext()$path))

library(tidyverse)

# Functions ----
get_weather <- function(month, overwrite = FALSE) {
  # month <- 1
  month_pad <- str_pad(month, 2, "left", 0)
  url <- 
    paste0(
      "http://files.asmith.ucdavis.edu/weather/",
      "daily/county_noweight/",
      2023, month_pad,
      ".csv"
    )
  
  file_name <- paste0("input/weather/daily-", 2023, month_pad,".csv")
  
  if (file.exists(file_name) & !overwrite) {
    return(invisible())
  }
  
  download.file(
    url = url, 
    destfile = file_name
  )
}

# Download all ----
if (FALSE) {
  map(
    1:12,
    possibly(~get_weather(.x), "error")
  )
}

# Aggregate data ----
all_weather <- 
  list.files("prep/sources/weather/", full.names = TRUE) |> 
  read_csv()


weather_stats <- 
  all_weather |> 
  transmute(
    state_abbr = st_abb,
    county_name,
    county_fips = fips,
    night = (tmin * 9/5) + 32,
    day =  (tmax * 9/5) + 32
  ) |> 
  summarise(
    .by = c(state_abbr, county_name, county_fips),
    pct_temp_ideal = mean(between(day, 60, 90)),
    pct_temp_below_60 = mean(day < 60),
    avg_daytime_temp = mean(day)
  )

# Save ----
write_csv(weather_stats, "prep/sources/output/weather-2023.csv")
