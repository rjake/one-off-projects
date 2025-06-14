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
  list.files("input/weather/", full.names = TRUE) |> 
  read_csv()


weather_stats <- 
  all_weather |> 
  #filter(fips == "37063") |> 
  transmute(
    state_abbr = st_abb,
    date,
    ppt,
    county_name,
    county_fips = fips,
    night = (tmin * 9/5) + 32,
    day =  (tmax * 9/5) + 32
  ) |> 
  summarise(
    .by = c(state_abbr, county_name, county_fips),
    total_precip_in = round(sum(ppt) / 25.4, 1),
    avg_temp_july_day = ifelse(str_detect(date, "202307"), day, NA) |> mean(na.rm = TRUE), 
    avg_temp_jan_day = ifelse(str_detect(date, "202301"), day, NA) |> mean(na.rm = TRUE), 
    pct_temp_60_80 = mean(between(day, 60, 80)),
    pct_temp_75_90 = mean(between(day, 75, 90)),
    pct_temp_below_40 = mean(day < 40),
    pct_temp_below_50 = mean(day < 50),
    pct_temp_below_60 = mean(day < 60),
    avg_daytime_temp = mean(day),
    avg_night_temp = mean(night)
  )

# Save ----
write_csv(weather_stats, "output/weather-2023.csv")
