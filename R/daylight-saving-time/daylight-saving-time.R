library(tidyverse)
library(suncalc) #install.packages("suncalc")
library(lubridate)

times <-
  seq.Date(
    as.Date("2023-01-01"), 
    as.Date("2023-12-01"), 
    "1 month"
  ) |> 
  getSunlightTimes(lat = 39.9526, lon = -75.1652) |> 
  print()


num_to_time <- function(x) {
  int <- floor(x)
  mod <- x %% int
  
  hour <- ifelse(int == 12, 12, int %% 12)
  mins <- round(mod * 60) |> str_pad(2, "left", "0")
  
  paste(hour, mins, sep = ":")
}


times |> 
  select(date, sunrise, sunset, noon = solarNoon) |> 
  mutate(
    across(-date, ~with_tz(.x, tz = "America/New_York")),
    sunrise = hour(sunrise) + (minute(sunrise) / 60),
    sunset = hour(sunset) + (minute(sunset) / 60),
    noon = hour(noon) + (minute(noon) / 60)
  ) |> 
  mutate(
    daylight = sunset - sunrise,
    quarter = quarter(date),
    my_end = case_when(
      quarter == 1 ~ 17.5,
      quarter == 2 ~ 19.5,
      quarter == 3 ~ 19.5,
      quarter == 4 ~ 17.5,
      TRUE ~ 18.5
    ),
    my_start = my_end - daylight,
    my_noon =  my_end - (daylight / 2)
  ) |> 
  mutate(across(-date, ~round(.x, 2))) |> 
  # print()
  ggplot(aes(date)) +
  geom_step(aes(y = sunrise, color = "original")) +
  geom_step(aes(y = noon, color = "original"), linetype = "dashed") +
  geom_step(aes(y = sunset, color = "original")) +
  geom_step(aes(y = my_start, color = "proposed")) +
  geom_step(aes(y = my_noon, color = "proposed"), linetype = "dashed") +
  geom_step(aes(y = my_end, color = "proposed")) +
  scale_y_continuous(
    labels = num_to_time,
    limits = c(0, 24),
    breaks = c(5, 9, 12, 17, 19, 21, 23)
  ) +
  scale_color_manual(values = c("grey80", "black")) +
  theme_minimal() +
  theme(panel.grid = element_blank())

last_plot()$data
