#library(moonsun) # devtools::install_github("cran/moonsun")

#install.packages("swephR")
library(swephR)

se_objects <-
  SE |> 
  as_tibble() |> 
  gather("id", "value")

se_objects |> 
  filter(str_detect(id, "FLG.*(HEL|BAR|XY|JPL|ORB)")) 
  

planets <- 
  SE |> 
  as_tibble() |> 
  select(SUN:NEPTUNE, EARTH) |> 
  gather("planet", "id")



get_coords <- function(year = 2022, month = 1) {
  jdut <- swe_julday(year, month, 1, 0, SE$GREG_CAL)
  
  res <-
    swe_calc_ut(
      jd_ut = jdut,
      ipl = planets$id,
      iflag = SE$FLG_XYZ # heliocentric
    ) |> 
    as.tibble()
  
  res$xx |>
    as.data.frame() |>
    select(
      lon = 1,
      lat = 2,
      dist = 3
    ) |> 
    mutate(
      id = planets$id,
      year = year
    )
}


# date_ranges <-
#   tibble(year = 2010:2022) |> 
#   expand_grid(month = 1:12)

date_ranges <-
  tibble(year = seq(-3000, 3000, 500))

prep_coords <- 
  pmap_dfr(date_ranges, get_coords) |> 
  print()
  
  
planet_coords <-
  prep_coords |> 
  #distinct()
  left_join(planets) |>
  filter(planet != "MOON") |> 
  print()
  

planet_coords |>
#  filter(planet %in% c("SUN", "MERCURY", "VENUS", "EARTH")) |> 
  ggplot(aes(lon, lat)) +
  facet_wrap(~year, ncol = 7) +
  geom_point(aes(color = planet)) +
  geom_point(aes(x = 0, y = 0)) +
  #geom_path(aes(group = planet, color = planet, order = year)) +
  coord_fixed()
