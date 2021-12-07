library(tidycensus)
library(tidyverse)
library(simplecolors)

vars_acs <- load_variables(2019, "acs5", TRUE)

vars_of_interest <- 
  vars_acs |> 
  filter(str_detect(name, "B01001|B03002|B05002"))


get_estimates <-
  get_acs(
    geography = "county", 
    year = 2019, 
    variables = vars_of_interest$name, 
    state = "PA",
    county = "Philadelphia"
  )


sum_condition <- function(x, y) {
  sum(ifelse(x, y, 0), na.rm = TRUE)
}


prep_estimates <- 
  get_estimates |> 
  rename_all(tolower) |> 
  select(variable, estimate) |> 
  left_join(
    select(vars_acs, variable = name, label, concept)
  ) |> 
  print()


demo_pcts <- 
  prep_estimates |> 
  summarise(
    gender_male = sum_condition(variable == "B01001_002", estimate),
    gender_female = sum_condition(variable == "B01001_026", estimate),
    age_20s = sum_condition(str_detect(label, "\\!{2}2\\d+"), estimate),
    age_30s = sum_condition(str_detect(label, "\\!{2}3\\d+"), estimate),
    age_40s = sum_condition(str_detect(label, "\\!{2}4\\d+"), estimate),
    age_50s = sum_condition(str_detect(label, "\\!{2}5\\d+"), estimate),
    age_60s = sum_condition(str_detect(label, "\\!{2}6\\d+"), estimate),
    # nationality_citizen = sum_condition(variable == "B05002_002", estimate),
    # nationality_immigrant = sum_condition(variable == "B05002_013", estimate),
    race_white =  sum_condition(variable == "B03002_003", estimate),
    race_black =  sum_condition(variable == "B03002_004", estimate),
    race_asian =  sum_condition(variable == "B03002_006", estimate),
    race_latino = sum_condition(variable == "B03002_012", estimate),
    race_other = 
      sum_condition(variable == "B01001_001", estimate) - 
      race_white - race_black - race_asian - race_latino
  ) |> 
  gather() |> 
  separate(key, into = c("demo", "strata")) |> 
  add_count(demo, wt = value) |> 
  mutate(pct = value / n * 100) |> 
  print()
  

get_percents <- function(x, n = 10000) {
  df <- 
    demo_pcts |> 
    filter(demo == x)
  
  tibble(
    {{x}} := sample(
      x = df$strata, 
      size = n,
      replace = TRUE,
      prob = df$pct
    )
  )
}


expected_proportions <- 
  map_dfc(unique(demo_pcts$demo), get_percents) |> 
  count(across(everything())) |>
  mutate(espected_pct = n / sum(n) * 100) |>
  print()


clipr::write_clip(expected_proportions)
