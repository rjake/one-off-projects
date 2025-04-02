# Census ----
"-----------------------------------------------------------------

------------------------------------------------------------------"

# Workspace ----
setwd(dirname(.rs.api.getSourceEditorContext()$path))

library(tidyverse)
library(tidycensus)
library(glue)

state_info <- 
  tidycensus::get_acs(
    geography = "state", 
    variables = "B01001_001", 
    cache_table = TRUE
  ) |> 
  transmute(
    state_fips = GEOID,
    state_name = NAME,
    state_file_name = janitor::make_clean_names(NAME)
  )

acs_vars <- load_variables(2022, "acs5", TRUE)

vars_of_interest <- 
  acs_vars |> 
  filter(
    str_detect(
      name, 
      c(
        "B01001_001",    # population
        "B09019_01[13]", # gayness
        "B06012_00[23]", # poverty
        "B25087_0[01].",  # housing costs
        "B25096.*"       # house price
      ) |> 
        glue_collapse("|")
    )
  ) |> 
  filter(!str_detect(name, "B25087_00[12]|B25096_0(01|02|12)")) |> 
  mutate(
    category = 
      case_when(
        name == "B01001_001" ~ "population",
        str_detect(name, "B09019_01[13]") ~ "lgbt",
        str_detect(name, "B06012_00[23]") ~ "poverty",
        str_detect(name, "B25087_0[01]") ~ "housing costs",
        str_detect(name, "B25096.*") ~ "house price"
      )
  )


# Get Data ----
get_subcounty <- function(state, overwrite = FALSE) {
  # state <- "North Carolina"
  state_file_name <- janitor::make_clean_names(state)
  file_name <- paste0("input/census/county-subdivision-vars/", state_file_name, ".csv")
  
  if (file.exists(file_name) & !overwrite) {
    return(invisible())
  }
  
  df <- 
    get_acs(
      state = state,
      geography = "county subdivision", 
      year = 2023, 
      variables = vars_of_interest$name
    )
  
  write_csv(df, file_name)
}


if (FALSE) {
  # county 
  county_estimates <-
    get_acs(
      geography = "county", 
      year = 2023, 
      variables = vars_of_interest$name,
      cache_table = TRUE
    ) |> 
    rename(
      geoid = GEOID,
      location = NAME
    )
  
  write_csv(county_estimates, "input/census/raw-county-estimates.csv")
  
  # subcounty
  map(
    state_info$state_name,
    possibly(~get_subcounty(.x), "error")
  )
}

county_estimates <- 
  read_csv("input/census/raw-county-estimates.csv") 

subcounty_estimates <- 
  list.files("input/census/county-subdivision-vars/", full.names = TRUE) |> 
  read_csv()

# Aggregate data ----
aggregate_census <- function(df) {
  # df <- county_estimates |> filter(str_detect(GEOID, "^10"))
  
  prep_df <- 
    df |> 
    select(-moe) |> 
    rename(
      fips = GEOID,
      name = NAME
    ) |> 
    filter(!str_detect(variable, "B25096_00(1|2|12)")) |> 
    left_join(
      vars_of_interest |> 
        select(
          variable = name, 
          category,
          label
        ) |> 
        mutate(
          label = str_remove(label, "Estimate!!")
        )
    ) |> 
    mutate(
      subcategory = 
        case_when(
          variable == "B25087_001" ~ "n_house",
          # monthly cost
          str_detect(variable, "B25087_01[6789]") ~ "cost_higher",
          str_detect(variable, "B25087.*") ~ "cost_to_2499",
          # housing price
          str_detect(variable, "B25096_0.[345]") ~ "price_under_150K",
          str_detect(variable, "B25096_0.[6]") ~ "price_150_299K",
          str_detect(variable, "B25096_0.[78]") ~ "price_300_499K",
          str_detect(variable, "B25096.*") ~ "price_higher",
          .default = category
        )
    ) |> 
    summarise(
      .by = c(fips, name, category, subcategory),
      n = sum(estimate)
    ) |> 
    mutate(
      .by = c(fips, category),
      pct = n / sum(n)
    )
  
  prep_df |> 
    select(-category) |> 
    pivot_wider(
      names_from = subcategory,
      values_from = c(n, pct)
    ) |> 
    mutate(
      pct_poverty = n_poverty / n_population,
      pct_lgbt = n_lgbt / n_population
    ) |> 
    select(-pct_population)
}

county_acs <- 
  county_estimates |> 
  aggregate_census() |> 
  rename(
    county_fips = fips,
    county_name = name
  ) 

subcounty_acs <- 
  subcounty_estimates |> 
  aggregate_census() |> 
  rename(
    subcounty_fips = fips,
    subcounty_name = name
  ) 

# Save ----
write_csv(county_acs, "output/census-county-2024.csv")
write_csv(subcounty_acs, "output/census-subcounty-2024.csv")

