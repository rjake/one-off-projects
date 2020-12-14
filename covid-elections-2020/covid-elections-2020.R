library(tidyverse)
library(glue)
library(urbnmapr) # devtools::install_github("UrbanInstitute/urbnmapr")

raw_election <- read_csv(
  "https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv"
) %>% as_tibble()

raw_covid <- read_csv(
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
) %>% as_tibble()


covid_last_week <-
  raw_covid %>% 
  drop_na() %>% 
  group_by(fips) %>% 
  filter(date >= Sys.Date() - 7) %>% 
  summarise(
    cases = range(cases) %>% diff(),
    deaths = range(deaths) %>% diff()
  ) %>% 
  ungroup() %>% 
  mutate(state_fips = str_sub(fips, 1, 2))


election_totals <-
  raw_election %>% 
  transmute(
    fips = county_fips,
    state_fips = str_sub(fips, 1, 2),
    state = state_name,
    county = county_name,
    pct_gop = per_gop,
    n_dem = votes_dem,
    n_gop = votes_gop,
    n_other = total_votes - n_dem - n_gop,
    total_votes
  )


counties <- 
  urbnmapr::countydata %>% 
  select(fips = county_fips, pop = hhpop) %>% 
  mutate(state_fips = str_sub(fips, 1, 2))

states <-
  urbnmapr::statedata %>% 
  select(state_fips, state_name, pop = hhpop)

county_totals <-
  counties %>% 
  left_join(election_totals) %>% 
  left_join(covid_last_week) %>% 
  mutate(
    case_rate = (cases / pop) * 1e5,
    death_rate = (deaths / pop) * 1e5
  ) %>% 
  drop_na()



state_totals <- local({
  election_state <-
    election_totals %>% 
    group_by(state_fips) %>% 
    summarise(
      total_dem = sum(n_dem),
      total_gop = sum(n_gop),
      total_votes = sum(total_votes)
    ) %>% 
    ungroup() %>% 
    mutate(pct_gop = total_gop / total_votes)
  
  covid_state <-
    covid_last_week %>% 
    group_by(state_fips) %>% 
    summarise(
      cases = sum(cases),
      deaths = sum(deaths)
    ) %>% 
    ungroup()
  
  states %>% 
    left_join(election_state) %>% 
    left_join(covid_state) %>%
    mutate(
      case_rate = (cases / pop) * 1e5,
      death_rate = (deaths / pop) * 1e5
    ) 
})

cor_county <- round(cor(county_totals$pct_gop, county_totals$death_rate), 2)
cor_state <- round(cor(state_totals$pct_gop, state_totals$death_rate), 2)

dates_run <- 
  (Sys.Date() - c(7, 0)) %>% 
  format("%m/%d/%y") %>% 
  paste(collapse = " - ")

phrase <- "with a higher republican margin of victory had higher \ndeath rates in the past week"

county_totals %>% 
  filter(death_rate > 0) %>% 
  ggplot(aes(pct_gop, death_rate)) +
  geom_point(
    aes(
      text = glue("{county}, {state}"),
      color = pct_gop < 0.5,
      size = deaths
    ),
    alpha = 0.5
  ) +
  geom_smooth(method='lm', formula = y ~ x, color = "black", se = FALSE) +
  ylim(0, 200) +
  theme(panel.background = element_rect(fill = "white", color = "grey80")) +
  labs(
    title = glue("Counties {phrase}"),
    subtitle = glue("{dates_run}, R2 = {cor_county}"),
    size = "Deaths",
    color = "More Rep.",
    x = "% GOP",
    y = "Death Rate per 100K"
  )

plotly::ggplotly()


ggplot(state_totals, aes(pct_gop, death_rate)) +
  geom_smooth(method='lm', formula = y ~ x, color = "black", se = FALSE) +
  geom_point(
    aes(
      text = state_name,
      fill = pct_gop < 0.5,
      size = deaths
    ),
    alpha = 0.6, shape = 21
  ) +
  theme(panel.background = element_rect(fill = "white", color = "grey80")) +
  labs(
    title = "States with a higher republican margin of victory have higher death rates",
    subtitle = glue("{dates_run}, R2 = {cor_state}"),
    size = "Deaths",
    fill = "More Rep.",
    x = "% GOP",
    y = "Death Rate per 100K"
  )
  

plotly::ggplotly()
