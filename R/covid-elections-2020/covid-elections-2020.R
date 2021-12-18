# from project directory
setwd("covid-elections-2020")

library(tidyverse)
library(glue)
library(lubridate)
library(simplecolors)
library(zoo)
library(urbnmapr) # devtools::install_github("UrbanInstitute/urbnmapr")

county_pop <- read_csv("2019_county_population.csv")
state_pop <- read_csv("2019_state_population.csv")

county_election <- read_csv("2020_county_elections.csv")
state_election <- read_csv("2020_state_elections.csv")


raw_covid <-
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv" |>
  data.table::fread() |>
  as_tibble() |>
  mutate(
    fips =
      ifelse(county == "New York City", "36061", fips) |>
      str_pad( 5, "left", "0")
  )

# population
county_pop_totals <-
  county_pop |>
  mutate(
    county_name = ifelse(state_name == "Alaska", "(total)", county_name),
    county_fips = ifelse(state_name == "Alaska", "total", county_fips)
  ) |>
  count(
    county_fips, county_name, state_name,
    wt = county_pop,
    name = "county_pop"
  )


# elections -----
election_totals <-
  county_election |>
  left_join(county_pop_totals) |>
  drop_na(votes, county_pop) |>
  relocate(
    starts_with("state"), starts_with("county"), votes
  ) |>
  arrange(desc(pct_dem)) |>
  mutate(
    # rank counties by non-dem percentage
    rank_electorate = cumsum(votes) / sum(votes) * 100,
    electorate_category =  cut(rank_electorate, c(0, 25, 50, 75, 100))
  ) |>
  group_by(electorate_category) |>
  mutate(
    pct_dem_category =
      range(pct_dem) |>
      floor() |>
      paste(collapse = "-")
  ) |>
  ungroup() |>
  mutate(pct_dem_category = fct_reorder(pct_dem_category, pct_dem)) |>
  print()
#

# check breakdown
election_totals |>
  group_by(pct_dem_category) |>
  summarise(
    n_county = n(),
    n_votes = sum(votes) |> scales::number(big.mark = ","),
    n_pop = sum(county_pop) |> scales::number(big.mark = ",")
  )


# covid -----
covid_rollup <-
  raw_covid |>
  drop_na() |>
  group_by(date = floor_date(date, "month"), county, state, fips) |>
  summarise(
    new_cases = range(cases) |> diff(),
    new_deaths = range(deaths) |> diff()
  ) |>
  ungroup() |>
  mutate(
    county = ifelse(state == "Alaska", "(total)", county),
    fips = ifelse(state == "Alaska", "total", fips)
  ) |>
  group_by(date = floor_date(date, "month"), county, state, fips) |>
  summarise(
    new_cases = sum(new_cases),
    new_deaths = sum(new_deaths)
  ) |>
  ungroup()



county_totals <-
  covid_rollup |>
  rename(county_fips = fips) |>
  left_join(county_pop_totals) |>
  left_join(
    election_totals |>
      select(county_fips, state_fips, pct_dem, pct_dem_category)
  ) |>
  filter(county_pop >= 1000) |>
  drop_na() |>
  mutate(
    case_rate = (new_cases / county_pop) * 1e5,
    death_rate = (new_deaths / county_pop) * 1e5
  )

# check counts
count(county_election, state_fips, name = "n_election") |>
  full_join(
    county_totals |>
      distinct(county, state_name, state_fips) |>
      count(state_name, state_fips)
  )



state_totals <-
  county_totals |>
  group_by(state_fips, state_name, date) |>
  summarise(
    new_cases = sum(new_cases),
    new_deaths = sum(new_deaths)
  ) |>
  ungroup() |>
  left_join(state_election) |>
  left_join(state_pop) |>
  mutate(
    case_rate = (new_cases / state_pop) * 1e5,
    death_rate = (new_deaths / state_pop) * 1e5
  ) |>
  print()


# recent correlation plots ----
recent_county <-
  county_totals |>
  filter(date >= max(date))


recent_state <-
  state_totals |>
  filter(date >= max(date))

cor_county <- round(cor(1- recent_county$pct_dem, recent_county$death_rate), 2)
cor_state <- round(cor(1 - recent_state$pct_dem, recent_state$death_rate), 2)

dates_run <-
  c(max(state_totals$date), Sys.Date() - 1) |>
  format("%m/%d/%y") |>
  paste(collapse = " - ")

phrase <- "with a higher republican + independent margin of \nvictory have higher death rates"

recent_county |>
  filter(county_pop > 1000, new_deaths > 5) |>
  # filter(death_rate > 0) |>
  ggplot(aes(100 - pct_dem, death_rate)) +
  geom_point(
    aes(
      text = glue("{county_name}, {state}"),
      color = 100 - pct_dem < 50,
      size = new_deaths
    ),
    alpha = 0.5
  ) +
  geom_smooth(method='lm', formula = y ~ x, color = "black", se = FALSE) +
  ylim(0, NA) +
  theme(panel.background = element_rect(fill = "white", color = "grey80")) +
  labs(
    title = glue("Counties {phrase}"),
    subtitle = glue("{dates_run}, R2 = {cor_county}"),
    size = "Deaths",
    color = "More Rep.",
    x = "% GOP",
    y = "Monthly Death Rate per 100K"
  )

plotly::ggplotly()


state_totals |>
  filter(date >= max(date)) |>
  ggplot(aes(100 - pct_dem, death_rate)) +
  geom_smooth(method='lm', formula = y ~ x, color = "black", se = FALSE) +
  geom_point(
    aes(
      text = state_name,
      fill = 100 - pct_dem < 50,
      size = new_deaths
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
    y = "Monthly Death Rate per 100K"
  )


plotly::ggplotly()



# highest 4 counties ----
county_totals |>
  inner_join(
    recent_county |>
      filter(county_pop > 1000, new_deaths > 20) |>
      slice_max(death_rate, n = 4) |>
      select(county_fips)
  ) |>
  ggplot(aes(date, death_rate, color = county_name)) +
  geom_line() +
  geom_point() +
  facet_wrap(~county_name, scales = "free") +
  guides(color = FALSE)


# % of deaths ----
pct_of_deaths <-
  county_totals |>
  filter(date > "2020-04-01") |>
  group_by(date, pct_dem_category) |>
  summarise(new_deaths = sum(new_deaths)) |>
  ungroup() |>
  group_by(date) |>
  mutate(
    total_deaths = sum(new_deaths),
    pct_deaths = round(new_deaths / total_deaths, 4)
  ) |>
  ungroup() |>
  mutate(
    date_order = dense_rank(date),
    year = dense_rank(year(date))
  ) |>
  print()

# as facets ----
pct_of_deaths |>
  mutate(
    facet =
      paste0(pct_dem_category, "%") |>
      fct_reorder(as.numeric(pct_dem_category))
  ) |>
  ggplot(aes(date, pct_deaths, color = pct_dem_category)) +
  geom_line(size = 1) +
  geom_point(aes(size = new_deaths)) +
  facet_wrap(~ facet) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, NA)
  ) +
  scale_x_date(date_labels = "%m/%y") +
  scale_color_manual(values = sc("red4", "red2", "blue2", "blue4")) +
  guides(alpha = FALSE, color = FALSE) +
  labs(
    title = "Percent of deaths each month by political affiliation",
    subtitle = "Data was aggregated by counties and ordered by % voting for non-democratic candidates (republican + other). The population was then broken into 4 categories of equal size. This means 25% of the population lives in counties that voted between 62-92% for Joe Biden (~180 counties, 39M voters) and 25% of the population livies in counties where 3-39% voted for him (~2K counties, 39M voters)." |>
      str_wrap(140),
    x = NULL,
    y = "% of total deaths",
    size = "# Deaths"
  ) +
  theme(
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 9),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = "grey80")
  )

# as path ----
pct_of_deaths |>
  ggplot(aes(new_deaths, pct_deaths, color = pct_dem_category, alpha = year)) +
  geom_path(size = 1) +
  geom_point(aes(size = new_deaths)) +
  # geom_point(size = 2) +
  facet_wrap(~ pct_dem_category, scales = "free_x") +
  scale_color_manual(
    values = sc("red4", "red2", "blue2", "blue4")
  ) +
  scale_y_continuous(
    labels = scales::percent_format()#,limits = c(0, NA)
  ) +
  scale_x_continuous(
    labels = scales::number_format(suffix = "K", scale = 0.001, accuracy = 1),
    limits = c(0, NA)
  ) +
  scale_alpha(range = c(0.2, 1)) +
  #scale_alpha_binned(2) +
  guides(alpha = FALSE, color = FALSE) +
  labs(
    title = "Percent of deaths by month",
    x = "Monthly Deaths",
    y = "% of total deaths",
    size = "# Deaths"
  ) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = "grey80")
  )


# NOT USING ---------------
raw_election <-
  paste0(
    gh_path,
    "tonmcg/US_County_Level_Election_Results_08-20/master/",
    "2020_US_County_Level_Presidential_Results.csv"
  ) |>
  data.table::fread() |>
  as_tibble() |>
  mutate(county_fips = str_pad(county_fips, 5, "left", "0"))

election_totals <-
  raw_election |>
  transmute(
    fips = county_fips,
    state_fips = str_sub(fips, 1, 2),
    state = state_name,
    county = county_name,
    pct_gop = per_gop,
    n_dem = votes_dem,
    n_gop = votes_gop,
    n_other = total_votes - n_dem - n_gop,
    total_votes,
    pct_dem = (n_dem / total_votes)
  ) |>
  left_join(counties) |>
  arrange(pct_dem) |>
  mutate(
    pct_electorate = row_number() / n(),
    electorate_category =  cut(pct_dem * 100, c(0, 25, 50, 75, 100))
  ) |>
  group_by(electorate_category) |>
  mutate(
    pct_dem_category =
      range(pct_dem * 100) |>
      floor() |>
      paste(collapse = "-")
  ) |>
  ungroup() |>
  mutate(pct_dem_category = fct_reorder(pct_dem_category, pct_dem)) |>
  print()

