# how are rates of testing related to confidence in case rates

library(tidyverse)
library(lubridate)
options(scipen = 999)

raw_data <-
  data.table::fread(
    "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
  ) |>
  filter(location == "United States") |>
  as_tibble()


us_population <- raw_data$population[1] # same for whole dataset

clean_data <-
  raw_data |>
  #select(where(~(mean(!is.na(.x)) > 0.75))) |>
  select(date, ends_with("smoothed")) |>
  rename_all(str_remove, "_smoothed") |>
  select(-new_vaccinations) |>
  drop_na() |>
  print()


covid_percents <-
  clean_data |>
  mutate(
    pct_cases = (new_cases / us_population),
    pct_tests = (new_tests / us_population),
    pct_deaths = (new_deaths / us_population),
  ) |>
  mutate(
    var = var(pct_cases),
    se = sqrt((pct_cases * (1 - pct_cases)) / new_tests), #var / sqrt(new_tests),
    moe = 1.96 * sqrt(se)
  ) |>
  print()

# √ [P (1-P) / n]  https://www.statisticshowto.com/probability-and-statistics/statistics-definitions/what-is-the-standard-error-of-a-sample/


covid_percents |>
  filter(date == "2020-12-01") |>
  select(new_cases, new_tests) |>
  as.matrix() |>
  prop.test()

get_confint <- function(x, y, name) {
  # x = 10; y = 100
  prop.test((x), (y), conf.level = 0.95, correct = FALSE) |>
    broom::tidy() |>
    select(
      pct = estimate,
      stat = statistic,
      conf.low,
      conf.high,
    ) |>
    rename_all(paste, name, sep = "_") |>
    mutate_all(~(.x))
}


covid_prop_test <-
  covid_percents |>
  #sample_n(50) |> arrange(date) |>
  #head(100) |>
  select(date, new_cases, new_tests) |>
  mutate(moe_cases = map2(new_cases, new_tests, get_confint, name = "case")) |>
  unnest(moe_cases) |>
  # mutate(moe_tests = map2(new_tests, us_population, get_confint, name = "test")) |>
  # unnest(moe_tests) |>
  print()


covid_moe <-
  covid_prop_test |>
  mutate(
    delta = (conf.high_case - pct_case),
    moe = scales::rescale(log(delta * 1000), c(min(pct_case), max(pct_case) )), # rescale delta
    moe_low = pct_case - moe,
    moe_high = pct_case + moe
  ) |>
  print()


set.seed(1); covid_moe |>
  #sample_frac(0.2) |> arrange(date) |>
  ggplot(aes(date, pct_case)) +
  geom_ribbon(
    aes(
      ymin = moe_low,
      ymax = moe_high
    ),
    fill = "white",
    color = "grey70"#,linetype = "dashed"
  ) +
  geom_line()
+
  coord_cartesian(ylim = c(0, 0.21), expand = expand_scale(add = 0))

##############################################

covid_percents |>
  ggplot(aes(date, pct_tests)) +
  geom_col()



clean_data |>
  #arrange(new_cases) |>
  #mutate(ord = row_number()) |>
  pull(new_cases) |>
  density()
  ggplot(aes(ord, new_cases)) +
  geom_point()


as.data.frame(spline(clean_data$new_cases))

clean_data |>
  mutate(
    spline = spline(new_cases, xout = 1:n() )$y
  ) |>
  ggplot(aes(date)) +
  geom_line(aes(y = new_cases)) +
  geom_line(aes(y = spline))

#############################################

covid_percents |>
  ggplot(aes(date, pct_cases)) +
  geom_line() +
  geom_line(aes(y = pct_tests))


#############################################












covid_moe |>
  ggplot(aes(date, pct_case)) +
  geom_ribbon(
    aes(
      ymin = conf.low_case,
      ymax = conf.high_case
    ),
    color = "blue",
    alpha = 0.8
  ) +
  geom_line()


covid_percents |>
  ggplot(aes(date, new_cases)) + geom_point()



covid_percents |>
  ggplot(aes(pct_cases, pct_tests)) +
  geom_path() +
  geom_point(aes(color = date), size = 1)

covid_moe |>
  ggplot(aes(date, pct_case)) +
  # geom_ribbon(
  #   aes(
  #     ymin = pct_cases - moe,
  #     ymax = pct_cases + moe
  #   ),
  #   alpha = 0.2
  # ) +
  geom_line()




clean_data |>
  filter(date >= "2020-06-01") |>
  ggplot(aes(new_cases, new_deaths, color = date)) +
  geom_point()


clean_data |>
  filter(date >= "2020-06-01") |>
  pivot_longer(
    cols = -date,
    names_to = "metric",
    values_to = "n",
    values_drop_na = TRUE
  ) |>
  group_by(
    date = floor_date(date, "week"),
    metric
  ) |>
  summarise(
    n = sum(n),
    avg = mean(n)
  ) |>
  ungroup() |>
  ggplot(aes(date, avg, fill = metric)) +
  geom_line() +
  facet_grid(metric~., scales = "free")








# https://covidtracking.com/data/download
# also: https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/ac8783bc025f3a24bebcadbc640a78462ae34a0d/data/states_current.csv
#
raw_data <- data.table::fread("~/../Downloads/all-states-history.csv")

unique(raw_data$state)

clean_data <-
  raw_data |>
  arrange(date) |>
  rename_all(str_replace_all, "([A-Z])", "_\\1") |>
  rename_all(tolower) |>
  #filter(state == "AK") |>
  #select(where(~(sum(!is.na(.x)) > 0.5))) |>
  select(date, state, matches("increase")) |>
  select(date, state, matches("death|positive|test_results"))


clean_data |>
  pivot_longer(-c(date, state), names_to = "metric", values_to = "n") |>
  group_by(
    date = floor_date(date, "week"),
    metric
  ) |>
  summarise(
    n = sum(n),
    avg = mean(n)
  ) |>
  ungroup() |>
  ggplot(aes(date, avg, fill = metric)) +
  geom_line() +
  facet_grid(metric~., scales = "free")
