library(tidyverse)
library(googlesheets4)
library(lubridate)
library(hms)
library(simplecolors)

#get data from googlesheets
raw_sleep <-
  read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1WGTL6_qZ0Un2NGK_lzkJRS6ycYTB-Yn-ZkmLZjVcccE/edit?usp=sharing",
    sheet = "Sheet2",
    skip = 1,
    col_types = "c",
    col_names = TRUE
  )


clean_sleep <-
  raw_sleep |> 
  filter(!str_detect(activity, "shower|notes|groggy|phone|lunch|duration")) |> 
  mutate(
    group = case_when(
      str_detect(activity, "dog") ~ "dog park",
      str_detect(activity, "dinner|snack") ~ "food",
      str_detect(activity, "teeth|pills|cody|tea") ~ "routine",
      TRUE ~ "bed"
    ),
    .after = activity
  ) |> 
  mutate(across(where(is.character), na_if, "")) |> 
  pivot_longer(
    cols = -c(activity, group),
    names_to = "date",
    values_to = "time",
    values_drop_na = TRUE
  ) |> 
  mutate(
    date = 
      paste0(date, "/2022") |> 
      mdy(),
    datetime = paste(date, time) |> ymd_hm(),
    time_adj = 
      case_when(
        str_detect(activity, "wake") ~ datetime + hours(24),
        str_detect(group, "dog|food") ~ datetime + hours(12),
        hour(datetime) < 10 ~ datetime + hours(24),
        TRUE ~ datetime + hours(12)
      ),
    x_axis = 
      update(
        time_adj,
        year = 2022,
        month = 1,
        mday = ifelse(as.Date(time_adj) == date, 1, 2)
      )
       #|>paste(hms(time_adj))
  ) |> 
  #filter(date == "2022-07-09") |> 
  print()


broken_sleep <-
  tibble(
    date =  c("2022-07-16"),
    start = c("01:57"),
    end =   c("05:40")
  ) |> 
  mutate(
    start = paste(date, start) |> ymd_hm(),
    end = paste(date, end) |> ymd_hm(),
    date = as.Date(date),
    across(c(start, end), update, year = 2022, month = 1, mday = 2)
  ) |> 
  print()


clean_sleep |> 
  ggplot(aes(x_axis, date, color = group)) +
  geom_vline(
    xintercept = c(
      ymd_hm("2022-01-01 23:00 EST"),
      ymd_hm("2022-01-02 00:00 EST")
    ),
    color = "grey80"
  ) +
  geom_line(
    aes(group = date),
    color = "grey80", linetype = "dotted"
  ) +
  geom_line(
    aes(group = paste(date, group)),
    data = ~filter(.x, group != "food")
  ) +
  geom_linerange(
    data = broken_sleep, 
    aes(
      x = start, xmin = start, xmax = end, 
      y = date, 
      group = date#, color = "broken"
    ),
    size = 2,
    color = "white", alpha = 0.8,
    show.legend = FALSE
  ) +
  geom_point() +
  scale_x_datetime(date_labels = "%H:%M") +
  scale_color_manual(
    values = c(
      "bed"      = sc("mutedred3"),
      "broken"   = sc("mutedred1"),
      "dog park" = sc("mutedviolet3"),
      "food"     = sc("mutedorange3"),
      "routine"  = sc("mutedgreen3")
    )
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank()
  ) +
  labs(x = NULL, y = NULL)
