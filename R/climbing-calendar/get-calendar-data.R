library(ical)
options(lubridate.week.start = 1)

ical_feed <- 
  list(
    d_c = "https://app.rockgympro.com/ical/public/0d6c7fa257084fd3b9628ca1759d88eb",
    d_f = "https://app.rockgympro.com/ical/public/47ed069d575c4005aa50ab205c49c1be",
    m_c = "https://app.rockgympro.com/ical/public/ff72f7c8859742ada90d47dfdf1bbb97",
    m_f = "https://app.rockgympro.com/ical/public/ddfa32c028e24859a68ef61cecda5ed9",
    r_c = "https://app.rockgympro.com/ical/public/7fa510dd88f746378b4e0d79da753e70",
    r_f = "https://app.rockgympro.com/ical/public/7905f67632b8482b9d15bfd1b7beef09",
    s_c = "https://app.rockgympro.com/ical/public/8a61329b24414b9e9f5313d64661c0c8",
    s_f = "https://app.rockgympro.com/ical/public/9c2215701f70418ea1ab25009b361e50"
  ) |> 
  set_names(
    ~str_replace_all(
      .x, 
      c(
        "^d" = "durham",
        "^m" = "morrisville",
        "^r" = "raleigh",
        "^s" = "salvage_yard",
        "c$" = "climbing",
        "f$" = "fitness",
        "_" = "."
      )
    )
  )


read_calendar <- function(feed) {
  # feed <- ical_feed$durham.climbing
  id <- names(ical_feed)[which(ical_feed == feed)]
  
  df <- 
    ical_parse_df(feed) |> 
    as_tibble()
  
  df |> 
    mutate(
      .before = everything(),
      location = str_extract(id, "^\\w+"),
      type = str_extract(id, "\\w+$")
    )
}


all_cals <- map_dfr(ical_feed, ~read_calendar(.x))


clean_cals <- 
  all_cals |> 
  mutate(
    start = 
      if_else(
        hour(start) == 0,
        start %m+% hours(18),
        start
      )
  ) |> 
  transmute(
    location,
    type, 
    date = as.Date(start),
    time = format(start, "%R"),
    hour = hour(start),
    summary = 
      summary |> 
      tolower() |> 
      str_remove_all(" \\|.*") |> 
      str_replace_all(" and ", " & ") |> 
      str_replace_all("-|:", " ") |> 
      str_remove_all("(?<=run club|bouldering league).*"),
    description = tolower(description)
  )


of_interest <- 
  clean_cals |> 
  filter(
    !str_detect(
      summary,
      "free pt neck|guest hours|guided.*experience|parents|run club|sauna closure|teen climb|vinyasa|vin to yin|women|yoga"
    ) |
      str_detect(summary, "doom|acro")
  )


prep_plot <- 
  of_interest |> 
  filter(
    month == "Oct"
  ) |> 
  add_count(summary, name = "n_event") |> 
  complete(
    date = 
      seq.Date(
        floor_date(today(), "week") |> as.Date(), 
        max(prep_plot$date), 
        by = "day"
      ),
    fill = list(
      type = "climbing",
      location = "durham"
    )
  ) |> 
  mutate(
    n_event = replace_na(n_event, 10),
    n_event = map_int(n_event, ~min(c(10, .x))),
    day = wday(date, label = TRUE),
    month = month(date, label = TRUE),
    week = floor_date(date, "week") |> as.Date(),
    facet_x = day,
    facet_y = week,
    x = str_extract(location, "^\\w") |> toupper(),
    label = 
      glue::glue(
        t = str_remove(time, "^0"),
        s = str_extract(summary, "([\\w&]+( |\\b)){1,3}") |> trimws() |> str_wrap(12),
        "{t}\n{s}"
      ) |> 
      str_remove("^NA\nNA$"),
    fill = location,#hour < 12,
    alpha = 1 - (n_event / max(n_event))
  )


prep_plot |> 
  arrange(date) |> 
  slice(1) |> 
  t()


prep_plot |>
  filter(
    (hour < 9 | hour > 17) | day %in% c("Sat", "Sun")
    # location == "durham",
    # type == "climbing"
  ) |> 
  arrange(time) |> 
  mutate(
    .by = c(location, date),
    y = row_number()
  ) |> 
  ggplot(
    aes(x = x, y = y)
  ) +
  facet_grid(
    rows = vars(facet_y),
    cols = vars(facet_x),
    scales = "free_y",
    space = "free"
  ) +
  scale_y_reverse(
    expand = expansion(add = 0.5)
  ) +
  geom_text(
    aes(x = "M", y = -0.25, label = glue("{day} {format(date, '%m/%d')}")),
    hjust = 0
  ) +
  geom_label(
    aes(
      label = label,
      fill = type,
      alpha = -n_event
    ),
    label.size = NA,
    lineheight = 0.75,
    size = 2.8
  ) +
  geom_rect(
    data = ~filter(.x, date == today()), 
    fill = NA, 
    colour = "black", 
    linewidth = 2,
    xmin = -Inf, xmax = Inf,
    ymin = -Inf,ymax = Inf
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.background = element_rect("white", "grey80"),
    panel.grid = element_blank(),
    strip.text = element_blank()
  ) +
  scale_fill_manual(values = c("cyan3", "orange")) +
  labs(
    x = NULL,
    y = NULL
  )
