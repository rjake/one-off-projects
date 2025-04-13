# Mountain Project ----
"-----------------------------------------------------------------
Download popular routes from mountain project:
1) Get xml of states IDs. 
  Go to: https://www.mountainproject.com/route-finder#settings 
    > Location Change > inspect source > copy element & save to xml file

2) Loop through state IDs to find the top 1000 routes in that state: 
  Top routes = Sport 5.9, 1 pitch, 2+ stars

3) Union all states into 1 file

------------------------------------------------------------------"


# Workspace ----
setwd(dirname(.rs.api.getSourceEditorContext()$path))

library(tidyverse)
library(glue)

mp_html <- read_lines("https://www.mountainproject.com/area/classics")

mp_state_ids <-
  mp_html |> 
  grep(pattern = "/classics/", value = TRUE) |> 
  grep(pattern = "(International|Alaska|Hawaii)", value = TRUE, invert = TRUE) |> 
  as_tibble() |> 
  transmute(
    id = str_extract(value, '(?<=classics/)\\d+'),
    title = str_extract(value, '[^">]+(?=</a>)') 
  )


# Functions ----
mp_download_state <- function(id, type, overwrite = FALSE) {
  # id <- mp_state_ids$id[3]
  state_name <- mp_state_ids$title[mp_state_ids$id == id]
  file_name <- 
    glue(
      x = state_name |> janitor::make_clean_names(),
      "input/mountain-project/{type}/{x}.csv"
    )
  
  if (file.exists(file_name) & !overwrite) {
    return(invisible())
  }
  
  if (type == "rock") {
    file_location <- 
      glue(
        "https://www.mountainproject.com/route-finder-export\\
        ?selectedIds={id}\\
        &type=rock\\
        &diffMinrock=2300&diffMinboulder=20000\\
        &diffMaxrock=2500&diffMaxboulder=20050\\
        &diffMinaid=70000&diffMinice=30000&diffMinmixed=50000\\
        &diffMaxaid=75260&diffMaxice=38500&diffMaxmixed=65050\\
        &is_sport_climb=1\\
        &stars=0&pitches=1\\
        &sort1=popularity+desc&sort2=rating"
      )
  } else if (type == "boulder") {
    file_location <- 
      glue(
        "https://www.mountainproject.com/route-finder-export\\
        ?selectedIds={id}\\
        &type=boulder\\
        &diffMinrock=2300&diffMinboulder=20150\\
        &diffMaxrock=2500&diffMaxboulder=20250\\
        &diffMinaid=70000&diffMinice=30000&diffMinmixed=50000\\
        &diffMaxaid=75260&diffMaxice=38500&diffMaxmixed=65050\\
        &stars=2.8&pitches=0\\
        &sort1=popularity+desc&sort2=rating"
      )
  }
  
  df <- read_csv(file_location)
  
  df |> 
    janitor::clean_names() |> 
    select(-your_stars) |> 
    rename(
      x = area_longitude,
      y = area_latitude
    ) |> 
    mutate(
      .before = everything(),
      state = state_name,
      state_id = id
    ) |> 
    write_csv(file_name)
}


round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

location_rev <- function(x) {
  # x <- "Mule Hollow Wall > Big Cottonwood Canyon > Central Wasatch > Wasatch Range > Utah"
  locations <- str_split_1(x, " > ") |> rev()
  
  locations[-1] |> 
    paste0(collapse = " > ")
}


# Download all ----
if (FALSE) {
  walk(
    mp_state_ids$id,
    possibly(~mp_download_state(.x, type = "rock")),
    .progress = TRUE
  )
  
  walk(
    mp_state_ids$id,
    possibly(~mp_download_state(.x, type = "boulder")),
    .progress = TRUE
  )
  "Downloaded these manually
  Warning messages: SSL/TLS connection timeout Failed to open 
  1: 'https://www.mountainproject.com/route-finder-export?selectedIds=105708959&type=boulder&diffMinrock=2300&diffMinboulder=20150&diffMaxrock=2500&diffMaxboulder=20250&diffMinaid=70000&diffMinice=30000&diffMinmixed=50000&diffMaxaid=75260&diffMaxice=38500&diffMaxmixed=65050&stars=2.8&pitches=0&sort1=popularity+desc&sort2=rating'
  2: 'https://www.mountainproject.com/route-finder-export?selectedIds=105708956&type=boulder&diffMinrock=2300&diffMinboulder=20150&diffMaxrock=2500&diffMaxboulder=20250&diffMinaid=70000&diffMinice=30000&diffMinmixed=50000&diffMaxaid=75260&diffMaxice=38500&diffMaxmixed=65050&stars=2.8&pitches=0&sort1=popularity+desc&sort2=rating'
  3: 'https://www.mountainproject.com/route-finder-export?selectedIds=105852400&type=boulder&diffMinrock=2300&diffMinboulder=20150&diffMaxrock=2500&diffMaxboulder=20250&diffMinaid=70000&diffMinice=30000&diffMinmixed=50000&diffMaxaid=75260&diffMaxice=38500&diffMaxmixed=65050&stars=2.8&pitches=0&sort1=popularity+desc&sort2=rating'
  "
}

# Aggregate data ----
all_routes <- 
  list.files(
    "input/mountain-project/",
    recursive = TRUE,
    full.names = TRUE
  ) |> 
  read_csv()


final_routes <- 
  all_routes |> 
  mutate(
    .before = everything(),
    state = str_to_title(state),
    location = map_chr(location, ~location_rev(.x))
  ) |> 
  relocate(y, .after = x)

# Crag-gregate ----
crag_id <- 
  final_routes |> 
  #filter(str_detect(location, "Nevada.*Red Rocks")) |> 
  mutate(
    #.keep = "used",
    regions = str_extract(location, "^[^>]+(> [^>]+){0,2}") |> trimws()
  ) |> 
  separate_wider_delim(
    regions, 
    delim = " > ",
    names = c("region_1", "region_2", "region_3"),
    too_few = "align_start",
    too_many = "drop"
  ) |> 
  mutate(
    region_2 = replace_na(region_2, "-"),
    region_3 = replace_na(region_3, "-")
  ) |> 
  #filter(state == "North Carolina") |> 
  group_by(state, region_1) |> 
  arrange(location, x) |> 
  mutate(x_diff = x - lag(x, default = first(x))) |> 
  arrange(location, y) |> 
  mutate(y_diff = y - lag(y, default = first(y))) |> 
  arrange(location) |> 
  mutate(
    region_crag_id = cumsum(abs(x_diff) > 0.2 | abs(y_diff) > 0.2),
    crag = glue("{state} > {region_1} > {str_pad(region_crag_id, 2, pad = '0')}")
  ) |> 
  ungroup() 
  
# Save ----
write_csv(crag_id, "output/mountain-project-popular-routes.csv")


crag_stats <-
  crag_id |> 
  summarise(
    .by = c(state, crag, region_1),
    region_2 = first(region_2),
    example_location = first(region_3),
    x = mean(x),
    y = mean(y),
    n = n(),
    n_boulder = sum(str_detect(route_type, "Boulder")),
    n_routes = sum(str_detect(route_type, "Sport")),
    mean_rating = mean(avg_stars),
    median_rating = median(avg_stars)
  )

write_csv(crag_stats, "output/crag-stats.csv")
