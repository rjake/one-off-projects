# Workspace ----
library(glue)
library(tidyverse)
library(rvest)

area <- "105855459" # North Carolina
output_folder <- "areas/WV"
output_raw_routes <- glue("{output_folder}/all_routes.csv")
output_descriptions <- glue("{output_folder}/scrape_descriptions.csv")


make_url <- function(export = TRUE,
                     type = NULL,
                     min_rock = NULL,
                     max_rock = NULL,
                     min_boulder = NULL,
                     max_boulder = NULL,
                     min_aid = NULL,
                     max_aid = NULL,
                     min_ice = NULL,
                     max_ice = NULL,
                     min_mixed = NULL,
                     max_mixed = NULL,
                     is_trad_climb = NULL,
                     is_sport_climb = NULL,
                     is_top_rope = NULL,
                     stars = NULL,
                     pitches = NULL,
                     sort1 = NULL,
                     sort2 = NULL) {
  
  base <- "https://www.mountainproject.com/route-finder"
  if (export) {
    base <- paste0(base, "-export")  
  }
  
  get_param_name <- function(x) {
    ifelse(
      grepl("^(min|max)", x), 
      gsub("^(m)(in|ax)_", "diff\\U\\1\\L\\2", x, perl = TRUE),
      gsub("_", "", x)
    )
  }
  
  args <- as.list(match.call())[-1]
  args$export <- NULL
  
  pairs <- 
    paste0(
      get_param_name(names(args)), "=", unlist(args)
    )
    
  params <- paste(pairs, collapse = "&")
  
  paste0(base, "?", params)
}

## url_list ----
make_url(type = "boulder", min_boulder = "20000", max_boulder = "20350", export = FALSE)

url_list <-
  list(
    boulder_1 = make_url(type = "boulder", min_boulder = "20000", max_boulder = "20350"),
    boulder_2 = make_url(type = "boulder", min_boulder = "20350", max_boulder = "21700"),
    sport = make_url(is_sport_climb = 1),
    toprope = make_url(is_top_rope = 1),
    trad_1 = make_url(max_rock = "2500", is_trad_climb = 1),
    trad_2 = make_url(min_rock = "2600", max_rock = "3500", is_trad_climb = 1),
    trad_3 = make_url(min_rock = "4600", is_trad_climb = 1)
  ) |> 
  map(
    ~paste0(.x, "&selectedIds=", area)
  )


download_data <- function(url, file_id) {
  raw_data <- read_csv(url)

  df <-
    raw_data |>
    janitor::clean_names() |>
    select(-your_stars) |>
    rename(
      x = area_longitude,
      y = area_latitude
    )
  
  #print(glue("{output_folder}/raw_{file_id}.csv"))
  write_csv(df, glue("{output_folder}/raw/{file_id}.csv"))
}


# Get data ----
if (!file.exists(output_raw_routes)) {
  walk(
    .x = seq_along(url_list),
    ~download_data(
      url = url_list[[.x]],
      file_id = names(url_list[.x])
    )
  )
}

# Clean data ----
raw_routes <-
  file.path(output_folder, "raw") |> 
  list.files(full.names = TRUE) |> 
  map(
    ~read_csv(.x, id = "url_query") |> 
      mutate_all(as.character)
  )

final_routes <-
  list_rbind(raw_routes) |> 
  select(-url_query) |> 
  distinct() |> 
  mutate(
    .before = everything(),
    route_id = str_extract(url, "\\d{5,}")
  )

# remove dupes of items that are are in multiple files (ex. TR + Sport)
final_routes |> 
  write_csv(output_raw_routes)



# Download Descriptions ----
all_routes <- read_csv(output_raw_routes)
# original_routes <- read_csv(output_raw_routes)
# existing_routes <- read_csv("WV/raw_scrape_descriptions.csv")
all_routes <-
  original_routes |>
  filter(!route_id %in% existing_routes$route_id) |> 
  select(url)
  #read_csv(output_raw_routes)

scrape_mp <- function(route_url) {
  file_exists <- file.exists(output_descriptions)
  # route_url <- all_routes$url[1]
  url_nodes <-
    route_url |>  
    read_html() |>  
    html_nodes("div.mt-2") |>  
    html_text()
  
  deets <-
    url_nodes |> 
    str_replace_all("^(\\s*)", "") |> 
    str_replace_all("(\\s*)$", "") |> 
    str_replace_all("(\\s*\\n\\s*)", "`") |> 
    trimws()
  
  df <-
    tibble(
      route_id = str_extract(route_url, "\\d{5,}"),
      info = deets
    ) |> 
    separate_wider_delim(
      info,
      delim = "`",
      names = c("field", "value"),
      too_few = "align_start",
      too_many = "drop",
      cols_remove = FALSE
    )
  
  write_csv(
    df,
    output_descriptions,
    append = file_exists
  )
}

if (FALSE) { 
  # 0.2 * nrow(all_routes) / 60 #approx time (mins)
  
  walk(
    all_routes$url,
    possibly({
      Sys.sleep(0.2)
      ~scrape_mp(.x)
    }),
    .progress = TRUE
  )
}
