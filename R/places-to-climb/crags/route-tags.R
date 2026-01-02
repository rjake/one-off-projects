# Mountain Project ----
"-----------------------------------------------------------------
Download routes descriptions from mountain project:
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

output_folder <- "NC"
output_raw_routes <- glue("{output_folder}/all_routes.csv")
output_descriptions <- glue("{output_folder}/raw/scrape_descriptions.csv")
output_final_metadata <- glue("{output_folder}/final_route_metadata.csv")

all_routes <- read_csv(output_raw_routes, col_types = c(route_id = "c"))

all_descriptions <- 
  read_csv(
    output_descriptions, 
    col_names = c(
      "route_id",
      "field",
      "value", 
      "orig"
    ),
    col_types = c(route_id = "c")
  ) |> 
  select(-orig)


location_rev <- function(x) {
  # x <- "Mule Hollow Wall > Big Cottonwood Canyon > Central Wasatch > Wasatch Range > Utah"
  locations <- str_split_1(x, " > ") |> rev()
  locations[-1] |>
    paste0(collapse = " > ")
}

prep_routes <-
  all_routes |> 
  mutate(
    .before = everything(),
    location = map_chr(location, ~location_rev(.x))
  ) |>
  relocate(y, .after = x)
  
clean_routes <-
  prep_routes |>
  #filter(str_detect(location, "Nevada.*Red Rocks")) |>
  mutate(
    #.keep = "used",
    route_id = str_extract(url, "\\d{5,}"),
    regions = str_extract(location, "^[^>]+(> [^>]+){0,2}") |> trimws()
  ) |>
  separate_wider_delim(
    regions,
    delim = " > ",
    names = c("region_1", "region_2", "region_3", "region_4"),
    too_few = "align_start",
    too_many = "drop"
    ) |>
  mutate(
    region_1 = replace_na(region_1, "-"),
    region_2 = replace_na(region_2, "-"),
    region_3 = replace_na(region_3, "-")
    ) |>
  group_by(region_1, region_2, region_3) |>
  arrange(location, x) |>
  mutate(x_diff = x - lag(x, default = first(x))) |>
  arrange(location, y) |>
  mutate(y_diff = y - lag(y, default = first(y))) |>
  ungroup() |>
  arrange(location) |>
  relocate(route_id)

route_types <-
  clean_routes |> 
  select(route_id, route_type) |> 
  separate_longer_delim(route_type, ", ") |> 
  filter(
    !str_detect(route_type, "(?i)^(aid|alpine|ice|snow)") # 14 routes
  ) |> 
  mutate(
    route_type = tolower(route_type) |> paste0("_ind"),
    ind = 1
  ) |> 
  pivot_wider(
    id_cols = route_id,
    names_from = route_type,
    values_from = ind,
    values_fill = 0
  ) |> 
  mutate(
    .after = boulder_ind,
    rope_ind = as.integer(boulder_ind == 0)
  )
  
summarise(route_types, across((-route_id), sum))

# Analysis ----

# * prep_desc ----
desc_wide <-
  all_descriptions |>
  mutate(
    field = tolower(field) |> str_remove_all(":")
  ) |> 
  filter(
    str_detect(field, "description|location|protection")
  ) |> 
  # there is 1 route that has repeated sections
  slice(.by = c(route_id, field), 1) |> 
  pivot_wider(
    names_from = field,
    values_from = value,
    values_fill = "-"
  )

prep_desc <-
  all_descriptions |> 
  filter(field == "Description") |> 
  left_join(
    clean_routes |> transmute(route_id = route_id, route)
  ) |> 
  distinct(
    route_id,
    route,
    text = glue("{route}: {value}") |> tolower()
  )


# * route_terms ----
route_terms <-
  list(
    features = 
      "ar[eê]te|big wall|chimney|crack|crimp|cruis(e|ing)|crux|dihedral|expos(ed|ure)|\\bface\\b|flake|hueco|jug|ledge|pocket|roof|slab|slop[ey](r)?|traverse",
    good =
      "beautiful|best|classic|cool|eas(ier|y)|excellent|fun|(?<!not )great|(?<!not )good|interesting|nice|scenic|warm.?up",
    bad =
      "(?<!not (too )?)bad\\b|careful|choss|crumbly|danger|dirty|friable|not good|run[ \\-]*out\\b|rust|sandy",
    skill = 
      "balanc(e|ing|y)|burly|dyn(o|amic)|\\bjam\\b|mantle|power(ful)\\b|tech(nical|y)|(heel|toe).?hook",
    other = 
      "difficult|high \\w+ bolt|pump(y)?\\b|scary|smear|spooky|stick[ \\-]clip|thin|tough|tricky"
  )


raw_desc_tags <-
  prep_desc |>
  # head() |>
  summarise(
    .by = c(route_id, text),
    tag =
      map(
        text,
        ~ str_extract_all(
          .x, 
          glue_collapse(route_terms, sep = "|")
        )[[1]] |>
          unique() |>
          sort()
      )
  )


desc_tags <-
  raw_desc_tags |> 
  unnest_longer(tag, keep_empty = TRUE) |> 
  arrange(route_id, tag) |> 
  mutate(
    category =
      case_when(
        str_detect(tag, route_terms$features) ~ "feature_tags",
        str_detect(tag, route_terms$good) ~ "good_tags",
        str_detect(tag, route_terms$bad) ~ "bad_tags",
        str_detect(tag, route_terms$skill) ~ "skill",
        str_detect(tag, route_terms$other) ~ "other_tags"
      )
  )

count(desc_tags, category, sort = TRUE)
count(desc_tags, category, tag) |> print(n = Inf)

desc_tags |> 
  filter(is.na(category) | tag == "") |> 
  view()

clean_tags <-
  desc_tags |> 
  drop_na() |> 
  #head(20) |> 
  #select(-text) |> 
  mutate(
    tag =
      tag |> 
      str_replace_all("-", " ") |> 
      recode(
        "arête" = "arete",
        "balancy" = "balance",
        "balancing" = "balance",
        "exposure" = "exposed",
        "cruising" = "cruise",
        "pump" = "pumpy",
        "rests" = "rest",
        "runout" = "run out",
        "slope" = "sloper",
        "slopy" = "sloper",
        "toehook" = "toe hook",
        "warmup" = "warm up"
      )
  ) |> 
  distinct()

clean_tags |> 
  select(-text) |> 
  write_csv(glue("{output_folder}/route_tags.csv"))


clean_tags |>
  count(category, tag) |> 
  # filter(str_detect(category, "feature|other")) |> 
  view()


clean_tags_wide <-
  clean_tags |> 
  summarise(
    .by = c(route_id, text, category),
    tag = glue_collapse(tag, ", ")
  ) |>
  arrange(route_id, category, tag) |> 
  pivot_wider(
    names_from = category,
    values_from = tag,
    values_fill = "-"
  )


as_indicator <- function(x) {
  x |> 
    as.integer() |> 
    replace_na(0) 
}


route_tags <-
  clean_tags_wide |> 
  mutate(
    bad_ind = (bad_tags != "-"),
    good_ind = (good_tags != "-"),
    fun_ind = str_detect(good_tags, "excellent|fun|great"),
    pretty_ind = str_detect(good_tags, "beautiful|scenic")
    
  ) |> 
  mutate(
    across(
      ends_with("_ind"), ~as_indicator(.x)
    )
  )


route_info <-
  clean_routes |> 
  mutate(
    .after = rating,
    rating_simple = 
      rating |> 
      tolower() |> 
      str_replace("v-easy", "v0") |>
      str_replace("(?<=v)(\\d)\\-(\\d)", "\\2") |> 
      str_replace("([abc])/([bcd])", "\\2") |> 
      str_replace("(5\\.1\\d) ([a-d])(.*)", "\\1\\2") |> 
      str_replace("5\\.1\\d\\-", "5.10a") |> 
      str_replace("5\\.1\\d\\+", "5.10d") |> 
      str_remove_all("[/ \\+\\-].*"),
    rating_int = 
      rating_simple |> 
      str_remove_all("[\\+\\-abcd ].*") |> 
      str_remove("^(v|5\\.)")
  ) |> 
  # count(rating, rating_simple, rating_int) |> .print_n()> 
  #distinct(rating, rating_simple) |> arrange(rating_simple)
  select(-c(route_type)) |> 
  left_join(desc_wide |> select(-location)) |> 
  left_join(route_tags) |> 
  left_join(route_types)


write_csv(route_info, output_final_metadata)

