# find all locally ###########################################################
# inspired by https://vincentarelbundock.github.io/Rdatasets/datasets.html
setwd(dirname(.rs.api.getSourceEditorContext()$path))
library(tidyverse)
library(glue)


top_100 <- cranlogs::cran_top_downloads(when = "last-month", count = 100) # 3/1/2023

use_packages <- c(
  "datasets",
  "AER",
  "carData",
  "dplyr",
  "Ecdat",
  "forcats",
  "ggplot2",
  "HistData",
  "ISLR",
  "lubridate",
  "MASS",
  "openintro",
  "survival",
  "tidyr",
  "usdata",
  "wooldridge"
)

# check that all are available
if (!all(use_packages %in% .packages(all.available = TRUE))) {
  setdiff(use_packages, .packages(all.available = TRUE)) |> 
    glue_collapse("', '") |> 
    glue(x = _, "these packages are missing: c('{x}')") |> 
    stop()
}

base_data <-
  data(package = use_packages)$results |>
  as_tibble() |> 
  rename_all(tolower) |>
  select(-libpath) |> 
  mutate(
    item = str_extract(item, "^[\\w\\.]+"),
    pkg_data = glue("{package}::{item}")
  ) |> 
  distinct() |> 
  mutate(
    origin = case_when(
      package == "datasets" ~ "base",
      package %in% tidyverse_packages() ~ "tidyverse",
      TRUE ~ "other"
    )
  ) |> 
  print()



get_data_info <- function(x) {
    max_rows <- 100
    max_cols <- 20
    
    pkg <- str_extract(x, ".*(?=::)")
    item <- str_extract(x, "(?<=::).*")
    
    # need this weird logic for data like AER::BankWages
    data(list = item, package = pkg)
    df <- get(item)
    rm(list = item, envir = globalenv())
    
    # metadata
    item_class <- class(df)
    is_df <- inherits(df, "data.frame") & !inherits(df, "sf")
    dims <- dim(df) |> paste(collapse = " x ")
    is_trunc <- TRUE
    
    if (!is_df) {
      return(NULL)
    } 
    # else if (nrow(df) < max_rows & ncol(df) <= max_cols) {
    #   use_data <- df
    #   is_trunc <- FALSE
    # } else {
    #   use_data <- 
    #     head(df, max_rows) |> 
    #     mutate(across(where(is.character), as.factor)) |> 
    #     mutate(across(where(is.factor), fct_drop)) |> 
    #     select(1:min(ncol(df), max_cols)) # max of n columns
    #   
    #   attr(use_data, "row.names") <- row.names(use_data) |> head(max_rows)
    #   attr(use_data, "na.action") <- NULL # can cause to be large size ex 
    #   attr(use_data, "spec") <- NULL # comes in with some tibbles, ex openintro::hfi
    # }
    
    class_types <- map_chr(df, ~pluck(class(.x), 1))

    tibble(
      pkg_data = x,
      pkg = pkg,
      data = item,
      #data = use_data,
      #is_trunc = is_trunc,
      dim = as.character(dims),
      n_row = nrow(df),
      n_col = ncol(df),
      class = item_class[1],
      n_discrete = sum(class_types %in% c("character", "factor", "ordered")),
      n_numeric = sum(class_types %in% c("numeric", "integer", "labelled")),
      n_date = sum(class_types %in% c("Date", "POSIXct", "dttm")),
      n_logic = sum(class_types %in% c("logical")),
      n_other = n_col - n_discrete - n_numeric - n_date - n_logic
    )
}


get_data_info(x = "ggplot2::diamonds")
get_data_info(x = "ggplot2::economics")
get_data_info(x = "ggplot2::economics_long")
get_data_info(x = "openintro::hfi")
get_data_info(x = "AER::BankWages") # not actually available when called this way
get_data_info(x = "openintro::antibiotics") # not truncated
get_data_info(x = "openintro::ucla_f18") # has logic
get_data_info(x = "stringr::sentences") # not df
get_data_info(x = "datasets::Seatbelts") # not df


ds_obj <-
  map_dfr(
    .x = base_data$pkg_data, 
    .f = possibly(get_data_info, otherwise = NULL)
  )


# # check sizes
# map(ds_obj, pluck, "data") |> 
#   map_df(object.size) |> 
#   gather() |> 
#   arrange(desc(value))

# sum(.Last.value$value)

# list_details <- 
#   ds_obj |> 
#   map( ~tidyselect:::select(.x, -data)) |> 
#   map_dfr(flatten)


# prep_details <-
#   ds_obj |> 
#   mutate(n_other = n_col - n_discrete - n_numeric - n_date - n_logic) |>
#   print()


data_details <- 
  base_data |> 
  rename(
    pkg = package,
    name = item
  ) |> 
  inner_join(ds_obj) |> #count(package, sort = TRUE) |> 
  arrange(name) |> 
  print()


data_details |> 
  count(pkg) |> 
  print(n = Inf)


write_csv(data_details, "data_details.csv")

# ds_list <- 
#   ds_obj[data_details$pkg_data] |> 
#   map(~tidyselect:::select(.x, data))
#   
# saveRDS(ds_list, "ds_list.Rds")
#





# PRIOR METHOD -----------------------------------------------------------------
# 
# # ds_obj <- # 10,190,456 bytes
#   ds_obj |>map(~modify_at(.x, .at = "data", ~if (inherits(.x, "data.frame")) head(.x, 500))) 

# * all_data ----
all_data <- 
  data(package = .packages(all.available = TRUE))$results |>
  as_tibble() |> 
  transmute(
    pkg = 
      str_replace(Package, "datasets", "base") |> 
      fct_infreq() |> 
      fct_relevel("base", after = 0),
    name = str_remove(Item, " .*"), 
    description = Title
  ) |> 
  filter(
    Package %in% c(
      "datasets", 
      "boot",
      "Ecdat",
      "MASS",
      "fds",
      "carData",
      "survival",
      tidyverse::tidyverse_packages()
      | name %in% c("acidity")
    )
  ) |> 
  #head(15) |> 
  arrange(name) |> 
  print()

# ds_env <- asNamespace("datasets")
# ds_env_list <- as.list(ds_env$.__NAMESPACE__.$lazydata) #|> head(10)
# ds_obj <- ds_env_list[sort(names(ds_env_list))]

# * ds_obj ----
ds_obj <- 
  map(
    .x = all_data$name,
    ~{
      df <- rlang::parse_expr(.x) |> eval()
      if (inherits(df, "data.frame")) df <-head(df, 500)
      df
    }
  ) |>
  set_names(all_data$name)


# * ds_info ----
ds_info <-
  tibble(
    name = names(ds_obj),
    class = map(ds_obj, class) |> map_chr(pluck, 1),
    dim = map(ds_obj, dim) |> map_chr(paste, collapse = " x ")
  ) |> 
  mutate(
    category = case_when(
      str_detect(class, "data.frame|mts|nfnGroupedData|tbl_df") ~ "data.frame",
      class %in% c("character", "factor", "numeric") ~ "vector",
      TRUE ~ "other"
    ),
    #.keep = "unused",
    .after = name
  ) |> 
  left_join(all_data) |> 
  print()
