# find all locally ###########################################################
# inspired by https://vincentarelbundock.github.io/Rdatasets/datasets.html
setwd(dirname(.rs.api.getSourceEditorContext()$path))
library(tidyverse)
library(glue)

base_data <-
  data(package = .packages(all.available = TRUE))$results |>
  as_tibble() |>
  rename_all(tolower) |>
  select(-libpath) |> 
  filter(
    str_detect(
      package, 
      paste(
        "dplyr|forcats|ggplot2|lubridate|stringr|tidyr",
        "carData|datasets|gamclass|^gt$|ISLR|MASS|mclust|nlme|nycflights13|openintro|rpart|survival|wooldridge",
        sep = "|"
      )
    ),
    !str_detect(item, "^(coralPval|Khan|NCI60|ipo|treering)$")
  ) |> 
  mutate(
    origin = case_when(
      package == "datasets" ~ "base",
      package %in% tidyverse_packages() ~ "tidyverse",
      TRUE ~ "other"
    ),
    pkg_data = glue("{package}::{item}")
  ) |> 
  print()


max_rows <- 100
max_cols <- 30

get_data_info <- function(x) {
    df <- rlang::parse_expr(x) |> eval()
    df_class <- class(df)
    is_df <- inherits(df, "data.frame")
    
    if (is.vector(df)) {
      use_data <- head(use_data, max_rows)
    } else if (!is_df) {
      use_data <- df
    } else if (nrow(df) < max_rows & ncol(df) <= max_cols) {
      use_data <- df
    } else {
      use_data <- 
        head(df, max_rows) |> 
        mutate(across(where(is.factor), fct_drop)) |> 
        select(1:min(ncol(df), max_cols)) # max of n columns
      
      attr(use_data, "row.names") <- row.names(use_data) |> head(max_rows)
      attr(use_data, "na.action") <- NULL # can cause to be large size ex 
      attr(use_data, "spec") <- NULL # comes in with some tibbles, ex openintro::hfi
    }
    
    list(
      pkg_data = x,
      data = use_data,
      is_trunc = !identical(df, head(df, max_rows)),
      dim = dim(df) |> paste(collapse = " x "),
      n_col = ncol(df),
      n_row = nrow(df),
      class = df_class[1]
    )
}

a <- get_data_info(x = "ggplot2::diamonds")
b <- get_data_info(x = "openintro::hfi")


ds_obj <-
  map(
    .x = base_data$pkg_data, #[201:205]
    possibly(get_data_info, otherwise = NULL)
  ) |>
  set_names(base_data$pkg_data) |> 
  discard(is.null)

# check sizes
map(ds_obj, pluck, "data") |> 
  map_df(object.size) |> 
  gather() |> 
  arrange(desc(value))

list_details <- 
  ds_obj |> 
  map( ~tidyselect:::select(.x, -data)) |> 
  map_dfr(pluck)


class_test <- function(class) {
  map_int(
    .x = map(ds_obj, pluck, "data"), # hard coded
    .f = ~sum(sapply(.x, \(col) class(col)[1] %in% class))
  )
}

prep_details <-
  list_details |> 
  mutate(
    category = case_when(
      str_detect(class, "data.frame|mts|nfnGroupedData|tbl_df") ~ "data.frame",
      class %in% c("character", "factor", "numeric") ~ "vector",
      TRUE ~ "other"
    ),
    is_df = category == "data.frame",
    .after = pkg_data
  ) |> 
  mutate(
    n_discrete = class_test(c("character", "factor", "ordered")),
    n_numeric = class_test(c("numeric", "integer")),
    n_numeric = ifelse(str_detect(class, "ts"), 0, n_numeric),
    n_date = class_test(c("Date", "POSIXct")),
    n_other = n_col - n_discrete - n_numeric - n_date
  ) |>
  print()


data_details <- 
  base_data |> 
  inner_join(prep_details) |> #count(package, sort = TRUE) |> 
  rename(
    pkg = package,
    name = item
  ) |> 
  filter(
    (is_df & n_row >= 30 & n_col >= 5)
    | !is_df
  ) |> 
  arrange(pkg_data) |> 
  print()


data_details |> 
  count(pkg) |> 
  .print()

write_csv(data_details, "data_details.csv")

ds_list <- 
  ds_obj[data_details$pkg_data] |> 
  map(~tidyselect:::select(.x, data))
  
saveRDS(ds_list, "ds_list.Rds")
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
