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
  mutate(
    item = str_extract(item, "^[\\w\\.]+"),
    pkg_data = glue("{package}::{item}")
  ) |> 
  distinct() |> 
  filter(
    !str_detect(package, "^(headliner|maptools|plyr|simplecolors|sp)$"),
    !str_detect(item, "^(coralPval|HHSCyberSecurityBreaches|Khan|NCI60|ipo|treering)$")
  ) |> 
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
    # if (pkg %in% c("AER", "Stat2Data")) {
    #   data(list = item)
    # } else {
      data(list = item, package = pkg)
    #}
    df <- get(item)  #rlang::parse_expr(x) |> eval()
    rm(list = item, envir = globalenv())
    df_class <- class(df)
    is_df <- inherits(df, "data.frame") & !inherits(df, "sf")
    
    
    dim <- dim(df) |> paste(collapse = " x ")
    is_trunc <- TRUE
    
    # if (is.list(df) & !is_df) {
    #   use_data <- map(df, head, max_rows / 2)
    #   
    # } else if (is.vector(df) & !is.list(df)) {
    #   use_data <- head(df, max_rows)
    #   dim <- length(df)
    #   
    # } else 
    if (!is_df) {
      return(NULL)
      # use_data <- df
      # is_trunc <- FALSE
      
    } else if (nrow(df) < max_rows & ncol(df) <= max_cols) {
      use_data <- df
      is_trunc <- FALSE
      
    } else {
      use_data <- 
        head(df, max_rows) |> 
        mutate(across(where(is.character), as.factor)) |> 
        mutate(across(where(is.factor), fct_drop)) |> 
        select(1:min(ncol(df), max_cols)) # max of n columns
      
      attr(use_data, "row.names") <- row.names(use_data) |> head(max_rows)
      attr(use_data, "na.action") <- NULL # can cause to be large size ex 
      attr(use_data, "spec") <- NULL # comes in with some tibbles, ex openintro::hfi
    }
    
    class_test <- function(df, class) {
      sum(
        sapply(
          df,
          function(col) {
            class(col) |> discard(str_detect, "labelled") |> pluck(1) %in% class
          }
        )
      )
    }
    
    list(
      pkg_data = x,
      data = use_data,
      is_trunc = is_trunc,
      dim = as.character(dim),
      n_col = ncol(df),
      n_row = nrow(df),
      class = df_class[1],
      n_discrete = class_test(df, c("character", "factor", "ordered")),
      n_numeric = class_test(df, c("numeric", "integer", "labelled")),
      n_date = class_test(df, c("Date", "POSIXct", "dttm"))
    )
}


get_data_info(x = "Stat2Data::BeeStings")
get_data_info(x = "COUNT::fishing")
get_data_info(x = "ks::platesf")
get_data_info(x = "ggplot2::diamonds")
get_data_info(x = "openintro::hfi")
get_data_info(x = "stringr::sentences")
get_data_info(x = "openintro::children_gender_stereo")
get_data_info("datasets::Seatbelts")

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
  map_dfr(flatten)



prep_details <-
  list_details |> 
  mutate(
    category = case_when(
      str_detect(class, "data.frame|nfnGroupedData|tbl_df") ~ "data.frame",
      class %in% c("character", "factor", "numeric") ~ "vector",
      str_detect(class, "ts$") ~ "ts",
      TRUE ~ "other"
    ),
    is_df = category == "data.frame",
    .after = pkg_data
  ) |> 
  mutate(
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
    (n_row >= 30 & n_col >= 5)
    | n_discrete > 0
  ) |> 
  arrange(name) |> 
  print()


data_details |> 
  count(pkg)

data_details |> 
#  filter(category == "other") |> 
  count(category, class)

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
