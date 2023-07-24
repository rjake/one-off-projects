# ____ WORKSPACE _______----
library(tidyverse)
library(extrafont)
library(lubridate)
library(palmerpenguins)
library(glue)

# import fonts - only once
#extrafont::font_import()

# load fonts - every session
extrafont::loadfonts(device = "win", quiet = TRUE)

seed <- NULL#2023 # use NULL = random

# * df_list  ----
df_list <-
  list(
    attitude = attitude,
    `forcats::gss_cat` = gss_cat,
    `ggplot2::economics` = economics,
    `ggplot2::mpg` = mpg,
    `ggplot2::diamonds` = diamonds,
    `lubridate::lakers` = lakers |> mutate(date = ymd(date)),
    `palmerpenguins::penguins` = palmerpenguins::penguins,
    `tidyr::world_bank_pop` = world_bank_pop
  )

# ____ FUNCTIONS _________----
pick_n <- function(x, n = 2, replace = FALSE) {
  if (!is.atomic(x) & !is.list(x)) {
    glue("criteria not met, should be a vector or list not '{class(x)}'") |>
      stop(call. = FALSE)
  }

  # vector: x <- head(label_caption)
  # list: x <- c(theme_list, theme_options)
  res <- x[!is.na(x)]

  if (length(res) < n) {
    return(0L)
  }

  set.seed(seed)

  ids <-
    sample(
      x = seq_along(res),
      size = n,
      replace = replace
    )

  res[ids]
}


make_ref_specs <- function() {
  if (runif(1, min = 0, max = 4) < 1) { # 1 in 5 times (0:4)
    return({
      list(
        ref_hline = NA, # x
        ref_vline = NA, # y
        ref_linetype = NA,
        ref_color = NA
      )
    })
  }

  set.seed(seed)

  fn <- sample(c(NA_character_, "mean", "median"), 2, replace = TRUE)

  list(
    ref_hline = fn[1], # x
    ref_vline = fn[2], # y
    ref_linetype = c("dotted", "dashed", "solid"),
    ref_color =
      c(grey.colors(5, start = 0, end = 0.8), c("red", "blue", "green")) |>
      glue(x = _, '"{x}"')
  ) |>
    map(~sample(.x, 1, FALSE))
}


make_cc_specs <- function(vars_c, vars_d) {
  vars_c_ids <- pick_n(vars_c, n = 2)

  list(
    geom = c("geom_point", "geom_count"),
    type_x = "numeric",
    type_y = "numeric",
    aes_x = vars_c_ids[1],
    aes_y = vars_c_ids[2],
    aes_color = c(vars_c, vars_d),
    aes_size = c(vars_c),
    const_color = c(rep(NA_character_, 3), "my_color"),
    const_size = c(rep(NA_integer_, 3), c(2, 4)) |> as.character()
  ) |>
    map(~sample(.x, 1, FALSE)) |>
    append(make_ref_specs()) |>
    as_tibble()
}


make_dd_specs <- function(vars_d, vars_c) {
  var_ids <- pick_n(vars_d, n = 2)

  list(
    geom = c("geom_count", "geom_point", "geom_tile"),
    type_x = "discrete",
    type_y = "discrete",
    aes_x = var_ids[1],
    aes_y = var_ids[2],
    aes_color = c(vars_d, vars_c),
    aes_size = vars_c,
    const_color = c(rep(NA, 3), "my_color"),
    const_size = c(rep(NA, 3), 1:6) |> as.character()
  ) |>
    map(~sample(.x, 1, FALSE)) |>
    append(make_ref_specs()) |>
    as_tibble()
}


make_dc_specs <- function(vars_d, vars_c) {
  d_direction <- ifelse(runif(1) < 0.33, "x", "y")
  var_d_id <- pick_n(vars_d, n = 1)
  var_c_id <- pick_n(vars_c, n = 1)
  var_ids <- c(var_d_id, var_c_id)

  if (d_direction == "y") {
    var_ids <- rev(var_ids)
  }

  prep_df <-
    list(
      geom = c("geom_bar", "geom_violin", "geom_boxplot"),
      type_x = "discrete",
      type_y = "numeric",
      aes_x = var_ids[1],
      aes_y = var_ids[2],
      aes_fill = vars_d,
      aes_size = vars_c,
      const_fill = c(rep(NA_character_, 3), "my_color"),
      const_size = c(rep(NA_integer_, 3), 1:6) |> as.character()
    ) |>
    map(~sample(.x, 1, FALSE)) |>
    append(make_ref_specs()) |>
    as_tibble()

  if (prep_df$geom[1] == "geom_bar") {
    geom_bar_na <- ifelse(d_direction == "y", "aes_x", "aes_y")
    prep_df[[geom_bar_na]] <- NA_character_
  }

  prep_df
}


make_tc_specs <- function(vars_t, vars_c, vars_d) {
  var_t_id <- pick_n(vars_t, n = 1)
  var_c_id <- pick_n(vars_c, n = 1)
  var_d_id <- c(NA_character_, pick_n(vars_d, n = 1))

  prep_df <-
    list(
      type = "date vs numeric",
      geom = c("geom_bar", "geom_line", "geom_point", "geom_histogram", "geom_area"),
      type_x = "date",
      type_y = "numeric",
      aes_x = var_t_id,
      aes_y = var_c_id,
      aes_fill = vars_d,
      const_color = c(rep(NA_character_, 3), "my_color")
    ) |>
    map(~sample(.x, 1, FALSE)) |>
    append(make_ref_specs()) |>
    as_tibble()
}


define_inputs <- function(df_name = "lubridate::lakers", n = 10) {

  df <- df_list[[df_name]]

  # set up data variables ----
  vars_d <- # discrete
    df |>
    select(
      c(
        where(is.character),
        where(is.factor),
        where(~n_distinct(.x) < 20)
      )
    ) |>
    names()

  vars_c <- # continuous
    df |>
    select(c(where(is.numeric))) |>
    names()

  vars_t <-  # time or date
    df |>
    select(c(where(is.Date), where(is.POSIXct))) |>
    names()


  # build lists ----
  res <- tibble()

  # _ cc ----
  if (length(vars_c) >= 2) {
    get_specs <- map_dfr(1:n, ~make_cc_specs(vars_c, vars_d))
    res <- bind_rows(res, get_specs)
  }

  # _ dd ----
  if (length(vars_d) >= 2) {
    get_specs <- map_dfr(1:n, ~make_dd_specs(vars_c, vars_d))
    res <- bind_rows(res, get_specs)
 }

  # _ dc ----
  if (length(vars_d) >= 1 & length(vars_c) >= 1) {
    get_specs <- map_df(1:n, ~make_dc_specs(vars_d, vars_c))
    res <- bind_rows(res, get_specs)
  }

  # _ dt ----
  if (length(vars_t >= 1) & length(vars_c) >= 1 & length(vars_d) >= 1) {
    get_specs <- map_df(1:n, ~make_tc_specs(vars_t, vars_c, vars_d))
    res <- bind_rows(res, get_specs)
  }

  res |>
    mutate(
      .before = everything(),
      df = df_name
    )
}


plot_opts <- local({

  phrases <- sentences |> str_remove("\\.$") |> glue(x = _, '"{x}"')

  list(
    font_name =
      c(
        "Arial", "Arial Black", "Arial Rounded MT Bold",
        "Candara", "Comic Sans MS", "Courier New", "Georgia",
        "Ink Free", "MV", "Lucida Console Regular", "Times New "
      ),
    theme_name =
      c(
        NA,
        "theme_classic",
        "theme_light",
        "theme_linedraw",
        "theme_minimal",
        "theme_void"
      ),
    theme_opt =
      c(
        NA,
        'legend.position = "none"',
        'plot.title.position = "plot"',
        "panel.grid = element_blank()",
        "axis.text.x = element_text(angle = 90)"
      ),
    title_fn = c(
      "str_to_upper",
      "str_to_sentence",
      "str_to_title"
    ),
    label_title = phrases,
    label_subtitle = c(NA, phrases[1:50]),
    label_caption = c(NA, rev(phrases[1:50]))
  )
})


get_plot_features <- function() {
  plot_opts |>
    map_dfr(
      ~pluck(
        .x[seq_along(.x) |> sample(1)]
      )
    )
}


assemble_inputs <- function(df_name = "lubridate::lakers", n_each = 1) {
  df <- df_list[[df_name]] |> as_tibble()
  # df_name <-  "lubridate::lakers"
  get_all_inputs <- define_inputs(df_name, n = n_each)

  get_all_plot_inputs <-
    seq_along(get_all_inputs$df) |>
    map_dfr(~get_plot_features())

  bind_cols(get_all_inputs, get_all_plot_inputs)
}

# ____ BUILD ______________----
# * use_data ----
use_data <-
  names(df_list) |>
  map_dfr(~assemble_inputs(df_name = .x, n_each = 5)) |>
  print(n = 20)

# * massage_data ----
massage_data <-
  use_data |>
  mutate(
    aes_color = ifelse(!is.na(const_color), NA_character_, aes_color),
    aes_fill =  ifelse(!is.na(const_fill),  NA_character_, aes_fill),
    aes_size =  ifelse(!is.na(const_size),  NA_character_, aes_size),
    ref_hline =  ifelse(!is.na(aes_y),  NA_character_, aes_y),
    ref_vline =  ifelse(!is.na(aes_x),  NA_character_, aes_x)
  ) |>
  # what else do we need to do?
  print()


# from_list_to_string(record = massage_data[2,], "aes_")
from_list_to_string <- function(record = massage_data[2,], col_prefix = "aes_", sep = ", ", first_phrase_start = "") {
  prefix_regex <- glue("^{col_prefix}")

  x <-
    record |>
    select(matches(prefix_regex)) |>
    as.list() |>
    discard(is.na)

  if (!length(x)) {
    return("")
  }

  param_df <-
    tibble(
      param = names(x) |> str_remove_all(prefix_regex),
      value = flatten_chr(x)
    ) |>
    mutate(
      param = ifelse(row_number() == 1, glue("{first_phrase_start}{param}"), param)
    )

  glue_data(param_df, "{param} = {value}") |>
    glue_collapse(sep)
}


build_ggplot_code <- function(record_id = 70) {
  record <-
    massage_data[record_id, ] |>
    as_tibble() %>% # need magrittr here
    select(sort(names(.), decreasing = TRUE)) |>
    select(where(~sum(is.na(.x)) == 0))

  # gg_string start ----
  aes_args <- from_list_to_string(record, col_prefix = "aes_")
  const_args <- from_list_to_string(record, col_prefix = "const_", first_phrase_start = ", ")

  gg_string <-
    c(
      glue("{record$df} |> \n ggplot(aes({aes_args}){const_args})"),
      glue("{record$geom}()")
    )


  # vline & hline ----
  if (any(str_detect(names(record), "^ref_"))) {
    vhline_df <-
      record |>
      select(starts_with("ref_"))


    if ("ref_hline" %in% names(vhline_df)) {
      hline_string <-
        vhline_df |>
        glue_data(
          "{g}({int}{args})",
          g = "geom_hline",
          int = glue("yintercept = {vhline_df$ref_hline}({record$aes_y})"),
          args = from_list_to_string(vhline_df |> select(-ends_with("line")), col_prefix = "ref_", first_phrase_start = ", ")
        )

      gg_string <- c(gg_string, hline_string)
    }

    if ("ref_vline" %in% names(vhline_df)) {
      vline_string <-
        vhline_df |>
        glue_data(
          "{g}({int}{args})",
          g = "geom_hline",
          int = glue("xintercept = {vhline_df$ref_vline}({record$aes_x})"),
          args =
            from_list_to_string(
              vhline_df |> select(-ends_with("line")),
              col_prefix = "ref_",
              first_phrase_start = ", "
            )
        )

      gg_string <- c(gg_string, vline_string)
    }
  }

  # theme ----
  if ("theme_name" %in% names(record)) {
    gg_string <- c(gg_string, glue("{record$theme_name}()"))
  }

  theme_string <-
    glue(
      "theme(\n{font}{opts}\n)",
      font = glue('text = element_text(family = "{record$font_name}")'),
      opts =
        ifelse(
          "theme_opt" %in% names(record),
          ",\n{record$theme_opt}",
          ""
        ) |>
        glue()
    )

  gg_string <- c(gg_string, theme_string)


  # labs ----
  labs_fn <- parse(text = record$title_fn) |> eval()

  labs_args <-
    record |>
    select(starts_with("label_")) |>
    mutate_all(labs_fn) |>
    from_list_to_string("label_", sep = ",\n")

  if (labs_args != "") {
    gg_string <- c(gg_string, glue("labs(\n{labs_args}\n)"))
  }

  gg_string |>
    glue_collapse(" + \n  ")
}


build_ggplot_code(1)
build_ggplot_code(25)
build_ggplot_code(50)
build_ggplot_code(70)



create_assets <- function(record_id = 1) {
  code_to_use <- build_ggplot_code(record_id)

  p <-
    parse(text = code_to_use) |>
    eval()

  #p$layers$mapping

  # check certain df dynamics
  p_df <- ggplot_build(p)

  print(p)

  list(
    code_run = code_to_use,
    code_user = styler::style_text(code_to_use),
    plot = p,
    data = massage_data[record_id, ] |> as_tibble()
  )
}


create_assets(1)
create_assets(25)
create_assets(50)
create_assets(70)
create_assets(80)

  # if geom_count and too many dots, change to geom_point
  # make b/w first then assess color?
}


styler::style_text(final_string)


attitude |>
  ggplot(aes(color = raises, x = critical, y = rating), size = 2) +
  geom_point() +
  geom_hline(yintercept = mean(rating), color = "#000000", linetype = dotted) +
               geom_hline(xintercept = mean(critical), color = #000000, linetype = dotted) +
                            labs(
                              title = "It was hidden from sight by a mass of leaves and shrubs",
                              subtitle = "Hop over the fence and plunge in",
                              caption = "The juice of lemons makes fine punch"
                            )




  record |>
    glue_data(
      "df |>
        ggplot2(aes({x}, {y}))"
    )

}





my_color <- luv_colours$col




