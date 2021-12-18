setwd("presidential-debates-2008-2016")
library(tidyverse)

repo <-
  "https://raw.githubusercontent.com/rpatelCERN/PresidentialDebates/main/RawCSV/"

files <-
  c(
    "JoeBidenVDonaldTrump1",
    "JoeBidenVDonaldTrump2",

    "The_First_Clinton-Trump_Presidential_Debate",
    "The_Second_Clinton-Trump_Presidential_Debate",
    "The_Third_Clinton-Trump_Presidential_Debate",

    "The_First_Obama-Romney_Presidential_Debate",
    "The_Second_Obama-Romney_Presidential_Debate",
    "The_Third_Obama-Romney_Presidential_Debate",

    "The_First_McCain-Obama_Presidential_Debate",
    "The_Second_McCain-Obama_Presidential_Debate",
    "The_Third_McCain-Obama_Presidential_Debate"
  )



all_files <-
  map(
    .x = paste0(repo, files, ".csv"),
    .f = read_csv
  )


final_table <-
  tibble(
    file = files,
    x = all_files
  )  %>%
  unnest(x) %>%
  rename_all(tolower) %>%
  rename(line = x1) %>%
  mutate_all(tolower) %>%
  mutate(
    response = str_remove_all(response, "\\d+:\\d+"), # timestamps
    line = as.integer(line) + 1,
    party = case_when(
      str_detect(speaker, "obama|biden|clinton") ~ "democrat",
      str_detect(speaker, "trump|mccain|romney") ~ "republican",
      TRUE ~ "moderator"
    ),
    order = case_when(
      str_detect(file, "first|1") ~ 1,
      str_detect(file, "second|2") ~ 2,
      str_detect(file, "third|3") ~ 3
    ),
    debators = map_chr(
      .x = file,
      .f = ~paste(
        str_extract(., "biden|obama|clinton"),
        str_extract(., "trump|mccain|romney"),
        sep = "-"
      )
    )
  ) %>%
  select(-file) %>%
  select(debators, order, party, speaker, line, response) %>%
  print()


write_csv(final_table, "all_text.csv")
