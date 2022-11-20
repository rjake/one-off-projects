library(widyr)
library(tokenizers)
library(tidytext)
library(textstem)
library(furrr)
library(tidyverse)


complaints <-
  readr::read_csv("input/complaints.csv.gz")

prep_complaints <-
  complaints |>
  filter(product == "Student loan")

tidy_complaints <-
  prep_complaints |>
  select(complaint_id, consumer_complaint_narrative) |>
  unnest_tokens(word, consumer_complaint_narrative)

lemma_words <-
  tidy_complaints |>
  mutate(
    word = lemmatize_words(word)
  ) |>
  add_count(word)

nested_words <-
  lemma_words |>
  filter(n >= 50) |>
  filter(
    !str_detect(word, "^(a|and|at|as|be|but|can|do|for|from|i|in|it|me|my|of|on|that|the|they|this|to)$"),
    !str_detect(word, "xx+")
  ) |> #distinct(word, n) |> slice_max(order_by = n, n = 20) |> arrange(word)
  select(-n) |>
  nest(words = c(word))


slide_windows <- function(tbl, window_size) {
  skipgrams <- slider::slide(
    tbl,
    ~.x,
    .after = window_size - 1,
    .step = 1,
    .complete = TRUE
  )

  safe_mutate <- safely(mutate)

  out <- map2(
    skipgrams,
    1:length(skipgrams),
    ~ safe_mutate(.x, window_id = .y)
  )

  out |>
    transpose() |>
    pluck("result") |>
    compact() |>
    bind_rows()
}

plan(multisession)  ## for parallel processing

tidy_pmi <- # verrrry slow
  nested_words |>
  mutate(words = future_map(words, slide_windows, 4)) |>
  unnest(words) |>
  unite(window_id, complaint_id, window_id) |>
  pairwise_pmi(word, window_id)


.beep()

plan(sequential)


tidy_word_vectors <-
  tidy_pmi |>
  widely_svd( # identify dimensions
    item = item1,
    feature = item2,
    value = pmi,
    nv = 10,   # vectors to estimate
    maxit = 1000 # max iterations
  )

tidy_word_vectors



nearest_neighbors <- function(df, token) {
  df %>%
    widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) /
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))

        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE
    )(item1, dimension, value) %>%
    select(-item2)
}


tidy_word_vectors %>%
  nearest_neighbors("error")


tidy_word_vectors |>
  mutate(dimension = fct_reorder(factor(dimension), abs(value), sum)) |>
  #filter(as.integer(dimension) <= 6) |>
  group_by(dimension) |>
  top_n(12, abs(value)) |>
  ungroup() |>
  mutate(item1 = reorder_within(item1, value, dimension)) |>
  ggplot(aes(value, item1, fill = dimension)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~dimension, scales = "free_y", nrow = 2) +
  #scale_fill_viridis_b() +
  scale_y_reordered() +
  labs(
    x = NULL,
    y = "Value",
    title = "First 24 principal components for text of CFPB complaints",
    subtitle = paste("Top words contributing to the components that explain",
                     "the most variation")
  )




# original data ----
  # download.file(
  #   url =
  #     "https://github.com/EmilHvitfeldt/smltar/blob/master/data/complaints.csv.gz?raw=true",
  #   destfile = "input/complaints.csv.gz",
  #   mode = "wb"
  # )
