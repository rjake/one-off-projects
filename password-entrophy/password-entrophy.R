# It looks like  a complex password of length 10 has roughly the same entrophy (log2(94^10) = 65) 
# as a letter-only password of length 11 (log2(52^12) = 63) 
# or a lowercase-only password of length 14 (log2(26^14) = 66). 
# Passphrases are made of words so the true entrophy would be lower and a length of 16 probably makes up the difference

library(tidyverse)

n_lower <- 26
n_alpha <- 52
n_number <- 10
n_symbol <- 32
n_all <- (n_alpha + n_number + n_symbol)

res <- 
  tibble(
    type = c("lower only", "alpha only", "anything"),
    pool = c(n_lower, n_alpha, n_all),
    n = list(1:20)
  ) |> 
  unnest(n) |> 
  mutate(e = log2(pool^n)) |>  # entrphy
  print()

colors <- c(
  "anything" = "dodgerblue",
  "alpha only" = "grey75",
  "lower only" = "black"
)

res |> 
  ggplot(aes(n, e, fill = type, color = type)) +
  geom_line(
    #data = ~filter(.x, pool != n_alpha),
    aes(group = type),
    alpha = 0.5
  ) +
  geom_linerange(
    data = ~filter(.x, pool == n_lower),
    aes(xmin = n, xmax = n * 0.7),
    alpha = 0.3
  ) +
  geom_label(
    #data = ~filter(.x, pool != n_alpha),
    aes(label = n), 
    size = 3,
    fill = "white",
    label.size = 0,
    label.padding = unit(0.15, "lines")
  ) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  coord_cartesian(xlim = c(0, 20)) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  labs(
    title = "Password entrophy by type of restriction",
    caption = paste(
      "entrophy - https://www.omnicalculator.com/other/password-entropy",
      sep = "\n"
    ),
    x = 'Password Length',
    y = "Entrophy as log-2",
    color = NULL
  )
