library(tidyverse)
library(lubridate)
library(googlesheets4)
library(RecordLinkage) #jarowinkler # install.packages("RecordLinkage")

#library(extrafont)

setwd("~/../Desktop/typing")

raw_data <-
  read_sheet("https://docs.google.com/spreadsheets/d/1aAyxUTdjf4PK6De_qmMfR17GVhPs_V2Kv2lzwCbk1Yw/edit#gid=1150264314") %>% 
  mutate(
    day = str_pad(row_number() - 1, 2, "left", "0") %>% 
           paste0("day", .)#,day = date
  )

#raw_data$text[1] %>% str_wrap(90) %>% cat()
# https://thepracticetest.com/typing/tests/practice-paragraphs/index.php?t=11


prep_df <-
  raw_data %>%
  select(style,date, day, text) %>%
  filter(text != "-") %>%
  mutate(word = strsplit(text, " ")) %>%
  select(-text) %>%
  unnest(word) %>%
  group_by(date) %>%
  mutate(ord = row_number()) %>%
  ungroup()

orig_text <-
  prep_df %>%
  filter(style == "correct") %>%
  select(-c(date, day, style)) %>%
  rename(orig = word)

attempts <-
  filter(prep_df, !is.na(date))

df_compare <-
  attempts %>%
  left_join(orig_text) %>%
  mutate(
    dist = RecordLinkage::jarowinkler(orig, word, r = 2),
    correct = dist == 1
  )

# mean(df_compare$dist)
# sum(df_compare$dist)
# mean(df_compare$correct)
# sum(df_compare$correct)

plot_orig_text <-
  orig_text %>%
  mutate(
    orig = paste0(orig, " "),
    letter = strsplit(orig, "")
  ) %>%
  unnest(letter) %>%
  select(-c(orig)) %>%
  mutate(
    style = "",
    date = "text",
    letter_ord = row_number(),
    dist = 1,
    correct = TRUE
  ) %>%
  select(style, date, ord, letter, letter_ord, dist, correct)


plot_dates <-
  df_compare %>%
  mutate(
    word = paste0(word, " "),
    letter = strsplit(word, ""),
    day = format(date, format = "%m/%d/%y"),
    day = ifelse(style == "baseline", style, day)
  ) %>%
  unnest(letter) %>%
  select(-c(word)) %>%
  group_by(date) %>%
  mutate(letter_ord = row_number()) %>%
  ungroup() %>%
  select(style, date = day, ord, letter, letter_ord, dist, correct)

wpm <- function(x) x %/% 6

bind_rows(plot_dates, plot_orig_text) %>%
  #filter(letter_ord <= max(plot_dates$letter_ord)) %>%
  ggplot(aes(letter_ord, fct_rev(date))) +
  facet_grid(rows = vars(style), space = "free", scales = "free") +
  geom_text(
    aes(label = letter, color = correct), 
    size = 4, family = "Courier New" #"Lucida Sans Typewriter"#
  ) +
  scale_color_manual(values = c("red", "black")) +
  scale_x_continuous(
    expand = expand_scale(0.001),
    limit = c(0, 280),
    breaks = 1:10*60,
    labels = wpm
  ) +
  theme(
    panel.background = element_rect(color = "grey80", fill = "white"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    text = element_text(size = 11,  family = "Calibri")
  ) +
  labs(
    y = "",
    x = "words per minute"
  )

ggsave(
  "funaday_2020.png",
  height = 10,
  width = 25,
  units = "in",
  dpi = 300
)



# There are many idiosyncratic typing styles in between novice-style "hunt and peck" and touch typing. 

# For example, many "hunt and peck" typists have the keyboard layout memorized and are able to type while focusing their gaze on the screen. 

# Some use just two fingers, while others use 3-6 fingers. 

# Some use their fingers very consistently, with the same finger being used to type the same character every time, while others vary the way they use their fingers.
# Sys.sleep(61); beepr::beep(5);
