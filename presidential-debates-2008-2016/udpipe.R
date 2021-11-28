# https://bnosac.github.io/udpipe/docs/doc5.html
# https://universaldependencies.org/u/pos/AUX_.html

# install.packages("udpipe")
library(udpipe)
setwd("presidential-debates-2008-2016/")
speeches <- read_csv("all_text.csv")

# english <- udpipe_download_model("english") #english-lines english-partut
udmodel_english <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

txt <-
  speeches %>%
  filter(party != "moderator") %>%
  head(100) %>%
  pull(response)
# txt <- c(
#   "Leave now.",
#   "Please leave.",
#   "Let's leave.",
#   "We should leave.",
#   "I think we should leave.",
#   "I think it would be best that we leave."
# )

x <-
  udpipe_annotate(udmodel_english, x = txt) %>%
  as_tibble()

x

x %>%
  select(doc_id, sentence, token:feats, dep_rel) %>%
  filter(str_detect(upos, "VERB|AUX")) %>%
  separate_rows(feats, sep = "\\|") %>%
  # slice(1:3) %>%
  separate(feats, c("feature", "value"), sep = "\\=") # %>% spread(feature, value)

#%>%  .print()

stats <-
  keywords_rake(
    x = x,
    term = "lemma",
    group = "sentence_id",
    relevant = x$upos %in% c("NOUN", "ADJ")
  )

#stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
stats %>%
  filter(rake > 0, freq >= 4) %>%
  mutate(keyword = fct_reorder(keyword, rake)) %>%
  ggplot(aes(rake, keyword)) +
  geom_col(fill = "cadetblue") +
  labs(
    title = "Keywords identified by RAKE",
    x = "Rake"
  )


coocur <-
  cooccurrence(
    x = subset(x, upos %in% c("NOUN", "ADJ")),
    term = "lemma",
    group = c("doc_id", "paragraph_id", "sentence_id")
  )

head(coocur)
