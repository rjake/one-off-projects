# steps come from
# https://adamspannbauer.github.io/2017/12/24/graph-nlp-in-r/
  
library(spacyr)
library(visNetwork)
library(data.table)
library(tm)
library(tidyverse)

spacyr::spacy_initialize(model = "en_core_web_sm")

text <- 
  "Humpty Dumpty sat on a wall. Humpty Dumpty had a great fall. All the king's horses and all the king's men couldn't put Humpty together again."
  #sentences[7]
  #"Small regions of subcortical white matter hypoattenuation in the superior frontal gyri regions are not changed and may be benign perivascular spaces."

parsed_text <-
  text |> 
  spacyr::spacy_parse(tag = TRUE, lemma = TRUE, nounphrase = TRUE, dependency = TRUE)
  
spacyr::spacy_finalize()

prep_data <-
  parsed_text |> 
  mutate(lemma = ifelse(lemma == "-PRON-", tolower(token), lemma)) |> 
  filter(pos != "PUNCT")

#process spacy parse output for plotting with visNetwork
plot_data <-
  with(
    prep_data,
    list(
      nodes = tibble(
        id = token_id,
        label = token,
        group = pos,
        sentence_id = sentence_id,
        size = 5
      ),
      edges = tibble(
        to = head_token_id,
        from = token_id,
        label = token,
        sentence_id = sentence_id
      )
    )
  )

nodes <- plot_data$nodes
edges <- plot_data$edges

# view as heirarchical layout with: visHierarchicalLayout()
visNetwork(
  filter(nodes, sentence_id == 1), 
  filter(edges, sentence_id == 1), 
  main = text
) |>
  visHierarchicalLayout() |>
  visNodes(shape = "ellipse") |>
  visInteraction(
    dragNodes = TRUE,
    dragView = TRUE,
    zoomView = TRUE
  ) |>
  visLegend(zoom = FALSE)

# view as igraph (needs diff data prep)
visNetwork(
  nodes = mutate(nodes, color = "lightblue"),
  edges = filter(edges, from != to)
) |> 
  visIgraphLayout()
