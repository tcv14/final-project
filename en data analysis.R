library(tidyverse)
library(tidytext)

load("./News Article Data/news.english.RData")

data(stop_words)

own_stopwords.en <- tibble(
  `word` = c("facebook", "mark", "zuckerberg", "cambridge", "analytica"))

news.english.words <- news.english %>% unnest_tokens(word, value, token = "regex", pattern = "’") %>%
  unnest_tokens(word, word, token = "regex", pattern = "'") %>%
  unnest_tokens(word, word) %>%
  anti_join(stop_words) %>%
  anti_join(own_stopwords.en) %>%
  mutate(word = replace(word, word == "users", "user")) %>%
  mutate(word = replace(word, word == "companies", "company"))

frequency <- news.english.words %>% count(word, sort = TRUE)
