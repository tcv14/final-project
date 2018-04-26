library(tidyverse)
library(tidytext)
#install.packages("lsa")
library(lsa)

load("./News Article Data/news.german.RData")

stopwords_lsa <- as_data_frame(stopwords_de)
own_stopwords.de <- tibble(
  `value` = c("facebook", "mark", "zuckerberg", "zuckerbergs", "cambridge", "analytica", "er", "us",
              "will", "man", "damit", "ob", "nun", "dabei", "dafür", "darauf", "hätten",
              "deren", "doch", "dies", "also"))

news.german.words <- news.german %>% unnest_tokens(value, value) %>%
  anti_join(stopwords_lsa) %>%
  anti_join(own_stopwords.de) %>%
  mutate(value = replace(value, value == "nutzern", "nutzer")) %>%
  mutate(value = replace(value, value == "fragen", "frage")) %>%
  mutate(value = replace(value, value == "informationen", "information")) %>%
  mutate(value = replace(value, value == "firmen", "firma"))

häufigkeit <- news.german.words %>% count(value, sort = TRUE)

# Wörter laden und vorbereiten
sent <- c(
  # positive Wörter
  readLines(paste0("SentiWS_v1.8c_Positive.txt"), encoding = "UTF-8"),
  # negative Wörter
  readLines(paste0("SentiWS_v1.8c_Negative.txt"), encoding = "UTF-8")
) %>% 
  lapply(function(x) {
    # Extrahieren der einzelnen Spalten
    res <- strsplit(x, "\t", fixed = TRUE)[[1]]
    return(data.frame(words = res[1], value = res[2], stringsAsFactors = FALSE))
  }) %>%
  bind_rows %>% 
  mutate(words = gsub("\\|.*", "", words) %>% tolower, value = as.numeric(value)) %>% 
  # manche Wörter kommen doppelt vor, hier nehmen wir den mittleren Wert
  group_by(words) %>% summarise(value = mean(value)) %>% ungroup