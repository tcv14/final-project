library(tidyverse)
library(tidytext)
#install.packages("lsa")
library(lsa)
#install.packages("quanteda")
library(quanteda)

# load stop words
data(stop_words)
stopwords_lsa <- as_data_frame(stopwords_de)
colnames(stopwords_lsa) <- "word"
stopwords_quanteda <- as_data_frame(stopwords("german"))
colnames(stopwords_quanteda) <- "word"
own_stopwords.de <- tibble(
  `word` = c("facebook", "mark", "zuckerberg", "zuckerbergs", "cambridge", "analytica", "us",
              "nun", "dabei", "dafür", "darauf", "hätten", "deren", "ja", "eigentlich",
              "kommt", "mal", "heute", "gerade", "schon", "warum", "sagt", "beim", "gibt",
              "gestern", "marc"))

#########################
### For News Articles ###
#########################

# load news article data
load("./News Article Data/news.german.RData")

# clean news article words
news.german.words <- news.german %>%
  unnest_tokens(word, value, token = "regex", pattern = "’") %>%
  unnest_tokens(word, word, token = "regex", pattern = "'") %>%
  unnest_tokens(word, word) %>%
  mutate(word = gsub('[[:digit:]]+', '', word)) %>%
  mutate(word = gsub('[[:punct:] ]+', '', word)) %>%
  filter(word != '') %>%
  anti_join(stopwords_lsa) %>%
  anti_join(stopwords_quanteda) %>%
  anti_join(own_stopwords.de) %>%
  anti_join(stop_words) %>%
  mutate(word = replace(word, word == "nutzern", "nutzer")) %>%
  mutate(word = replace(word, word == "fragen", "frage")) %>%
  mutate(word = replace(word, word == "informationen", "information")) %>%
  mutate(word = replace(word, word == "firmen", "firma"))

# find highest frequency words
news_frequency.de <- news.german.words %>% count(word, sort = TRUE)

# plot highest frequency words
plot_frequency.de <- news_frequency.de %>%
  filter(n > quantile(n, 0.99)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

###################
### For Twitter ###
###################

# load twitter data
load("./Twitter Data/tweet.german.RData")

# clean twitter data- removing links, but retaining hashtags and mentions
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-zäüöß_\\d#@']|'(?![A-Za-zäüöß_\\d#@]))"
tweet.all_german.words <- tweet.german %>% 
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = "’") %>%
  unnest_tokens(word, word, token = "regex", pattern = "'") %>%
  unnest_tokens(word, word, token = "regex", pattern = unnest_reg) %>%
  mutate(word = gsub('[[:digit:]]+', '', word)) %>%
  filter(word != '') %>%
  anti_join(stopwords_lsa) %>%
  anti_join(stopwords_quanteda) %>%
  anti_join(own_stopwords.de) %>%
  anti_join(stop_words)

# finding most commonly used hashtags
tweet.hashtag_frequency.de <- tweet.all_german.words %>% 
  filter(word == str_extract_all(word, "#\\S+")) %>% 
  count(word, sort = TRUE)

# plot most commonly used hashtags
plot_tweet.hashtag_frequency.de <- tweet.hashtag_frequency.de %>%
  filter(n > quantile(n, 0.95)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

# finding most commonly used mentions
tweet.mention_frequency.de <- tweet.all_german.words %>% 
  filter(word == str_extract_all(word, "@\\S+")) %>% 
  count(word, sort = TRUE)

# plot most commonly used mentions
plot_tweet.mention_frequency.de <- tweet.mention_frequency.de %>%
  filter(n > quantile(n, 0.9)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

# clean twitter data, but removing all links, hashtags, and mentions
tweet.german.words <- tweet.all_german.words %>%
  mutate(word = str_replace_all(word, "#\\S+", "")) %>%
  mutate(word = str_replace_all(word, "@\\S+", "")) %>%
  filter(word != '')

# finding most commonly used words outside of hashtags and mentions
tweet_frequency.de <- tweet.german.words %>% count(word, sort = TRUE)

# plot most commonly used words outside of hashtags and mentions
plot_tweet_frequency.de <- tweet_frequency.de %>%
  filter(n > quantile(n, 0.99)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()


##########################
### Sentiment Analysis ###
##########################

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