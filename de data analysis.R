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
news_count.de <- news.german.words %>% count(word, sort = TRUE)

# plot highest frequency words
plot_count.de <- news_count.de %>%
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
tweet.hashtag_count.de <- tweet.all_german.words %>% 
  filter(word == str_extract_all(word, "#\\S+")) %>% 
  count(word, sort = TRUE)

# plot most commonly used hashtags
plot_tweet.hashtag_count.de <- tweet.hashtag_count.de %>%
  filter(n > quantile(n, 0.95)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

# finding most commonly used mentions
tweet.mention_count.de <- tweet.all_german.words %>% 
  filter(word == str_extract_all(word, "@\\S+")) %>% 
  count(word, sort = TRUE)

# plot most commonly used mentions
plot_tweet.mention_count.de <- tweet.mention_count.de %>%
  filter(n > quantile(n, 0.9)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

# clean twitter data, but removing all links, hashtags, and mentions
tweet.german.words <- tweet.all_german.words %>%
  mutate(word = str_replace_all(word, "#\\S+", "")) %>%
  mutate(word = str_replace_all(word, "@\\S+", "")) %>%
  mutate(word = gsub('[[:punct:]]+', '', word)) %>%
  filter(word != '')

# finding most commonly used words outside of hashtags and mentions
tweet_count.de <- tweet.german.words %>% count(word, sort = TRUE)

# plot most commonly used words outside of hashtags and mentions
plot_tweet_count.de <- tweet_count.de %>%
  filter(n > quantile(n, 0.99)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()


######################################################
### Comparing News Articles and Twitter Word Usage ###
######################################################

# find frequency for each word in the news articles and from twitter
frequency.de <- bind_rows(mutate(news.german.words, type = "News Article"),
                          mutate(tweet.german.words, type = "Twitter")) %>%
  count(type, word) %>%
  group_by(type) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(type, proportion) %>%
  gather(type, proportion, `News Article`)

library(scales)

# plot frequencies on same plot, words closer to the line have similar frequencies in both types of text
ggplot(frequency.de, aes(x = proportion, y = `Twitter`, color = abs(`Twitter` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~type, ncol = 1) +
  theme(legend.position="none") +
  labs(y = "Twitter", x = NULL)

# correlation test
correlation <- cor.test(data = frequency.de[frequency.de$type == "News Article",], ~ proportion + `Twitter`)
#knitr::kable(correlation)

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