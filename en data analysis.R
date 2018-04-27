library(tidyverse)
library(tidytext)
library(stringr)

# load stop words
data(stop_words)

# create own list of stop words based on subject content
own_stopwords.en <- tibble(
  `word` = c("facebook", "mark", "zuckerberg", "cambridge", "analytica", "fb", "didn", "don", "doesn", "ve"))

#########################
### For News Articles ###
#########################

# load news article data
load("./News Article Data/news.english.RData")

# clean news article words
news.english.words <- news.english %>% 
  unnest_tokens(word, value, token = "regex", pattern = "’") %>%
  unnest_tokens(word, word, token = "regex", pattern = "'") %>%
  unnest_tokens(word, word) %>%
  mutate(word = gsub('[[:digit:]]+', '', word)) %>%
  mutate(word = gsub('[[:punct:] ]+', '', word)) %>%
  filter(word != '') %>%
  anti_join(stop_words) %>%
  anti_join(own_stopwords.en) %>%
  mutate(word = replace(word, word == "users", "user")) %>%
  mutate(word = replace(word, word == "companies", "company"))

# find highest frequency words
news_frequency.en <- news.english.words %>% count(word, sort = TRUE)

# plot highest frequency words
plot_frequency.en <- news_frequency.en %>%
  filter(n > quantile(n, 0.99)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

###################
### For Twitter ###
###################

# load twitter data
load("./Twitter Data/tweet.english.RData")

# clean twitter data- removing links, but retaining hashtags and mentions
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tweet.all_english.words <- tweet.english %>% 
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = "’") %>%
  unnest_tokens(word, word, token = "regex", pattern = "'") %>%
  unnest_tokens(word, word, token = "regex", pattern = unnest_reg) %>%
  mutate(word = gsub('[[:digit:]]+', '', word)) %>%
  filter(word != '') %>%
  anti_join(stop_words) %>%
  anti_join(own_stopwords.en)

# finding most commonly used hashtags
tweet.hashtag_frequency.en <- tweet.all_english.words %>% 
  filter(word == str_extract_all(word, "#\\S+")) %>% 
  count(word, sort = TRUE)

# plot most commonly used hashtags
plot_tweet.hashtag_frequency.en <- tweet.hashtag_frequency.en %>%
  filter(n > quantile(n, 0.995)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

# finding most commonly used mentions
tweet.mention_frequency.en <- tweet.all_english.words %>% 
  filter(word == str_extract_all(word, "@\\S+")) %>% 
  count(word, sort = TRUE)

# plot most commonly used mentions
plot_tweet.mention_frequency.en <- tweet.mention_frequency.en %>%
  filter(n > quantile(n, 0.995)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

# clean twitter data, but removing all links, hashtags, and mentions
tweet.english.words <- tweet.all_english.words %>%
  mutate(word = str_replace_all(word, "#\\S+", "")) %>%
  mutate(word = str_replace_all(word, "@\\S+", "")) %>%
  filter(word != '')

# finding most commonly used words outside of hashtags and mentions
tweet_frequency.en <- tweet.english.words %>% count(word, sort = TRUE)

# plot most commonly used words outside of hashtags and mentions
plot_tweet_frequency.en <- tweet_frequency.en %>%
  filter(n > quantile(n, 0.999)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

