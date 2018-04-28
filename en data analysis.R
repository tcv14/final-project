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
  mutate(word = gsub('[[:punct:]]+', '', word)) %>%
  filter(word != '') %>%
  anti_join(stop_words) %>%
  anti_join(own_stopwords.en) %>%
  mutate(word = replace(word, word == "users", "user")) %>%
  mutate(word = replace(word, word == "companies", "company"))

# find highest frequency words
news_count.en <- news.english.words %>% count(word, sort = TRUE)

# plot highest frequency words
plot_count.en <- news_count.en %>%
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
tweet.hashtag_count.en <- tweet.all_english.words %>% 
  filter(word == str_extract_all(word, "#\\S+")) %>% 
  count(word, sort = TRUE)

# plot most commonly used hashtags
plot_tweet.hashtag_count.en <- tweet.hashtag_count.en %>%
  filter(n > quantile(n, 0.995)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

# finding most commonly used mentions
tweet.mention_count.en <- tweet.all_english.words %>% 
  filter(word == str_extract_all(word, "@\\S+")) %>% 
  count(word, sort = TRUE)

# plot most commonly used mentions
plot_tweet.mention_count.en <- tweet.mention_count.en %>%
  filter(n > quantile(n, 0.995)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

# clean twitter data, but removing all links, hashtags, and mentions
tweet.english.words <- tweet.all_english.words %>%
  mutate(word = str_replace_all(word, "#\\S+", "")) %>%
  mutate(word = str_replace_all(word, "@\\S+", "")) %>%
  mutate(word = gsub('[[:punct:]]+', '', word)) %>%
  filter(word != '')

# finding most commonly used words outside of hashtags and mentions
tweet_count.en <- tweet.english.words %>% count(word, sort = TRUE)

# plot most commonly used words outside of hashtags and mentions
plot_tweet_count.en <- tweet_count.en %>%
  filter(n > quantile(n, 0.999)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

######################################################
### Comparing News Articles and Twitter Word Usage ###
######################################################

# find frequency for each word in the news articles and from twitter
frequency.en <- bind_rows(mutate(news.english.words, type = "News Article"),
                          mutate(tweet.english.words, type = "Twitter")) %>%
  count(type, word) %>%
  group_by(type) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(type, proportion) %>%
  gather(type, proportion, `News Article`)

library(scales)

# plot frequencies on same plot, words closer to the line have similar frequencies in both types of text
ggplot(frequency.en, aes(x = proportion, y = `Twitter`, color = abs(`Twitter` - proportion))) +
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
correlation <- cor.test(data = frequency.en[frequency.en$type == "News Article",], ~ proportion + `Twitter`)
#knitr::kable(correlation)

##########################
### Sentiment Analysis ###
##########################

# load sentiments
afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
# positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust
nrc <- get_sentiments("nrc")

### Looking at news articles ###

# starting with the nrc sentiment for negative
nrc_negative <- nrc %>% filter(sentiment == "negative")
news.negative <- news.english.words %>%
  inner_join(nrc_negative) %>%
  count(word, sort = TRUE)

# then moving to the nrc sentiment for anger
nrc_anger <- nrc %>% filter(sentiment == "anger")
news.anger <- news.english.words %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

# finally examining the nrc sentiment for disgust
nrc_disgust <- nrc %>% filter(sentiment == "disgust")
news.disgust <- news.english.words %>%
  inner_join(nrc_disgust) %>%
  count(word, sort = TRUE)

news.bing <- news.english.words %>%
  inner_join(bing) %>%
  count(sentiment) %>%
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative)

