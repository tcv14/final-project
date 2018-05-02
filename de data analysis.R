detach("package:igraph")
library(tidyverse)
library(tidytext)
library(stringr)
#install.packages("lsa")
library(lsa)
#install.packages("tm")
library(tm)
library(wordcloud)
library(reshape2)
library(scales)

# load stop words
data(stop_words)
stopwords_lsa <- as_data_frame(stopwords_de)
colnames(stopwords_lsa) <- "word"
stopwords_tm <- as_data_frame(stopwords("german"))
colnames(stopwords_tm) <- "word"
own_stopwords.de <- tibble(
  `word` = c("facebook", "mark", "zuckerberg", "zuckerbergs", "cambridge", "analytica", "us",
              "nun", "dabei", "dafür", "darauf", "hätten", "deren", "ja", "eigentlich",
              "kommt", "mal", "heute", "gerade", "schon", "warum", "sagt", "beim", "gibt",
              "gestern", "marc", "john"))

library(igraph)

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
  anti_join(stopwords_tm) %>%
  anti_join(own_stopwords.de) %>%
  anti_join(stop_words) %>%
  mutate(word = replace(word, word == "nutzern", "nutzer")) %>%
  mutate(word = replace(word, word == "fragen", "frage")) %>%
  mutate(word = replace(word, word == "informationen", "information")) %>%
  mutate(word = replace(word, word == "firmen", "firma"))

# find highest frequency words
news_count.de <- news.german.words %>% count(word, sort = TRUE) %>%
  mutate(language = "German")

# plot top 10 words
plot_count.de <- news_count.de %>%
  group_by(language) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

# plot a wordcloud of the 50 most commonly used words
wordcloud(news_count.de$word, news_count.de$n, min.freq = 1, max.words = 50, 
          random.order = FALSE, rot.per = 0.25, colors = brewer.pal(8, "Dark2"))

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
  anti_join(stopwords_tm) %>%
  anti_join(own_stopwords.de) %>%
  anti_join(stop_words)

# finding most commonly used hashtags
tweet.hashtag_count.de <- tweet.all_german.words %>% 
  filter(word == str_extract_all(word, "#\\S+")) %>% 
  count(word, sort = TRUE)

# plot most commonly used hashtags
plot_tweet.hashtag_count.de <- tweet.hashtag_count.de %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col(fill = "mediumvioletred") + xlab(NULL) + coord_flip() + ggtitle("German")

# finding most commonly used mentions
tweet.mention_count.de <- tweet.all_german.words %>% 
  filter(word == str_extract_all(word, "@\\S+")) %>% 
  count(word, sort = TRUE)

# plot most commonly used mentions
plot_tweet.mention_count.de <- tweet.mention_count.de %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col(fill = "violetred4") + xlab(NULL) + coord_flip() + ggtitle("German")

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
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col(fill = "palevioletred4") + xlab(NULL) + coord_flip() + ggtitle("German")


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
  gather(type, proportion, `News Article`) %>%
  mutate(difference = abs(`Twitter` - proportion))

# plot frequencies on same plot, words closer to the line have similar frequencies in both types of text
plot_frequency.de <- ggplot(frequency.de, aes(x = proportion, y = `Twitter`, color = difference)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.003), low = "darkslategray4", high = "hotpink2") +
  facet_wrap(~type, ncol = 1) +
  theme(legend.position = "right") +
  labs(y = "Twitter", x = NULL) +
  ggtitle("Comparing Word Usage Between Twitter and News Articles (German)")

# correlation test
correlation.de <- cor.test(data = frequency.de[frequency.de$type == "News Article",], ~ proportion + `Twitter`)
#knitr::kable(correlation.de)

##########################
### Sentiment Analysis ###
##########################

# Wörter laden und vorbereiten
sentiment.de <- c(
  # positive Wörter
  readLines(paste0("SentiWS_v1.8c_Positive.txt"), encoding = "UTF-8"),
  # negative Wörter
  readLines(paste0("SentiWS_v1.8c_Negative.txt"), encoding = "UTF-8")) %>% 
  lapply(function(x) {
    # Extrahieren der einzelnen Spalten
    res <- strsplit(x, "\t", fixed = TRUE)[[1]]
    return(data.frame(words = res[1], value = res[2], stringsAsFactors = FALSE))}) %>%
  bind_rows() %>% 
  mutate(word = gsub("\\|.*", "", words) %>% tolower, value = as.numeric(value)) %>% 
  # manche Wörter kommen doppelt vor, hier nehmen wir den mittleren Wert
  group_by(word) %>% summarise(value = mean(value)) %>% 
  ungroup() %>%
  mutate(sentiment = ifelse(value > 0, "positive", "negative"))

### Looking at news articles ###

posneg <- sentiment.de %>%
  select(word, sentiment)

# using the positive/negative sentiment
news.posneg <- news.german.words %>%
  inner_join(posneg) %>%
  count(sentiment) %>%
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative)
#knitr::kable(news.posneg[1:3])
news.posneg_count <- news.german.words %>%
  inner_join(posneg) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
# plotting negative and positive words side-by-side to compare
plot_news.posneg_count <- news.posneg_count %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "n", x = NULL) +
  coord_flip()
# plotting a comparison word cloud
news.german.words %>% inner_join(posneg) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray80", "gray20"), max.words = 20)

# using the sentiment value assigned to each word
senval <- sentiment.de %>%
  select(word, value)

news.senval <- news.german.words %>%
  inner_join(senval) %>%
  summarize(sentiment = sum(value))

### Looking at tweets ###

# using the positive/negative sentiment
tweet.posneg <- tweet.german.words %>%
  inner_join(posneg) %>%
  count(sentiment) %>%
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative)
#knitr::kable(news.posneg)
tweet.posneg_count <- tweet.german.words %>%
  inner_join(posneg) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
# plotting negative and positive words side-by-side to compare
plot_tweet.posneg_count <- tweet.posneg_count %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  scale_fill_brewer(palette = "Set1") +
  labs(y = "n", x = NULL) +
  coord_flip() +
  ggtitle("German")
# plotting a comparison word cloud
tweet.german.words %>% inner_join(posneg) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray80", "gray20"), max.words = 20)

# using the sentiment value assigned to each word
tweet.senval <- tweet.german.words %>%
  inner_join(senval) %>%
  summarize(sentiment = sum(value))

#####################
### Markov Chains ###
#####################
library(ggraph)

### For news

news.german.bigrams <- news.german %>%
  unnest_tokens(bigram, value, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stopwords_lsa$word) %>%
  filter(!word1 %in% stopwords_tm$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word2 %in% stopwords_lsa$word) %>%
  filter(!word2 %in% stopwords_tm$word) %>%
  count(word1, word2, sort = TRUE)

set.seed(2)
arrow <- grid::arrow(type = "closed", length = unit(0.15, "inches"))

plot_news.german.bigrams <- news.german.bigrams %>%
  filter(n > 5) %>%
  graph_from_data_frame()

plot_news.german.bigrams <- ggraph(plot_news.german.bigrams, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = arrow, end_cap = circle(0.07, "inches")) +
  geom_node_point(color = "pink", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

### For Twitter

tweet.german.bigrams <- tweet.german %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, "#\\S+", "")) %>%
  mutate(text = str_replace_all(text, "@\\S+", "")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stopwords_lsa$word) %>%
  filter(!word1 %in% stopwords_tm$word) %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word2 %in% stopwords_lsa$word) %>%
  filter(!word2 %in% stopwords_tm$word) %>%
  count(word1, word2, sort = TRUE)

set.seed(22)
arrow <- grid::arrow(type = "closed", length = unit(0.15, "inches"))

plot_tweet.german.bigrams <- tweet.german.bigrams %>%
  filter(n > 7) %>%
  graph_from_data_frame()

plot_tweet.german.bigrams <- ggraph(plot_tweet.german.bigrams, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = arrow, end_cap = circle(0.07, "inches")) +
  geom_node_point(color = "pink", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

### Building Markov Chain
library(markovchain)

# create text file to read in
write.table(news.german, file = "news.german.txt", row.names = FALSE, col.names = FALSE)
# read in text file
news.german.text <- readLines('news.german.txt')
# delete empty lines
news.german.text <- news.german.text[nchar(news.german.text) > 0]
# remove all punctuation
news.german.text <- str_replace_all(news.german.text, "[[:punct:]]", " ")
# get a list of just the words split into tokens
news.german.text_terms <- unlist(strsplit(news.german.text, " "))
news.german.text_terms <- news.german.text_terms[news.german.text_terms != ""]
# creating the model
#news.german.text_fit <- markovchainFit(data = news.german.text_terms)
# generate text
#news.german.text_generate <- markovchainSequence(n = 10, markovchain = news.german.text_fit$estimate)

