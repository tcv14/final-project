#detach("package:igraph") # only run if running this R script after the "de data analysis.R" script
library(igraph)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(reshape2)
library(scales)
library(ggraph)
library(markovchain)

# load stop words
data(stop_words)

# create own list of stop words based on subject content
own_stopwords.en <- tibble(
  `word` = c("facebook", "mark", "zuckerberg", "cambridge", "analytica", "fb", "didn", "don", "doesn", "ve", "john"))

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
news_count.en <- news.english.words %>% count(word, sort = TRUE) %>%
  mutate(language = "English")

# plot top 10 words
plot_count.en <- news_count.en %>%
  group_by(language) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

# plot a wordcloud of the 50 most commonly used words
wordcloud(news_count.en$word, news_count.en$n, min.freq = 1, max.words = 50, 
            random.order = FALSE, rot.per = 0.25, colors = brewer.pal(8, "Dark2"),
          vfont = c("sans serif", "plain"))

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
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col(fill = "midnightblue") + xlab(NULL) + coord_flip() + ggtitle("English")

# finding most commonly used mentions
tweet.mention_count.en <- tweet.all_english.words %>% 
  filter(word == str_extract_all(word, "@\\S+")) %>% 
  count(word, sort = TRUE)

# plot most commonly used mentions
plot_tweet.mention_count.en <- tweet.mention_count.en %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col(fill = "turquoise4") + xlab(NULL) + coord_flip() + ggtitle("English")

# clean twitter data, but removing all links, hashtags, and mentions
tweet.english.words <- tweet.all_english.words %>%
  mutate(word = str_replace_all(word, "#\\S+", "")) %>%
  mutate(word = str_replace_all(word, "@\\S+", "")) %>%
  mutate(word = gsub('[[:punct:]]+', '', word)) %>%
  filter(word != '')

# finding most commonly used words outside of hashtags and mentions
tweet_count.en <- tweet.english.words %>% count(word, sort = TRUE)
write_csv(tweet_count.en, "./Shiny/tweet_count.en.csv")

# plot most commonly used words outside of hashtags and mentions
plot_tweet_count.en <- tweet_count.en %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col(fill = "royalblue3") + xlab(NULL) + coord_flip() + ggtitle("English")

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
  gather(type, proportion, `News Article`) %>%
  mutate(difference = abs(`Twitter` - proportion))

# plot frequencies on same plot, words closer to the line have similar frequencies in both types of text
plot_frequency.en <- ggplot(frequency.en, aes(x = proportion, y = `Twitter`, color = difference)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.003), low = "darkslategray4", high = "hotpink2") +
  facet_wrap(~type, ncol = 1) +
  theme(legend.position = "right") +
  labs(x = NULL, y = "Twitter") +
  ggtitle("Comparing Word Usage Between Twitter and News Articles (English)")

# correlation test
correlation.en <- cor.test(data = frequency.en[frequency.en$type == "News Article",], ~ proportion + `Twitter`)
#knitr::kable(correlation.en)

##########################
### Sentiment Analysis ###
##########################

# load sentiment lexicons
# scores from -5 to 5, with negative scores indicating negative sentiment and positive scores indicating positive sentiment
afinn <- get_sentiments("afinn")
# positive, negative
bing <- get_sentiments("bing")
# positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, trust
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

wordcloud(news.negative$word, news.negative$n, max.words = 15, random.order = FALSE,
          rot.per = 0.25, colors = brewer.pal(8, "Paired"))
wordcloud(news.anger$word, news.anger$n, max.words = 15, random.order = FALSE,
          rot.per = 0.25, colors = brewer.pal(8, "Set3"))
wordcloud(news.disgust$word, news.disgust$n, max.words = 15, random.order = FALSE,
          rot.per = 0.25, colors = brewer.pal(8, "Accent"))

# using the bing sentiment lexicon
news.bing <- news.english.words %>%
  inner_join(bing) %>%
  count(sentiment) %>%
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative)
#knitr::kable(news.bing[1:3])
news.bing_count <- news.english.words %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
# for shiny app
write_csv(news.bing_count, "./Shiny/news.bing_count.csv")
# plotting negative and positive words side-by-side to compare
plot_news.bing_count <- news.bing_count %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  scale_fill_brewer(palette = "Set3") +
  labs(y = "n", x = NULL) +
  coord_flip()
# plotting a comparison word cloud
news.english.words %>% inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray80", "gray20"), max.words = 20)

# using the afinn sentiment lexicon
news.afinn <- news.english.words %>%
  inner_join(afinn) %>%
  summarize(sentiment = sum(score))
# divide -262 by 5 gives -52.4 (used to compare with German)

### Looking at tweets ###

# starting with the nrc sentiment for negative
tweet.negative <- tweet.english.words %>%
  inner_join(nrc_negative) %>%
  count(word, sort = TRUE)

# then moving to the nrc sentiment for anger
tweet.anger <- tweet.english.words %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

# finally examining the nrc sentiment for disgust
tweet.disgust <- tweet.english.words %>%
  inner_join(nrc_disgust) %>%
  count(word, sort = TRUE)

# using the bing sentiment lexicon
tweet.bing <- tweet.english.words %>%
  inner_join(bing) %>%
  count(sentiment) %>%
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative)
#knitr::kable(tweet.bing)
tweet.bing_count <- tweet.english.words %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
# for shiny app
write_csv(tweet.bing_count, "./Shiny/tweet.bing_count.csv")
# plotting negative and positive words side-by-side to compare
plot_tweet.bing_count <- tweet.bing_count %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  scale_fill_brewer(palette = "Accent") +
  labs(y = "n", x = NULL) +
  coord_flip() +
  ggtitle("English")
# plotting a comparison word cloud
tweet.english.words %>% inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray80", "gray20"), max.words = 20)

# using the afinn sentiment lexicon
tweet.afinn <- tweet.english.words %>%
  inner_join(afinn) %>%
  summarize(sentiment = sum(score))
# divide -8176 by 5 gives -1635.2 (used to compare with German)

#####################
### Markov Chains ###
#####################

### For news

news.english.bigrams <- news.english %>%
  unnest_tokens(bigram, value, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

set.seed(1)
arrow <- grid::arrow(type = "closed", length = unit(0.15, "inches"))

plot_news.english.bigrams <- news.english.bigrams %>%
  filter(n > 7) %>%
  graph_from_data_frame()

plot_news.english.bigrams <- ggraph(plot_news.english.bigrams, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = arrow, end_cap = circle(0.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

### For Twitter

tweet.english.bigrams <- tweet.english %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, "#\\S+", "")) %>%
  mutate(text = str_replace_all(text, "@\\S+", "")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

set.seed(12)
arrow <- grid::arrow(type = "closed", length = unit(0.15, "inches"))

plot_tweet.english.bigrams <- tweet.english.bigrams %>%
  filter(n > 50) %>%
  graph_from_data_frame()

plot_tweet.english.bigrams <- ggraph(plot_tweet.english.bigrams, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = arrow, end_cap = circle(0.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

### Building Markov Chain

# create text file to read in
write.table(news.english, file = "news.english.txt", row.names = FALSE, col.names = FALSE)
# read in text file
news.english.text <- readLines('news.english.txt')
# delete empty lines
news.english.text <- news.english.text[nchar(news.english.text) > 0]
# remove all punctuation
news.english.text <- str_replace_all(news.english.text, "[[:punct:]]", "")
# get a list of just the words split into tokens
news.english.text_terms <- unlist(strsplit(news.english.text, " "))
# creating the model, this takes a few minutes
#news.english.text_fit <- markovchainFit(data = news.english.text_terms)
#mcfit <- news.english.text_fit$estimate
#save(mcfit, file = "./Shiny/mcfit.RData")

# generate text directly after running previous line of code
#news.english.text_generate <- markovchainSequence(n = 10, markovchain = news.english.text_fit$estimate)

# generate text from the .RData file
load("./Shiny/mcfit.RData")
news.english.text_generate <- markovchainSequence(n = 10, markovchain = mcfit)
