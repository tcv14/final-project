source("en data analysis.R")
source("de data analysis.R")

# frequency of words used
plot_count.de.en <- bind_rows(news_count.en, news_count.de) %>%
  group_by(language) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = language)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~language, scales = "free_y") +
  labs(y = NULL, x = NULL) +
  coord_flip()
