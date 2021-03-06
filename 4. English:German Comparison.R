# source necessary files
source("2. English Text Analysis.R")
source("3. German Text Analysis.R")

# frequency of words used
count.de.en <- bind_rows(news_count.en, news_count.de)
write_csv(count.de.en, "./Shiny/count.de.en.csv")

# plot english and german frequency together
plot_count.de.en <- count.de.en %>%
  group_by(language) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = language)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~language, scales = "free_y") +
  labs(y = "n", x = NULL) +
  coord_flip()