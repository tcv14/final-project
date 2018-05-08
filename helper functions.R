# helper functions for reading in news articles
library(tidyverse)
#install.packages("rvest")
library(rvest)

# These functions scrape news articles online by taking in the URL for the article
# the html_node is next matched based on the html encoding give in ""
# after the node is matched, it reads the corresponding text between the nodes
# the function binds all text by row and returns a data frame
# these functions are sourced in the file "en news data.R" and "de news data.R"

# article with header 1
read_article.h1 <- function(newsurl) {
  read <- read_html(newsurl)
  read.h1 <- read %>% html_nodes("h1") %>% html_text()
  read.p <- read %>% html_nodes("p") %>% html_text()
  read.out <- bind_rows(as_data_frame(read.h1), as_data_frame(read.p))
  return(as_data_frame(read.out))
}

# article with header 1 and 2
read_article.h12 <- function(newsurl) {
  read <- read_html(newsurl)
  read.h1 <- read %>% html_nodes("h1") %>% html_text()
  read.h2 <- read %>% html_nodes("h2") %>% html_text()
  read.p <- read %>% html_nodes("p") %>% html_text()
  read.out <- bind_rows(as_data_frame(read.h1), as_data_frame(read.h2)[1,], as_data_frame(read.p))
  return(as_data_frame(read.out))
}

# article with header 2 and 3
read_article.h23 <- function(newsurl) {
  read <- read_html(newsurl)
  read.h2 <- read %>% html_nodes("h2") %>% html_text()
  read.h3 <- read %>% html_nodes("h3") %>% html_text()
  read.p <- read %>% html_nodes("p") %>%  html_text()
  read.out <- bind_rows(as_data_frame(read.h2), as_data_frame(read.h3), as_data_frame(read.p))
  return(as_data_frame(read.out))
}

# article with header 1, 2, 4
read_article.h124 <- function(newsurl) {
  read <- read_html(newsurl)
  read.h1 <- read %>% html_nodes("h1") %>% html_text()
  read.h2 <- read %>% html_nodes("h2") %>% html_text()
  read.h4 <- read %>% html_nodes("h4") %>% html_text()
  read.p <- read %>% html_nodes("p") %>% html_text()
  read.out <- bind_rows(as_data_frame(read.h1), as_data_frame(read.h2), as_data_frame(read.h4), as_data_frame(read.p))
  return(as_data_frame(read.out))
}

# article with header 1, 2, 3
read_article.h123 <- function(newsurl) {
  read <- read_html(newsurl)
  read.h1 <- read %>% html_nodes("h1") %>% html_text()
  read.h2 <- read %>% html_nodes("h2") %>% html_text()
  read.h3 <- read %>% html_nodes("h3") %>% html_text()
  read.p <- read %>% html_nodes("p") %>% html_text()
  read.out <- bind_rows(as_data_frame(read.h1), as_data_frame(read.h2), as_data_frame(read.h3), as_data_frame(read.p))
  return(as_data_frame(read.out))
}

# article with header 1 and unordered list
read_article.h1ul <- function(newsurl) {
  read <- read_html(newsurl)
  read.h1 <- read %>% html_nodes("h1") %>% html_text()
  read.ul <- read %>% html_nodes("ul") %>% html_text()
  read.p <- read %>% html_nodes("p") %>% html_text()
  read.out <- bind_rows(as_data_frame(read.h1), as_data_frame(read.ul)[23,], as_data_frame(read.p))
  return(as_data_frame(read.out))
}

# article with header 1, 2, 3, and unordered list
read_article.h123ul <- function(newsurl) {
  read <- read_html(newsurl)
  read.h1 <- read %>% html_nodes("h1") %>% html_text()
  read.h2 <- read %>% html_nodes("h2") %>% html_text()
  read.h3 <- read %>% html_nodes("h3") %>% html_text()
  read.ul <- read %>% html_nodes("ul") %>% html_text()
  read.p <- read %>% html_nodes("p") %>% html_text()
  read.out <- bind_rows(as_data_frame(read.h1), as_data_frame(read.h2), as_data_frame(read.h3), as_data_frame(read.ul)[c(24:26),], as_data_frame(read.p))
  return(as_data_frame(read.out))
}