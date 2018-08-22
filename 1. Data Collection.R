#############
## Twitter ##
#############

library(twitteR)

# set up authorization to mine data from Twitter API
consumerKey <- "aJNOJTkWVtVeHpg6j1aClFIDr"
consumerSecret <- "cXoHnFrryqmfjS0uB13x66HhODPhfFO5gRQWCE5KBMI5wbeyGm"
api_key <- "aJNOJTkWVtVeHpg6j1aClFIDr"
api_secret <- "cXoHnFrryqmfjS0uB13x66HhODPhfFO5gRQWCE5KBMI5wbeyGm"
access_token <- "975190164925485056-kcPzJmWbOKe8K2XSPiRolaHj6mdKjAp"
access_token_secret <- "JCTKEegnNogKQxTyxndhrfVQip6EPrjHZzJXOEVcnTU2E"
my_oauth <- setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# search English tweets with the hashtag "MarkZuckerberg"
#facebook_en <- searchTwitter('#MarkZuckerberg -filter:retweets', n=10000, lang="en")
#facebook_en <- twListToDF(facebook_en)
save(facebook_en, file = "./Twitter Data/facebook_en.Rdata")
load("./Twitter Data/facebook_en.RData")

# search English tweets with the hashtag "Zuckerberg"
#facebook_en2 <- searchTwitter('#Zuckerberg -filter:retweets', n=10000, lang="en")
#facebook_en2 <- twListToDF(facebook_en2)
save(facebook_en2, file = "./Twitter Data/facebook_en2.Rdata")
load("./Twitter Data/facebook_en2.RData")

# search English tweets with the hashtag "FacebookDataBreach"
#facebook_en3 <- searchTwitter('#FacebookDataBreach -filter:retweets', n=5000, lang="en")
#facebook_en3 <- twListToDF(facebook_en3)
save(facebook_en3, file = "./Twitter Data/facebook_en3.Rdata")
load("./Twitter Data/facebook_en3.RData")

# search English tweets with the hashtag "Facebook" and the word "Zuckerberg" in the tweet
#facebook_en4 <- searchTwitter('#Facebook + Zuckerberg -filter:retweets', n=15000, lang="en")
#facebook_en4 <- twListToDF(facebook_en4)
save(facebook_en4, file = "./Twitter Data/facebook_en4.Rdata")
load("./Twitter Data/facebook_en4.RData")

# search German tweets with the hashtag "MarkZuckerberg"
#facebook_de <- searchTwitter('#MarkZuckerberg -filter:retweets', n=1500, lang="de")
#facebook_de <- twListToDF(facebook_de)
save(facebook_de, file = "./Twitter Data/facebook_de.Rdata")
load("./Twitter Data/facebook_de.RData")

# search German tweets with the hashtag "Zuckerberg"
#facebook_de2 <- searchTwitter('#Zuckerberg -filter:retweets', n=1500, lang="de")
#facebook_de2 <- twListToDF(facebook_de2)
save(facebook_de2, file = "./Twitter Data/facebook_de2.Rdata")
load("./Twitter Data/facebook_de2.RData")

# search German tweets with the hashtag "FacebookDataBreach"
#facebook_de3 <- searchTwitter('#FacebookDataBreach -filter:retweets', n=1500, lang="de")
#facebook_de3 <- twListToDF(facebook_de3)
save(facebook_de3, file = "./Twitter Data/facebook_de3.Rdata")
load("./Twitter Data/facebook_de3.RData")

# search German tweets with the hashtag "Datenskandal"
#facebook_de4 <- searchTwitter('#Datenskandal -filter:retweets', n=1500, lang="de")
#facebook_de4 <- twListToDF(facebook_de4)
save(facebook_de4, file = "./Twitter Data/facebook_de4.Rdata")
load("./Twitter Data/facebook_de4.RData")

# search German tweets with the hashtag "Facebook" and the word "Zuckerberg" in the tweet
#facebook_de5 <- searchTwitter('#Facebook + Zuckerberg -filter:retweets', n=1000, lang="de")
#facebook_de5 <- twListToDF(facebook_de5)
save(facebook_de5, file = "./Twitter Data/facebook_de5.Rdata")
load("./Twitter Data/facebook_de5.RData")

# bind all english tweets together, only keeping the text portion
tweet.english <- as_data_frame(bind_rows(facebook_en[1], facebook_en2[1], facebook_en3[1], facebook_en4[1]))
save(tweet.english, file = "./Twitter Data/tweet.english.Rdata")
load("./Twitter Data/tweet.english.RData")

# bind all german tweets together, only keeping the text portion
tweet.german <- as_data_frame(bind_rows(facebook_de[1], facebook_de2[1], facebook_de3[1], facebook_de4[1], facebook_de5[1]))
save(tweet.german, file = "./Twitter Data/tweet.german.Rdata")
load("./Twitter Data/tweet.german.RData")


###################
## News Articles ##
###################

## helper functions for reading in news articles ##

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

## reading in English news articles ##

# read in 5 articles from New York Times and get rid of unnecessary rows
nyt_1 <- read_article.h1('https://www.nytimes.com/2018/04/10/us/politics/zuckerberg-facebook-senate-hearing.html')
nyt_1 <- nyt_1[-c(42:51),]
nyt_2 <- read_article.h1('https://www.nytimes.com/2018/03/21/technology/mark-zuckerberg-facebook.html')
nyt_2 <- nyt_2[-c(14:18,24:25),]
nyt_3 <- read_article.h1('https://www.nytimes.com/2018/03/23/technology/zuckerberg-facebook-employees.html')
nyt_3 <- nyt_3[-c(14:18,29:33),]
nyt_4 <- read_article.h1('https://www.nytimes.com/2018/04/09/technology/mark-zuckerberg-facebook.html')
nyt_4 <- nyt_4[-c(23:27,40:44),]
nyt_5 <- read_article.h1('https://www.nytimes.com/2018/03/21/technology/facebook-zuckerberg-data-privacy.html?action=Click&contentCollection=BreakingNews&contentID=66708068&pgtype=sectionfront')
nyt_5 <- nyt_5[-c(33:43),]

# bind all articles together and save as one file
nyt <- bind_rows(nyt_1, nyt_2, nyt_3, nyt_4, nyt_5)
save(nyt, file = "./News Article Data/nyt.RData")
load("./News Article Data/nyt.RData")

# read in 5 articles from LA Times and get rid of unnecessary rows
lat_1 <- read_article.h1ul('http://www.latimes.com/business/technology/la-fi-tn-facebook-silence-20180320-story.html')
lat_1 <- lat_1[-c(2,31:32),]
lat_2 <- read_article.h1ul('http://www.latimes.com/business/technology/la-fi-tn-facebook-zuckerberg-20180404-story.html')
lat_2 <- lat_2[-c(15:19),]
lat_3 <- read_article.h1ul('http://www.latimes.com/business/la-fi-tn-zuckerberg-facebook-20180321-story.html')
lat_3 <- lat_3[-c(2,29:34),]
lat_4 <- read_article.h1ul('http://www.latimes.com/business/technology/la-fi-tn-zuckerberg-facebook-regulation-20180411-story.html')
lat_4 <- lat_4[-c(28:31),]
lat_5 <- read_article.h1ul('http://www.latimes.com/entertainment/tv/la-et-st-mark-zuckerberg-20180410-story.html')
lat_5 <- lat_5[-c(2,33:35),]

# bind all articles together and save as one file
lat <- bind_rows(lat_1, lat_2, lat_3, lat_4, lat_5)
save(lat, file = "./News Article Data/lat.RData")
load("./News Article Data/lat.RData")

# read in 5 articles from Chicago Chronicle and get rid of unnecessary rows
cht_1 <- read_article.h1('http://www.chicagotribune.com/entertainment/theater/ct-ae-social-network-facebook-jones-0325-story.html')
cht_1 <- cht_1[-c(25:27),]
cht_2 <- read_article.h1('http://www.chicagotribune.com/business/ct-facebook-mark-zuckerberg-congress-20180406-story.html')
cht_3 <- read_article.h1('http://www.chicagotribune.com/news/nationworld/ct-cambridge-analytica-facebook-20180321-story.html')
cht_4 <- read_article.h1('http://www.chicagotribune.com/business/ct-mark-zuckerberg-facebook-capitol-hill-20180409-story.html')
cht_5 <- read_article.h1('http://www.chicagotribune.com/business/ct-biz-mark-zuckerberg-congress-testify-20180328-story.html')

# bind all articles together and save as one file
cht <- bind_rows(cht_1, cht_2, cht_3, cht_4, cht_5)
save(cht, file = "./News Article Data/cht.RData")
load("./News Article Data/cht.RData")

# read in 3 articles from Housten Chronicle and 2 articles from Dallas News and get rid of unnecessary rows
hsc_1 <- read_article.h1('https://www.chron.com/business/technology/article/Yes-Mark-Zuckerberg-will-wear-a-suit-to-Congress-12820177.php')
hsc_2 <- read_article.h12('https://www.houstonchronicle.com/news/article/Facebook-s-Mark-Zuckerberg-to-Capitol-Hill-It-12818356.php')
hsc_2 <- hsc_2[-c(30:99),]
hsc_3 <- read_article.h1('https://www.chron.com/business/technology/article/watch-live-Facebook-Mark-Zuckerberg-congress-12819262.php')
dln_1 <- read_article.h123('https://www.dallasnews.com/business/technology/2018/04/10/mistake-sorry-facebook-ceo-mark-zuckerberg-faces-congress')
dln_2 <- read_article.h1('https://www.dallasnews.com/opinion/editorials/2018/04/10/mark-zuckerbergs-congressional-testimony-reflects-importance-data-security')

# bind all articles together and save as one file
tex <- bind_rows(hsc_1, hsc_2, hsc_3, dln_1, dln_2)
save(tex, file = "./News Article Data/tex.RData")
load("./News Article Data/tex.RData")

# read in 5 articles from San Francisco Chronicle and get rid of unnecessary rows
sfc_1 <- read_article.h12('https://www.sfchronicle.com/business/article/Why-Mark-Zuckerberg-should-step-down-as-Facebook-12813743.php')
sfc_1 <- sfc_1[-c(37:41),]
sfc_2 <- read_article.h12('https://www.sfchronicle.com/business/article/Mark-Zuckerberg-finally-says-sorry-for-12772062.php')
sfc_2 <- sfc_2[-c(22:26),]
sfc_3 <- read_article.h12('https://www.sfchronicle.com/business/article/Remember-the-last-time-Mark-Zuckerberg-asked-us-12771600.php')
sfc_3 <- sfc_3[-c(14:19),]
sfc_4 <- read_article.h12('https://www.sfchronicle.com/business/article/Facebook-s-Zuckerberg-gives-hint-of-what-he-12806395.php')
sfc_4 <- sfc_4[-c(32:36),]
sfc_5 <- read_article.h12('https://www.sfchronicle.com/business/article/Facebook-declines-to-confirm-report-that-12784782.php')
sfc_5 <- sfc_5[-c(28:32),]

# bind all articles together and save as one file
sfc <- bind_rows(sfc_1, sfc_2, sfc_3, sfc_4, sfc_5)
save(sfc, file = "./News Article Data/sfc.RData")
load("./News Article Data/sfc.RData")

# complete english news articles
news.english <- bind_rows(nyt, lat, cht, tex, sfc) %>% 
  filter(value != ' ') %>%
  filter(value != '') %>%
  unique()

# save complete english news articles
save(news.english, file = "./News Article Data/news.english.RData")
load("./News Article Data/news.english.RData")

## reading in German news articles ##

# read in 5 articles from Deutsche Welle and get rid of unnecessary rows
dw_1 <- read_article.h1('http://www.dw.com/de/facebook-datenskandal-was-bisher-geschah/a-43322775')
dw_1 <- dw_1[1:23,]
dw_2 <- read_article.h1('http://www.dw.com/de/facebook-auch-zuckerbergs-daten-gingen-an-cambridge-analytica/a-43348946')
dw_2 <- dw_2[1:14,]
dw_3 <- read_article.h1('http://www.dw.com/de/facebook-datenskandal-deutlich-gr%C3%B6%C3%9Fer-als-angenommen/a-43257966')
dw_3 <- dw_3[1:18,]
dw_4 <- read_article.h1('http://www.dw.com/de/facebook-chef-mark-zuckerberg-es-tut-mir-leid/a-43315648')
dw_4 <- dw_4[1:16,]
dw_5 <- read_article.h1('http://www.dw.com/de/facebook-skandal-datenklau-kennt-keine-landesgrenzen/a-43278436')
dw_5 <- dw_5[1:24,]

# bind all articles together and save as one file
dw <- bind_rows(dw_1, dw_2, dw_3, dw_4, dw_5)
save(dw, file = "./News Article Data/dw.RData")
load("./News Article Data/dw.RData")

# read in 5 articles from tagesschau and get rid of unnecessary rows
ts_1 <- read_article.h124('https://www.tagesschau.de/ausland/facebook-zuckerberg-us-kongress-101.html')
ts_1 <- ts_1[-c(5,10:11,13,16:17,21:26,29,31,35:40,44,50:56),]
ts_2 <- read_article.h124('https://www.tagesschau.de/ausland/facebook-zuckerberg-congress-101.html')
ts_2 <- ts_2[-c(5,8:9,14:19,23:28,31:32,39,42:44),]
ts_3 <- read_article.h124('https://www.tagesschau.de/ausland/facebook-383.html')
ts_3 <- ts_3[-c(6,8:10,14:15,25:30),]
ts_4 <- read_article.h124('https://www.tagesschau.de/ausland/facebook-interview-101.html')
ts_4 <- ts_4[-c(5,7:9,14,26:31),]
ts_5 <- read_article.h124('https://www.tagesschau.de/ausland/cambridge-analytica-durchsuchungen-101.html')
ts_5 <- ts_5[-c(7:10,31:33),]

# bind all articles together and save as one file
ts <- bind_rows(ts_1, ts_2, ts_3, ts_4, ts_5)
save(ts, file = "./News Article Data/ts.RData")
load("./News Article Data/ts.RData")

# read in 5 articles from Frankfurter Allgemeine and get rid of unnecessary rows
fa_1 <- read_article.h123('http://www.faz.net/aktuell/wirtschaft/diginomics/datenskandal-um-facebook-mark-zuckerberg-bleibt-optimistisch-15523561.html')
fa_1 <- fa_1[-c(6:42,44:45,55:63,67:112),]
fa_2 <- read_article.h123('http://www.faz.net/aktuell/wirtschaft/diginomics/facebook-datenskandal-mark-zuckerberg-gibt-fehler-zu-15534285.html')
fa_2 <- fa_2[-c(7:43,46:47,52:56,63:110),]
fa_3 <- read_article.h123('http://www.faz.net/aktuell/wirtschaft/diginomics/amerikanischer-senat-bestellt-facebook-chef-zuckerberg-ein-15514592.html')
fa_3 <- fa_3[-c(4:39,42:43,47:51,53:98),]
fa_4 <- read_article.h123('http://www.faz.net/aktuell/feuilleton/debatten/facebooks-falschheit-mark-zuckerberg-in-washington-15535571.html')
fa_4 <- fa_4[-c(6:42,45:46,56:60,65:110),]
fa_5 <- read_article.h123('http://www.faz.net/aktuell/wirtschaft/diginomics/facebook-chef-mark-zuckerberg-raeumt-fehler-im-datenskandal-ein-15506294.html')
fa_5 <- fa_5[-c(4:40,42:43,53:59,61:104),]

# bind all articles together and save as one file
fa <- bind_rows(fa_1, fa_2, fa_3, fa_4, fa_5)
save(fa, file = "./News Article Data/fa.RData")
load("./News Article Data/fa.RData")

# read in 5 articles from Berliner Morgenpost and get rid of unnecessary rows
bm_1 <- read_article.h23('https://www.morgenpost.de/politik/article213970075/Datenskandal-Facebook-Chef-Zuckerberg-gibt-sich-reumuetig.html')
bm_1 <- bm_1[-c(8,12:14,28:29),]
bm_2 <- read_article.h23('https://www.morgenpost.de/politik/article213987041/So-lullte-Mark-Zuckerberg-den-US-Senat-nach-Datenskandal-ein.html')
bm_2 <- bm_2[-c(7,13:15,33:34),]
bm_3 <- read_article.h23('https://www.morgenpost.de/politik/article214082465/Facebook-Skandal-EU-verlangt-Kooperation-von-Zuckerberg.html')
bm_3 <- bm_3[-c(6:8,16:18),]
bm_4 <- read_article.h23('https://www.morgenpost.de/wirtschaft/article213856375/Zuckerberg-entschuldigt-sich-mit-riesiger-Zeitungsanzeige.html')
bm_4 <- bm_4[-c(7:8,15:16),]
bm_5 <- read_article.h23('https://www.morgenpost.de/politik/ausland/article213780841/Facebook-betrachtet-sich-im-Datenskandal-als-Opfer.html')
bm_5 <- bm_5[-c(2:3,14),]

# bind all articles together and save as one file
bm <- bind_rows(bm_1, bm_2, bm_3, bm_4, bm_5)
save(bm, file = "./News Article Data/bm.RData")
load("./News Article Data/bm.RData")

# read in 5 articles from Süddeutsche Zeitung and get rid of unnecessary rows
sz_1 <- read_article.h123ul('http://www.sueddeutsche.de/digital/cambridge-analytica-etwa-millionen-nutzer-von-facebook-datenskandal-betroffen-1.3932186')
sz_1 <- sz_1[-c(9:28,38:42),]
sz_2 <- read_article.h123ul('http://www.sueddeutsche.de/wirtschaft/facebook-mark-zuckerberg-fehler-datenskandal-1.3937547')
sz_2 <- sz_2[-c(4:6,8:28,38:42),]
sz_3 <- read_article.h123ul('http://www.sueddeutsche.de/digital/zuckerberg-facebook-fehler-1.3916536')
sz_3 <- sz_3[-c(14:35,52:56),]
sz_4 <- read_article.h123ul('http://www.sueddeutsche.de/digital/facebook-chef-vor-us-kongress-zuckerberg-praesentiert-sich-als-erfahrener-entschuldiger-1.3939075')
sz_4 <- sz_4[-c(8:29,55:59),]
sz_5 <- read_article.h123ul('http://www.sueddeutsche.de/wirtschaft/zuckerberg-vor-dem-us-kongress-facebook-das-bin-ich-1.3937816')
sz_5 <- sz_5[-c(6:7,9:29,37:41),]

# bind all articles together and save as one file
sz <- bind_rows(sz_1, sz_2, sz_3, sz_4, sz_5)
save(sz, file = "./News Article Data/sz.RData")
load("./News Article Data/sz.RData")

# complete german news articles
news.german <- bind_rows(dw, ts, fa, bm, sz) %>%
  filter(value != '') %>%
  unique()

# save complete german news articles
save(news.german, file = "./News Article Data/news.german.RData")
load("./News Article Data/news.german.RData")
