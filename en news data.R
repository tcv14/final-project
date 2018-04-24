source('helper functions.R')

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

nyt <- bind_rows(nyt_1, nyt_2, nyt_3, nyt_4, nyt_5)
save(nyt, file = "./News Article Data/nyt.RData")
load("./News Article Data/nyt.RData")

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

lat <- bind_rows(lat_1, lat_2, lat_3, lat_4, lat_5)
save(lat, file = "./News Article Data/lat.RData")
load("./News Article Data/lat.RData")

cht_1 <- read_article.h1('http://www.chicagotribune.com/entertainment/theater/ct-ae-social-network-facebook-jones-0325-story.html')
cht_1 <- cht_1[-c(25:27),]
cht_2 <- read_article.h1('http://www.chicagotribune.com/business/ct-facebook-mark-zuckerberg-congress-20180406-story.html')
cht_3 <- read_article.h1('http://www.chicagotribune.com/news/nationworld/ct-cambridge-analytica-facebook-20180321-story.html')
cht_4 <- read_article.h1('http://www.chicagotribune.com/business/ct-mark-zuckerberg-facebook-capitol-hill-20180409-story.html')
cht_5 <- read_article.h1('http://www.chicagotribune.com/business/ct-biz-mark-zuckerberg-congress-testify-20180328-story.html')

cht <- bind_rows(cht_1, cht_2, cht_3, cht_4, cht_5)
save(cht, file = "./News Article Data/cht.RData")
load("./News Article Data/cht.RData")

hsc_1 <- read_article.h1('https://www.chron.com/business/technology/article/Yes-Mark-Zuckerberg-will-wear-a-suit-to-Congress-12820177.php')
hsc_2 <- read_article.h12('https://www.houstonchronicle.com/news/article/Facebook-s-Mark-Zuckerberg-to-Capitol-Hill-It-12818356.php')
hsc_2 <- hsc_2[-c(30:99),]
hsc_3 <- read_article.h1('https://www.chron.com/business/technology/article/watch-live-Facebook-Mark-Zuckerberg-congress-12819262.php')
dln_1 <- read_article.h123('https://www.dallasnews.com/business/technology/2018/04/10/mistake-sorry-facebook-ceo-mark-zuckerberg-faces-congress')
dln_2 <- read_article.h1('https://www.dallasnews.com/opinion/editorials/2018/04/10/mark-zuckerbergs-congressional-testimony-reflects-importance-data-security')

tex <- bind_rows(hsc_1, hsc_2, hsc_3, dln_1, dln_2)
save(tex, file = "./News Article Data/tex.RData")
load("./News Article Data/tex.RData")

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

sfc <- bind_rows(sfc_1, sfc_2, sfc_3, sfc_4, sfc_5)
save(sfc, file = "./News Article Data/sfc.RData")
load("./News Article Data/sfc.RData")

# complete english news articles
news.english <- bind_rows(nyt, lat, cht, tex, sfc)
save(news.english, file = "./News Article Data/news.english.RData")
load("./News Article Data/news.english.RData")
