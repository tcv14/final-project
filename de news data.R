# sourcing helper functions
source('helper functions.R')

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
