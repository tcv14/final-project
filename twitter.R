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

