library(ggmap)
library(tidyverse)

# map of Germany
germany <- geocode("germany")
hamburg <- geocode("hamburg")
berlin <- geocode("berlin")
bonn <- geocode("bonn")
frankfurt <- geocode("frankfurt am main")
munich <- geocode("munich")

ger.map <- get_map(germany, maptype = "roadmap", zoom = 6)
germany.map <- ggmap(ger.map, extent = "device") +
  geom_point(
    data = hamburg,
    aes(x = lon, y = lat),
    color = "red", size = 3) +
  geom_text(
    data = hamburg,
    aes(label = "tagesschau"),
    hjust = 0.5, vjust = 2, size = 3) +
  geom_point(
    data = berlin,
    aes(x = lon, y = lat),
    color = "red", size = 3) +
  geom_text(
    data = berlin,
    aes(label = "Berliner Morgenpost"),
    hjust = 0.5, vjust = 2, size = 3) +
  geom_point(
    data = bonn,
    aes(x = lon, y = lat),
    color = "red", size = 3) +
  geom_text(
    data = bonn,
    aes(label = "Bonn"),
    hjust = -0.1, size = 4) +
  geom_text(
    data = bonn,
    aes(label = "Deutsche Welle"),
    hjust = 0.4, vjust = 2, size = 3) +
  geom_point(
    data = frankfurt,
    aes(x = lon, y = lat),
    color = "red", size = 3) +
  geom_text(
    data = frankfurt,
    aes(label = "Frankfurter Allgemeine Zeitung"),
    hjust = 0.5, vjust = 2, size = 3) +
  geom_point(
    data = munich,
    aes(x = lon, y = lat),
    color = "red", size = 3) +
  geom_text(
    data = munich,
    aes(label = "Süddeutsche Zeitung"),
    hjust = 0.6, vjust = 2, size = 3)

save.image(file = "germany.RData")
load("germany.RData")


# map of United States
us <- geocode("united states")
la <- geocode("los angelos")
sf <- geocode("san francisco")
hs <- geocode("housten")
da <- geocode("dallas")
ch <- geocode("chicago")
ny <- geocode("new york city")

us.map <- get_map(us, maptype = "roadmap", zoom = 4)
unitedstates.map <- ggmap(us.map, extent = "device") +
  geom_point(
    data = la,
    aes(x = lon, y = lat),
    color = "red", size = 3) +
  geom_text(
    data = la,
    aes(label = "Los Angelos Times"),
    hjust = 0.5, vjust = -2, size = 3) +
  geom_point(
    data = sf,
    aes(x = lon, y = lat),
    color = "red", size = 3) +
  geom_text(
    data = sf,
    aes(label = "San Francisco Chronicle"),
    hjust = -0.1, size = 3) +
  geom_point(
    data = hs,
    aes(x = lon, y = lat),
    color = "red", size = 3) +
  geom_text(
    data = hs,
    aes(label = "Housten Chronicle"),
    hjust = 0.5, vjust = 3, size = 3) +
  geom_point(
    data = da,
    aes(x = lon, y = lat),
    color = "red", size = 3) +
  geom_text(
    data = da,
    aes(label = "Dallas News"),
    hjust = 0.5, vjust = 1.5, size = 3) +
  geom_point(
    data = ch,
    aes(x = lon, y = lat),
    color = "red", size = 3) +
  geom_text(
    data = ch,
    aes(label = "Chicago Tribune"),
    hjust = -0.05, vjust = 1.5, size = 3) +
  geom_point(
    data = ny,
    aes(x = lon, y = lat),
    color = "red", size = 3) +
  geom_text(
    data = ny,
    aes(label = "New York"),
    hjust = -0.1, size = 3) +
  geom_text(
    data = ny,
    aes(label = "New York Times"),
    hjust = 0.9, vjust = -1, size = 3)

save.image(file = "us.RData")
load("us.RData")
