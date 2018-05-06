library(ggmap)
library(tidyverse)
library(ggplot2)   
library(grid)

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
    hjust = 0.2, vjust = 2, size = 3) +
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
points.us <- bind_rows(la, sf, hs, da, ch, ny) %>%
  mutate(city = c("Los Angelos", "San Francisco", "Housten", "Dallas", "Chicago", "New York")) %>%
  mutate(newspaper = c("LA Times", "San Francisco Chronicle", "Housten Chronicle", "Dallas News", "Chicago Tribune", "New York Times"))

map.data <- map_data("state")

unitedstates.map <- ggplot(map.data) + 
  geom_map(aes(map_id = region),  
           map = map.data,  
           fill = "white",             
           color = "grey20", size = 0.25) + 
  expand_limits(x = map.data$long, y = map.data$lat) +            
  theme(axis.line = element_blank(),  
        axis.text = element_blank(),  
        axis.ticks = element_blank(),                     
        axis.title = element_blank(),  
        panel.background = element_blank(),  
        panel.border = element_blank(),                     
        panel.grid.major = element_blank(), 
        plot.background = element_blank(),                     
        plot.margin = unit(0 * c( -1.5, -1.5, -1.5, -1.5), "lines")) +  
  geom_point(data = points.us,             
             aes(x = lon, y = lat), size = 3,  
             color = "red") +
  geom_text(data = points.us,
            aes(x = lon, y = lat, label = points.us$city),
            hjust = 0.5, vjust = -1) +
  geom_text(data = points.us,
            aes(x = lon, y = lat, label = points.us$newspaper),
            hjust = 0.2, vjust = 1.5)

save.image(file = "us.RData")
load("us.RData")
