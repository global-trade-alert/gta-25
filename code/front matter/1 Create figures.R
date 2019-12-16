rm(list=ls())

## Req: Create bar chart with jumbos introduced in populist era

library(gtalibrary)
library(stringr)
library(tidyverse)
library(plyr)
library(data.table)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
chapter.folders=gta25_setup(internal.name = 'front matter',
                            in.dev = F,
                            author=NULL,
                            wipe.data = F,
                            wipe.figs = T)

data.path=chapter.folders$data.path
figure.path=chapter.folders$figure.path

gta_colour_palette()

###### MAPS

# The first map would show the number of times each country's commercial interests have been hit from 1 Jan 2017 to 15 Nov 2019. 

load("0 gtalibrary/data/world.geo.rda")

world <- world.geo

data[,c("UN","value")] <- data[,c("un_code","implemented")]
data$UN <- gta_un_code_vector(data$UN)

# merge data with map data
world = merge(world, data[,c("UN","value")], by="UN", all.x=T)

###### IMPORTANT, sort for X (id) again
world <-  world[with(world, order(X)),]
# world$value[is.na(world$value) == T] <- 0

plot=ggplot() +
  geom_polygon(data=subset(world, country != "Antarctica"), aes(x = long, y = lat, group = group, fill = value), size = 0.2, color = "white") +
  coord_fixed() + # Important to fix world map proportions
  labs(x="", y="") +
  scale_fill_gradient(low = gta_colour$blue[4], high = gta_colour$blue[1], breaks=waiver(), position="bottom", na.value = "#CCCCCC") + # Set color gradient
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

plot

gta_plot_saver(plot=plot,
               path = paste0(figure.path),
               name= "TOP - Number of hits",
               eps = T,
               png=T)



# The second map would show the share of a country's exports that are affected by harmful foreign measures implemented from 1 Jan 2017 to 15 Nov 2019. 

world <- world.geo

data[,c("UN","value")] <- data[,c("un_code","implemented")]
data$UN <- gta_un_code_vector(data$UN)

# merge data with map data
world = merge(world, data[,c("UN","value")], by="UN", all.x=T)

###### IMPORTANT, sort for X (id) again
world <-  world[with(world, order(X)),]
# world$value[is.na(world$value) == T] <- 0




plot=ggplot() +
  geom_polygon(data=subset(world, country != "Antarctica"), aes(x = long, y = lat, group = group, fill = value), size = 0.2, color = "white") +
  coord_fixed() + # Important to fix world map proportions
  labs(x="", y="") +
  scale_fill_gradient(low = gta_colour$blue[4], high = gta_colour$blue[1], breaks=waiver(), position="bottom", na.value = "#CCCCCC") + # Set color gradient
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

plot

gta_plot_saver(plot=plot,
               path = paste0(figure.path),
               name= "BOTTOM - Affected export shares",
               eps = T,
               png=T)
