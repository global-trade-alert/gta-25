rm(list=ls())

## Req: Create bar chart with jumbos introduced in populist era

library(gtalibrary)
library(stringr)
library(tidyverse)
library(plyr)
library(data.table)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
chapter.folders=gta25_setup(internal.name = 'Jumbos in the populist era',
                            in.dev = F,
                            author=NULL,
                            wipe.data = F,
                            wipe.figs = T)

data.path=chapter.folders$data.path
figure.path=chapter.folders$figure.path

gta_colour_palette()
base.size=12
aspect.ratio = NULL
background.color="#FFFFFF"
font.colour = "#555555"
font.colour.bold = "#555555"
base.family = "Open Sans"
base.family.bold = "Open Sans Bold"
legend.box.align = "horizontal"
legend.title.align = 0
x.bottom.angle = 0
x.top.angle = x.bottom.angle
x.bottom.align = 0.5
y.left.angle = 0
y.right.angle = y.left.angle
legend.position = "top"

load(file = "0 report production/GTA 25/prep pre report/jumbo.data.Rdata")

p1 = ggplot(data=jumbo.data, aes(x=order,y=log10(trade.value/1000000000),fill=factor(Trade.war))) +
  geom_bar(position="dodge",stat="identity", width = .85) + 
  # coord_flip() +
  # geom_text(aes(y=0, label=title), hjust=0, color="white", size=2, vjust=.3)+
  theme(line = element_line(colour = "#FFFFFF", size = 2, linetype = 1, lineend = "square"),
        rect = element_rect(fill = "#FFFFFF", colour="#FFFFFF",size=0, linetype = 1),
        text = element_text(family=base.family, colour = font.colour, size=base.size),
        title = element_text(family=base.family, colour= font.colour, size=base.size),
        aspect.ratio = aspect.ratio,
        axis.title.x = element_text(family=base.family, colour = font.colour, size=base.size*0.9, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x.bottom = element_text(family=base.family, colour = font.colour, size=base.size*0.9, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.x.top = element_text(family=base.family, colour = font.colour, size=base.size*0.9, margin = margin(t = 0, r = 0, b = 10, l = 0)),
        axis.title.y.left = element_text(family=base.family, colour = font.colour, size=base.size*0.9, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.y.right = element_text(family=base.family, colour = font.colour, size=base.size*0.9, margin = margin(t = 0, r = 0, b = 0, l = 10)),
        axis.text.x.bottom = element_blank(),
        axis.text.x.top = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour= "#eeeeee", linetype = 1, size=0),
        legend.background = element_rect(fill="transparent", colour = "#cccccc", size=0, linetype=1),
        legend.position = legend.position,
        legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.title.align = legend.title.align,
        legend.text = element_text(family=base.family, colour = font.colour, size = base.size*0.9, margin = margin(b=0, r=10, l=0), hjust = 0),
        legend.text.align = 0,
        legend.direction = "horizontal",
        legend.key = element_rect(fill="transparent", colour = "#cccccc", size = 0, linetype = 1),
        legend.margin = margin(l = 0),
        legend.box = legend.box.align,
        legend.box.just = 0,
        legend.box.margin = margin(t = 0, r = 0, b = 10, l = 0),
        panel.background = element_rect(fill="#FFFFFF", colour="#999999", size=0.5, linetype=1),
        panel.grid.minor.y = element_line(colour = "#eeeeee", linetype = 1, size = 0.5, lineend = "square"),
        panel.grid.major = element_line(colour = "#eeeeee", linetype = 1, size = 0.5, lineend = "square"),
        panel.grid.minor = element_line(colour = background.color, linetype = 1, size = 0, lineend = "square"),
        plot.background = element_rect(fill = "#F9F9F9", colour="#CCCCCC",size=0, linetype = 1),
        plot.title = element_text(family = base.family, colour = font.colour.bold, face = "bold", size = base.size, hjust = 0),
        plot.subtitle = element_text(family = base.family, colour = font.colour, size = base.size*0.9, hjust=0, margin=margin(t=0, r=0, b=10, l=0)),
        plot.caption = element_text(family = base.family, colour = font.colour, size = base.size*0.7),
        plot.margin = unit(c(0.05,0.05,0.05,0.05), "npc"),
        strip.background = element_rect(fill="#FFFFFF", colour=gta_colour$grey[1], size=0.5, linetype=1),
        strip.text = element_text(family=base.family, colour = font.colour, size=base.size*0.9, hjust = 0.5)) +
  # gta_theme() +
  scale_fill_manual(labels=c("All other jumbos", "US-China trade war", "Non-China-specific trade war", "Subsidies to mitigate trade war effects"), 
                    values=c(gta_colour$blue[1], gta_colour$red[1], gta_colour$brown[c(1,4)]))+
  labs(y="Amount of trade affected in USD (log scale)", x ="Jumbo protectionist interventions implemented since 1 January 2017") +
  scale_y_continuous(limits = c(0,2.5),
                     breaks = c(0,0.69897,1,1.69897,2),
                     labels = c("0"="1 billion ", "0.69897" = "5 billion", "1"="10 billion ", "1.69897" = "50 billion", "2"="100 billion "))
p1

gta_plot_saver(plot = p1,
               path = figure.path,
               name = "Jumbo bar chart",
               cairo_ps = T,
               pdf = T,
               height = 15,
               width = 27)


###### MAPS
## jumbo counts
jumbo.countries <- read.xlsx(paste0("0 report production/GTA 25/help files/trade per intervention_jumbos checked.xlsx"),sheetName ="populist era")
jumbo.countries <- subset(jumbo.countries, is.na(implementing.jurisdiction)==F)
jumbo.countries$implementing.jurisdiction=as.character(jumbo.countries$implementing.jurisdiction)
jumbo.countries$implementing.jurisdiction[grepl("Malta",jumbo.countries$implementing.jurisdiction) & grepl("Finland",jumbo.countries$implementing.jurisdiction)]="European Union"
names(jumbo.countries) <- c("name","implemented")
write.xlsx(jumbo.countries[,c("name","implemented")], file=paste0(figure.path,"Countries implementing jumbo measures.xlsx"),sheetName="implementers",row.names=F)



#Create map
load(paste0(chapter.folders$data.path, "jumbo country names.Rdata"))
data=subset(gtalibrary::country.names, name %in% jumbo.country.names)[,c("un_code","name")]
data$implemented=1

world.geo <- gtalibrary::world.geo

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
               name= "Map jumbo implementers in populist era",
               cairo_ps = T,
               pdf = T,
               png=T)
