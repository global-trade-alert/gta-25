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

# The first map would show the number of times 
# each country's commercial interests have been hit from 1 Jan 2017 to 15 Nov 2019. 

load(paste0(data.path,"Nr of hits in populist era.Rdata"))
write.xlsx(nr.hits[,c("affected.jurisdiction","intervention.id","type")], file=paste0(figure.path,"Nr of hits in populist era.xlsx"),sheetName="Nr of hits",row.names=F)

map.nr.hits <- function(type.eval=NULL, color.low=NULL, color.high=NULL, legend.name=NULL) {
  world.geo <- gtalibrary::world.geo
  # load("0 gtalibrary/data/world.geo.rda")
  
  world <- world.geo
  data <- subset(nr.hits, type == type.eval)
  data[,c("UN","value")] <- data[,c("a.un","intervention.id")]
  data$UN <- gta_un_code_vector(data$UN)
  
  # merge data with map data
  world = merge(world, data[,c("UN","value")], by="UN", all.x=T)
  
  ###### IMPORTANT, sort for X (id) again
  world <-  world[with(world, order(X)),]
  # world$value[is.na(world$value) == T] <- 0
  
  plot=ggplot() +
    geom_polygon(data=subset(world, country != "Antarctica"), aes(x = long, y = lat, group = group, fill = value), size = 0.2, color = "white") +
    coord_fixed() + # Important to fix world map proportions
    scale_x_continuous(limits=c(-13900000,17000000))+
    labs(x="", y="") +
    scale_fill_gradient(name=legend.name, 
                        na.value="#dadada",
                        low = color.low, 
                        high = color.high, 
                        breaks=c(min(world$value[is.na(world$value)==F]),seq(0,1500,200),max(world$value[is.na(world$value)==F])),
                        # labels=c("0","250","500","750"),
                        guide=guide_colorbar(barwidth=15, label.hjust = 0.5, title.position = "top"))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_blank(),
          legend.position = c(0.55,0),
          legend.justification = c(0.5,0.3),
          legend.direction = "horizontal",
          plot.title = element_text(family = "", colour = "#333333", size = 11, hjust = 0.5, margin = margin(b=10)),
          legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10,b=5)),
          legend.text = element_text(family="", colour = "#333333", size = 11*0.8, angle = 0, hjust=0, vjust=0, margin = margin(r=10)),
          legend.text.align = 0,
          plot.background = element_rect(fill="#FFFFFF"),
          plot.margin = unit(c(0.0,0.0,0.05,0.0), "npc"),
          
    ) 
  
  print(plot)
  
  gta_plot_saver(plot=plot,
                 path = paste0(figure.path),
                 name= paste0("Number of hits in populist era - ",type.eval),
                 pdf = T,
                 cairo_ps = T,
                 png=T)
}


# The second map would show the share of a country's 
# exports that are affected by harmful foreign measures 
# implemented from 1 Jan 2017 to 15 Nov 2019. 

load(paste0(data.path,"Exports affected in populist era.Rdata"))
write.xlsx(ex.coverages[,c("affected.jurisdiction","coverage","type")], file=paste0(figure.path,"Exports affected in populist era.xlsx"),sheetName="Nr of hits",row.names=F)

map.coverage <- function(type.eval=NULL, color.low=NULL, color.high=NULL, legend.name=NULL, breaks.manual = NULL) {

  world <- gtalibrary::world.geo
  data <- subset(ex.coverages, type == type.eval)
  data[,c("UN","value")] <- data[,c("un","coverage")]
  data$UN <- gta_un_code_vector(data$UN)
  
  # merge data with map data
  world = merge(world, data[,c("UN","value")], by="UN", all.x=T)
  
  ###### IMPORTANT, sort for X (id) again
  world <-  world[with(world, order(X)),]
  # world$value[is.na(world$value) == T] <- 0

  if (is.null(breaks.manual)){
    breaks <- c(min(world$value[is.na(world$value)==F]),seq(0,1,0.25),max(world$value[is.na(world$value)==F]))
  }else {
    eval(parse(text=paste0("breaks <- ",breaks.manual)))
  }
    
  plot=ggplot() +
    geom_polygon(data=subset(world, country != "Antarctica"), aes(x = long, y = lat, group = group, fill = value), size = 0.2, color = "white") +
    coord_fixed() + # Important to fix world map proportions
    scale_x_continuous(limits=c(-13900000,17000000))+
    labs(x="", y="") +
    scale_fill_gradient(name=legend.name, 
                        na.value="#dadada",
                        low = color.low, 
                        high = color.high, 
                        breaks=breaks,
                        labels=scales::percent,
                        guide=guide_colorbar(barwidth=15, label.hjust = 0.5, title.position = "top"))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_blank(),
          legend.position = c(0.55,0),
          legend.justification = c(0.5,0.3),
          legend.direction = "horizontal",
          plot.title = element_text(family = "", colour = "#333333", size = 11, hjust = 0.5, margin = margin(b=10)),
          legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10,b=5)),
          legend.text = element_text(family="", colour = "#333333", size = 11*0.8, angle = 0, hjust=0, vjust=0, margin = margin(r=10)),
          legend.text.align = 0,
          plot.background = element_rect(fill="#FFFFFF"),
          plot.margin = unit(c(0.0,0.0,0.05,0.0), "npc"),
          
    )
  
  print(plot)
  
  gta_plot_saver(plot=plot,
                 path = paste0(figure.path),
                 name= paste0("Exporting countries affected by populist era interventions - ",type.eval),
                 cairo_ps = T,
                 pdf=T,
                 png=T)
  

}


map.nr.hits(type.eval="harmful",
            legend.name = "Number of hits to a nation’s commercial interests due \nto protectionism implemented from 1 January 2017 \nto 15 November 2019 (the Populist era)",
            color.low = gta_colour$red[4],
            color.high = gta_colour$red[1])

map.nr.hits(type.eval="liberalising",
            legend.name = "Number of times a nation’s commercial interests benefitted \nfrom trade reforms implemented from 1 January 2017 \nto 15 November 2019 (the Populist era)",
            color.low = gta_colour$green[4],
            color.high = gta_colour$green[1])

map.coverage(type.eval="harmful",
            legend.name = "Share of national goods exports exposed to \nprotectionism implemented from 1 January 2017 \nto 15 November 2019 (the Populist era)",
            color.low = gta_colour$red[4],
            color.high = gta_colour$red[1])

map.coverage(type.eval="liberalising",
             legend.name = "Share of national goods exports benefitting from \ntrade reforms implemented from 1 January 2017 \nto 15 November 2019 (the Populist era)",
             color.low = gta_colour$green[4],
             color.high = gta_colour$green[1],
             breaks.manual = "c(min(world$value[is.na(world$value)==F]),seq(0,0.6,0.2),max(world$value[is.na(world$value)==F]))")
