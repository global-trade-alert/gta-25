rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(xlsx)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
source('0 report production/GTA 25/help files/GTA 25 cutoff and definitions.R')

this.chapter="USA, China and EU targeting G20"

wdpath = "0 dev/gta-25-pb/"
output.path = paste0(wdpath,"tables & figures/USA, China and EU targeting G20/")
data.path = paste0(wdpath,"code/USA, China and EU targeting G20/data/")

gta_colour_palette()

dest.markets <- list(c(156),c(840),c(eu.members))
dest.markets.names <- c("China","USA", "EU")
dest.markets.names.2 <- c("Chinese","US","EU")

# Figure 1 create graph ------------------------------------------------------

# Graph 1: A scatter plot for all G20 (except destination market). Y-axis shows the
# % bilateral exports to destination market (China, USA, or EU) affected by 
# harmful measures implemented by destination market since 2017-01-01. 
# X-axis shows the amount of exports to the destination market divided 
# by the GDP of the exporter (share of GDP being exported to the destination market).

load(paste0(data.path,"Populist era g20 targeting.Rdata"))

fig1.data <- coverages
openxlsx::write.xlsx(fig1.data, file=paste0(output.path,"Table for Figure 1.xlsx"), rowNames=F)
fig1.data$importer <- as.character(fig1.data$importer)
fig1.data$exporter <- as.character(fig1.data$exporter)
fig1.data$importer[fig1.data$importer == "United States of America"] <- "USA"
fig1.data$exporter[fig1.data$exporter == "United States of America"] <- "USA"

# FOR LDCs AND AFRICAN UNION

fig1.create <- function(dst) {
  
  y.name = paste0("Share of export to ",dest.markets.names[dst],"\naffected by harmful measures")
  x.name = paste0("Share of GDP exported to ",dest.markets.names[dst])
    
    fig1 <- ggplot(data=subset(fig1.data, importer == dest.markets.names[dst]))+
      geom_text(aes(x=gdp.share, y=harmful, label=exporter), nudge_x = 0.002, hjust = 0, vjust=0.5, color = gta_colour$grey[1], size=4)+
      geom_point(aes(x=gdp.share, y=harmful), color = gta_colour$blue[1], size=2)+
      scale_y_continuous(name=y.name,
                         sec.axis = sec_axis(trans = ~., name=y.name))+
      scale_x_continuous(name=x.name)+
      coord_cartesian(clip="off")+
      gta_theme()
      
  return(fig1)
  }

# Figure 2 create graph ------------------------------------------------------

# Graph 2: Same as above, except Y-axis shows the share of bilateral exports to destination 
# market affected by harmful measures implemented by destination market since 2017-01-01 MINUS 
# the share of exports to destination market profiting from liberalising measures implemented 
# by destination market since 2017-01-01.


fig2.create <- function(dst) {
  
  y.name = paste0("Difference in share of exports to ",dest.markets.names[dst],"\naffected by harmful measures and liberalising measures")
  x.name = paste0("Share of GDP exported to ",dest.markets.names[dst])
  
  fig2 <- ggplot(data=subset(fig1.data, importer == dest.markets.names[dst]))+
    geom_text(aes(x=gdp.share, y=coverage.difference, label=exporter), nudge_x = 0.002, hjust = 0, vjust=0.5, color = gta_colour$grey[1], size=4)+
    geom_point(aes(x=gdp.share, y=coverage.difference), color = gta_colour$blue[1], size=2)+
    scale_y_continuous(name=y.name, labels = percent,
                       sec.axis = sec_axis(trans = ~., name=y.name, labels = percent))+
    scale_x_continuous(name=x.name, labels = percent)+
    coord_cartesian(clip="off")+
    gta_theme()
  
  fig2
  return(fig2)
}


# Create panels per sector ------------------------------------------------

for (dst in 1:length(dest.markets.names)) {
  
  fig1 <- fig1.create(dst)
  fig2 <- fig2.create(dst)
  
  figA <- grid.arrange(fig1, fig2, nrow=2)

  gta_plot_saver(plot = figA,
                 path = paste0(output.path),
                 name = paste0("Figure 1 - Destination market ",dest.markets.names[dst]),
                 cairo_ps = T,
                 height = 29.7,
                 width = 21)

}
  
