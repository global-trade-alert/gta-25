rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(xlsx)
library(scales)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')


directories=gta25_setup(internal.name="USA, China and EU targeting G20",
                        in.dev=F,
                        author=NULL,
                        wipe.data=F,
                        wipe.figs=T)

data.path = directories$data.path
output.path = directories$figure.path

gta_colour_palette()

dest.markets <- list(c(156),c(840),c(eu.members))
dest.markets.names <- c("China","USA", "EU")
dest.markets.names.2 <- c("Chinese","US","EU")

# Figure 1 create graph ------------------------------------------------------

# Graph 1: A scatter plot for all G20 (except destination market). Y-axis shows the
# % bilateral exports to destination market (China, USA, or EU) affected by 
# harmful interventions implemented by destination market since 2017-01-01. 
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
  
  y.name = paste0("Percentage of exports to ",dest.markets.names[dst],"\naffected by harmful interventions")
  x.name = paste0("Share of GDP exported to ",dest.markets.names[dst])
    
    fig1 <- ggplot(data=subset(fig1.data, importer == dest.markets.names[dst]))+
      geom_text(aes(x=gdp.share, y=harmful, label=exporter), nudge_x = 0.02, hjust = 0, vjust=0.5, color = gta_colour$grey[1], size=3)+
      geom_point(aes(x=gdp.share, y=harmful), color = gta_colour$blue[1], size=2)+
      scale_y_continuous(name=y.name, limits = c(0,1), labels = percent,
                         sec.axis = sec_axis(trans = ~., name=y.name, labels = percent))+
      scale_x_continuous(name=x.name, labels = percent, limits=c(0,1))+
      coord_cartesian(clip="off")+
      coord_fixed(ratio=1)+
      gta_theme()
      fig1
  return(fig1)
  }

# Figure 2 create graph ------------------------------------------------------


## Graph 2: 
## Simple scatter of y-axis from graph 1 to % bilateral exports to destination benefitting from liberalisations.

fig2.create <- function(dst) {
  
  y.name = paste0("Percentage of exports to ",dest.markets.names[dst],"\naffected by harmful interventions")
  x.name = paste0("Percentage of exports to ",dest.markets.names[dst],"\nbenefitting from liberalising interventions")
  
  fig2 <- ggplot(data=subset(fig1.data, importer == dest.markets.names[dst]),aes(x=liberalising, y=harmful))+
    geom_point(color = gta_colour$blue[1], size=2)+
    geom_abline(intercept=0, slope=1, linetype="dashed")+
    scale_y_continuous(name=y.name, labels = percent,
                       sec.axis = sec_axis(trans = ~., name=y.name, labels = percent), limits = c(0,1))+
    scale_x_continuous(name=x.name, labels = percent, limits = c(0,1))+
    coord_cartesian(clip="off")+
    coord_fixed(ratio=1)+
    gta_theme()
  

  fig2
  return(fig2)
}


# Create panels per sector ------------------------------------------------

for (dst in 1:length(dest.markets.names)) {
  
  fig1 <- fig1.create(dst)
  fig2 <- fig2.create(dst)
  
  figA <- grid.arrange(fig1, fig2, nrow=2)
  figA

  gta_plot_saver(plot = figA,
                 path = paste0(output.path),
                 name = paste0("Figure 1 - Destination market ",dest.markets.names[dst]),
                 cairo_ps = T,
                 height = 29.7,
                 width = 21)
}
  

