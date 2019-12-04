rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(xlsx)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')

directories=gta25_setup(internal.name="Single & multi-country hits",
                        in.dev=F,
                        author=NULL,
                        wipe.data=F,
                        wipe.figs=T)

data.path = directories$data.path
output.path = directories$figure.path

gta_colour_palette()

# Figure 1 create graph ------------------------------------------------------


# The 3 plot shows the share of world trade facing harmful interventions 
# implemented (1) from 2017-2019, (2) from 2014-2016 and (3) from NA-to end of 2016.
# 
# (1) World trade affected by tariff interventions affecting a single nation in the home market
# (2) World trade affected by tariff interventions affecting mulitple nations in the home market
# (3) World trade affected by all other interventions affecting single nation in the home market
# (4) World trade affected by all other interventions affecting multiple nations in the home market
# (5) World trade affected by export incentives affecting single nation in a foreign market
# (6) World trade affected by export incentives affecting multiple nations in a foreign market

load(paste0(data.path,"Single nation coverages.Rdata"))
load(paste0(data.path,"Multiple nation coverages.Rdata"))
fig1.data <- rbind(single.nation.cov, multiple.nation.cov)
fig1.data$mast.chapter <- as.character(fig1.data$mast.chapter)
fig1.data$nations.affected <- as.character(fig1.data$nations.affected)

# LIST OF PERIODS TO BE ITERATED
periods <- list(c("2017-01-01",cutoff),c("2014-01-01",break.date),c("2009-01-01",break.date))


write.xlsx(fig1.data, file=paste0(output.path,"Table for Figure 1.xlsx"),row.names=F, sheetName = "Coverages")

fig1.create <- function(prd) {
    
    coordinates <- data.frame()
    coordinates <- as.data.frame(rbind(c(0,5,20,15,"Market \naffected"),
                                       c(0,5,15,5, "Home"),
                                       c(0,5,5,0, "Foreign"),
                                       c(5,10,20,15, "Instrument"),
                                       c(5,10,15,10, "Tariffs"),
                                       c(5,10,10,5, "All other"),
                                       c(5,10,5,0, "Export \nincentives"),
                                       c(10,15,20,15, "Interventions affecting \na single jurisdiction"),
                                       c(10,15,15,10, round(subset(fig1.data, mast.chapter == "TARIFF" & period == prd & nations.affected == "one")$coverages,3)),
                                       c(10,15,10,5, round(subset(fig1.data, mast.chapter == "NOT.TARIFF" & period == prd & nations.affected == "one")$coverages,3)),
                                       c(10,15,5,0, round(subset(fig1.data, mast.chapter == "export incentives" & period == prd & nations.affected == "one")$coverages,3)),
                                       c(15,20,20,15, "Interventions affecting \nmultiple jurisdictions"),
                                       c(15,20,15,10, round(subset(fig1.data, mast.chapter == "TARIFF" & period == prd & nations.affected == "multiple")$coverages,3)),
                                       c(15,20,10,5, round(subset(fig1.data, mast.chapter == "NOT.TARIFF" & period == prd & nations.affected == "multiple")$coverages,3)),
                                       c(15,20,5,0, round(subset(fig1.data, mast.chapter == "export incentives" & period == prd & nations.affected == "multiple")$coverages,3))))
    
    
    coordinates$V1 <- as.numeric(as.character(coordinates$V1))
    coordinates$V2 <- as.numeric(as.character(coordinates$V2))
    coordinates$V3 <- as.numeric(as.character(coordinates$V3))
    coordinates$V4 <- as.numeric(as.character(coordinates$V4))
    
    fig1 <- ggplot()+
    geom_rect(data=data.frame(), aes(xmin=coordinates[1,1], xmax=coordinates[1,2], ymin=coordinates[1,3], ymax=coordinates[1,4]), fill="transparent", alpha=1, color = gta_colour$grey[1])+
    geom_rect(data=data.frame(), aes(xmin=coordinates[2,1], xmax=coordinates[2,2], ymin=coordinates[2,3], ymax=coordinates[2,4]), fill="transparent", alpha=1, color = gta_colour$grey[1])+
    geom_rect(data=data.frame(), aes(xmin=coordinates[3,1], xmax=coordinates[3,2], ymin=coordinates[3,3], ymax=coordinates[3,4]), fill="transparent", alpha=1, color = gta_colour$grey[1])+
    geom_rect(data=data.frame(), aes(xmin=coordinates[4,1], xmax=coordinates[4,2], ymin=coordinates[4,3], ymax=coordinates[4,4]), fill="transparent", alpha=1, color = gta_colour$grey[1])+
    geom_rect(data=data.frame(), aes(xmin=coordinates[5,1], xmax=coordinates[5,2], ymin=coordinates[5,3], ymax=coordinates[5,4]), fill="transparent", alpha=1, color = gta_colour$grey[1])+
    geom_rect(data=data.frame(), aes(xmin=coordinates[6,1], xmax=coordinates[6,2], ymin=coordinates[6,3], ymax=coordinates[6,4]), fill="transparent", alpha=1, color = gta_colour$grey[1])+
    geom_rect(data=data.frame(), aes(xmin=coordinates[7,1], xmax=coordinates[7,2], ymin=coordinates[7,3], ymax=coordinates[7,4]), fill="transparent", alpha=1, color = gta_colour$grey[1])+
    geom_rect(data=data.frame(), aes(xmin=coordinates[8,1], xmax=coordinates[8,2], ymin=coordinates[8,3], ymax=coordinates[8,4]), fill="transparent", alpha=1, color = gta_colour$grey[1])+
    geom_rect(data=data.frame(), aes(xmin=coordinates[9,1], xmax=coordinates[9,2], ymin=coordinates[9,3], ymax=coordinates[9,4]), fill=gta_colour$red[1], alpha=as.numeric(as.character(coordinates[9,5])), color = gta_colour$grey[1])+
    geom_rect(data=data.frame(), aes(xmin=coordinates[10,1], xmax=coordinates[10,2], ymin=coordinates[10,3], ymax=coordinates[10,4]), fill=gta_colour$red[1], alpha=as.numeric(as.character(coordinates[10,5])), color = gta_colour$grey[1])+
    geom_rect(data=data.frame(), aes(xmin=coordinates[11,1], xmax=coordinates[11,2], ymin=coordinates[11,3], ymax=coordinates[11,4]), fill=gta_colour$red[1], alpha=as.numeric(as.character(coordinates[11,5])), color = gta_colour$grey[1])+
    geom_rect(data=data.frame(), aes(xmin=coordinates[12,1], xmax=coordinates[12,2], ymin=coordinates[12,3], ymax=coordinates[12,4]), fill="transparent", alpha=1, color = gta_colour$grey[1])+
    geom_rect(data=data.frame(), aes(xmin=coordinates[13,1], xmax=coordinates[13,2], ymin=coordinates[13,3], ymax=coordinates[13,4]), fill=gta_colour$red[1], alpha=as.numeric(as.character(coordinates[13,5])), color = gta_colour$grey[1])+
    geom_rect(data=data.frame(), aes(xmin=coordinates[14,1], xmax=coordinates[14,2], ymin=coordinates[14,3], ymax=coordinates[14,4]), fill=gta_colour$red[1], alpha=as.numeric(as.character(coordinates[14,5])), color = gta_colour$grey[1])+
    geom_rect(data=data.frame(), aes(xmin=coordinates[15,1], xmax=coordinates[15,2], ymin=coordinates[15,3], ymax=coordinates[15,4]), fill=gta_colour$red[1], alpha=as.numeric(as.character(coordinates[15,5])), color = gta_colour$grey[1])+
    geom_text(data=data.frame(), aes(x=coordinates[1,2]-((coordinates[1,2]-coordinates[1,1])/2), y=coordinates[1,4]-((coordinates[1,4]-coordinates[1,3])/2), label = coordinates[1,5]), alpha=1, color = gta_colour$grey[1])+
    geom_text(data=data.frame(), aes(x=coordinates[2,2]-((coordinates[2,2]-coordinates[2,1])/2), y=coordinates[2,4]-((coordinates[2,4]-coordinates[2,3])/2), label = coordinates[2,5]), alpha=1, color = gta_colour$grey[1])+
    geom_text(data=data.frame(), aes(x=coordinates[3,2]-((coordinates[3,2]-coordinates[3,1])/2), y=coordinates[3,4]-((coordinates[3,4]-coordinates[3,3])/2), label = coordinates[3,5]), alpha=1, color = gta_colour$grey[1])+
    geom_text(data=data.frame(), aes(x=coordinates[4,2]-((coordinates[4,2]-coordinates[4,1])/2), y=coordinates[4,4]-((coordinates[4,4]-coordinates[4,3])/2), label = coordinates[4,5]), alpha=1, color = gta_colour$grey[1])+
    geom_text(data=data.frame(), aes(x=coordinates[5,2]-((coordinates[5,2]-coordinates[5,1])/2), y=coordinates[5,4]-((coordinates[5,4]-coordinates[5,3])/2), label = coordinates[5,5]), alpha=1, color = gta_colour$grey[1])+
    geom_text(data=data.frame(), aes(x=coordinates[6,2]-((coordinates[6,2]-coordinates[6,1])/2), y=coordinates[6,4]-((coordinates[6,4]-coordinates[6,3])/2), label = coordinates[6,5]), alpha=1, color = gta_colour$grey[1])+
    geom_text(data=data.frame(), aes(x=coordinates[7,2]-((coordinates[7,2]-coordinates[7,1])/2), y=coordinates[7,4]-((coordinates[7,4]-coordinates[7,3])/2), label = coordinates[7,5]), alpha=1, color = gta_colour$grey[1])+
    geom_text(data=data.frame(), aes(x=coordinates[8,2]-((coordinates[8,2]-coordinates[8,1])/2), y=coordinates[8,4]-((coordinates[8,4]-coordinates[8,3])/2), label = coordinates[8,5]), alpha=1, color = gta_colour$grey[1])+
    geom_text(data=data.frame(), aes(x=coordinates[9,2]-((coordinates[9,2]-coordinates[9,1])/2), y=coordinates[9,4]-((coordinates[9,4]-coordinates[9,3])/2), label = coordinates[9,5]), alpha=1, color = gta_colour$red[1])+
    geom_text(data=data.frame(), aes(x=coordinates[10,2]-((coordinates[10,2]-coordinates[10,1])/2), y=coordinates[10,4]-((coordinates[10,4]-coordinates[10,3])/2), label = coordinates[10,5]), alpha=1, color = gta_colour$red[1])+
    geom_text(data=data.frame(), aes(x=coordinates[11,2]-((coordinates[11,2]-coordinates[11,1])/2), y=coordinates[11,4]-((coordinates[11,4]-coordinates[11,3])/2), label = coordinates[11,5]), alpha=1, color = gta_colour$red[1])+
    geom_text(data=data.frame(), aes(x=coordinates[12,2]-((coordinates[12,2]-coordinates[12,1])/2), y=coordinates[12,4]-((coordinates[12,4]-coordinates[12,3])/2), label = coordinates[12,5]), alpha=1, color = gta_colour$grey[1])+
    geom_text(data=data.frame(), aes(x=coordinates[13,2]-((coordinates[13,2]-coordinates[13,1])/2), y=coordinates[13,4]-((coordinates[13,4]-coordinates[13,3])/2), label = coordinates[13,5]), alpha=1, color = gta_colour$red[1])+
    geom_text(data=data.frame(), aes(x=coordinates[14,2]-((coordinates[14,2]-coordinates[14,1])/2), y=coordinates[14,4]-((coordinates[14,4]-coordinates[14,3])/2), label = coordinates[14,5]), alpha=1, color = gta_colour$red[1])+
    geom_text(data=data.frame(), aes(x=coordinates[15,2]-((coordinates[15,2]-coordinates[15,1])/2), y=coordinates[15,4]-((coordinates[15,4]-coordinates[15,3])/2), label = coordinates[15,5]), alpha=1, color = gta_colour$red[1])+
    labs(x=NULL,y=NULL)+
    ggtitle(paste0("Interventions from ",periods[[prd]][1]," to ",periods[[prd]][2]))+
    gta_theme()+
    theme(plot.background = element_blank(),
          panel.background = element_blank(),
          axis.text = element_text(size=0),
          axis.title = element_text(size=0),
          panel.grid.major = element_line(color="transparent"),
          panel.grid.minor = element_line(color="transparent"),
          panel.grid.minor.y = element_line(color="transparent"),
          axis.text.x.bottom = element_blank(),
          axis.text.y.left = element_blank()
          )
  return(fig1)
  }



# Create panels per sector ------------------------------------------------

for (prd in 1:length(periods)) {
  
  fig1 <- fig1.create(prd)

  gta_plot_saver(plot = fig1,
                 path = paste0(output.path),
                 name = paste0("Figure 1 - ",periods[[prd]][1]," to ",periods[[prd]][2]),
                 cairo_ps = T,
                 width = 21)
  
}

