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


directories=gta25_setup(internal.name="Targeting LDCs etc",
                        in.dev=F,
                        author=NULL,
                        wipe.data=F,
                        wipe.figs=T)


data.path = directories$data.path
output.path = directories$figure.path

this.chapter="Country groups targeted by destination markets"

gta_colour_palette()

# Figure 1 create graph ------------------------------------------------------

# Graph 1: Y-axis shows the share of bilateral exports of LDCs/African Union
# facing (1) harmful interventions implemented by destination markets and (2) reforming/liberalising
# interventions implemented by destination market. The X-axis shows the destination markets. (see attachement)

load(paste0(data.path,"Destination markets targeting.Rdata"))

fig1.data <- dest.markets.cov
# fig1.data$order[fig1.data$importer=="China"] <- 1
# fig1.data$order[fig1.data$importer=="EU-28"] <- 2
# fig1.data$order[fig1.data$importer=="USA"] <- 3
# fig1.data$order[fig1.data$importer=="G7"] <- 4
# fig1.data$order[fig1.data$importer=="BRICS"] <- 5
# fig1.data$order[fig1.data$importer=="G20"] <- 6
fig1.data$importer <- as.character(fig1.data$importer)
fig1.data$exporter <- as.character(fig1.data$exporter)
fig1.data.lines <- pivot_wider(data = fig1.data, names_from = "type", values_from = "coverages")
fig1.data.lines$colour <- "red"
fig1.data.lines$colour[fig1.data.lines$liberalising > fig1.data.lines$harmful] <- "green"
fig1.data.text <- aggregate(coverages ~ importer+exporter+order, fig1.data, function(x) max(x))

write.xlsx(dest.markets.cov, file=paste0(output.path,"Table for Figure 1.xlsx"),row.names=F, sheetName = "Coverages")

# FOR LDCs AND AFRICAN UNION

fig1.create <- function() {
    
    fig1 <- ggplot()+
      geom_text(data=subset(fig1.data.text, exporter == "LDCs"), aes(x=1-0.2, y=coverages[order==1], label="LDCs"), nudge_y = -0.06, hjust = 1, vjust=0.5, angle=90,color = gta_colour$grey[1])+
      geom_text(data=subset(fig1.data.text, exporter == "African Union"), aes(x=1+0.2, y=coverages[order==1], label="African Union"), nudge_y = -0.06, hjust = 1, vjust=0.5, angle=90,color = gta_colour$grey[1])+
      geom_rect(data=subset(fig1.data.lines, exporter == "LDCs"), aes(xmin=order-0.29, xmax=order-0.11, ymin=liberalising, ymax=harmful, fill=colour), alpha=0.3)+
      geom_rect(data=subset(fig1.data.lines, exporter == "African Union"), aes(xmin=order+0.29, xmax=order+0.11, ymin=liberalising, ymax=harmful, fill=colour), alpha=0.3)+
      geom_point(data=subset(fig1.data, exporter == "African Union"), aes(x=order+0.2, y=coverages, colour = type),size=5)+
      geom_point(data=subset(fig1.data, exporter == "LDCs"), aes(x=order-0.2, y=coverages, colour = type),size=5)+
      geom_vline(xintercept = c(seq(1.5,5.5,1)),colour="#dadada")+
      gta_plot_wrapper(data = subset(fig1.data, exporter %in% c("LDCs","African Union")),
                       data.x = "order",
                       data.y = "coverages",
                       colour.labels = c("Harmful","Liberalising"),
                       colour.legend.title = "Intervention type",
                       y.left.limits = c(0,1),
                       y.left.labels = percent,
                       y.left.name = "% of trade to\ndestination\nmarket affected",
                       colour.palette = c(gta_colour$red[1], gta_colour$green[1]),
                       x.bottom.name = "Destination markets",
                       x.bottom.labels = c(unique(fig1.data$importer)),
                       x.bottom.breaks = c(seq(1,6,1)),
                       x.bottom.expand = c(0.05,0.05),
                       x.top.enable = T,
                       x.top.labels = c(unique(fig1.data$importer)),
                       colour.legend.col = 2,
                       fill.palette = c(gta_colour$green[1], gta_colour$red[1]))+
      guides(fill=FALSE, colour=guide_legend(title=NULL))+
      gta_theme(legend.position = "bottom")+
      theme(panel.grid.major.x = element_blank(),
            axis.text.x.top = element_text(face="bold", size=10),
            axis.text.x.bottom = element_blank(),
            axis.title.x.bottom = element_blank(),
            axis.title.y.left = element_text(angle = 0,hjust=1),
            axis.text.y.left = element_blank(),
            axis.title.y.right = element_blank())
    
    
    fig1
    
  return(fig1)
  }

# FOR LOWER AND UPPER MIDDLE INCOME COUNTRIES

fig2.data.text <- aggregate(coverages ~ importer+exporter+order, fig1.data, function(x) min(x))

fig2.create <- function() {
  
  fig2 <- ggplot()+
    geom_text(data=subset(fig2.data.text, exporter == "Lower middle income countries"), aes(x=1-0.2, y=coverages[order==1], label="Lower middleincome countries"), nudge_y = -0.03, hjust = 1, vjust=0.5, angle=90,color = gta_colour$grey[1])+
    geom_text(data=subset(fig2.data.text, exporter == "Upper middle income countries"), aes(x=1+0.2, y=coverages[order==1], label="Upper middle income countries"), nudge_y = -0.03, hjust = 1, vjust=0.5, angle=90,color = gta_colour$grey[1])+
    geom_rect(data=subset(fig1.data.lines, exporter == "Lower middle income countries"), aes(xmin=order-0.29, xmax=order-0.11, ymin=liberalising, ymax=harmful, fill=colour), alpha=0.3)+
    geom_rect(data=subset(fig1.data.lines, exporter == "Upper middle income countries"), aes(xmin=order+0.29, xmax=order+0.11, ymin=liberalising, ymax=harmful, fill=colour), alpha=0.3)+
    geom_point(data=subset(fig1.data, exporter == "Upper middle income countries"), aes(x=order+0.2, y=coverages, colour = type),size=5)+
    geom_point(data=subset(fig1.data, exporter == "Lower middle income countries"), aes(x=order-0.2, y=coverages, colour = type),size=5)+
    geom_vline(xintercept = c(seq(1.5,5.5,1)),colour="#dadada")+
    gta_plot_wrapper(data = subset(fig1.data, exporter %in% c("LDCs","African Union")),
                     data.x = "order",
                     data.y = "coverages",
                     colour.labels = c("Harmful","Liberalising"),
                     colour.legend.title = "Intervention type",
                     y.left.limits = c(0,1),
                     y.left.labels = percent,
                     y.left.name = "% of trade to\ndestination\nmarket affected",
                     colour.palette = c(gta_colour$red[1], gta_colour$green[1]),
                     x.bottom.name = "Destination markets",
                     x.bottom.labels = c(unique(fig1.data$importer)),
                     x.bottom.breaks = c(seq(1,6,1)),
                     x.bottom.expand = c(0.05,0.05),
                     x.top.enable = T,
                     x.top.labels = c(unique(fig1.data$importer)),
                     colour.legend.col = 2,
                     fill.palette = c(gta_colour$green[1], gta_colour$red[1]))+
    guides(fill=FALSE, colour=guide_legend(title=NULL))+
    gta_theme(legend.position = "bottom")+
    theme(panel.grid.major.x = element_blank(),
          axis.text.x.top = element_text(face="bold", size=10),
          axis.text.x.bottom = element_blank(),
          axis.title.x.bottom = element_blank(),
          axis.title.y.left = element_text(angle = 0,hjust=1),
          axis.text.y.left = element_blank(),
          axis.title.y.right = element_blank())
  
  fig2
  return(fig2)
}


# EXAMPLE PLOT WITH LINES ------------------------------------------------

figex <- ggplot()+
  geom_line(data=subset(fig1.data, exporter == "African Union"), aes(x=order, y=coverages, colour=type), alpha=0.3)+
  geom_point(data=subset(fig1.data, exporter == "African Union"), aes(x=order, y=coverages, colour = type),size=3)+
  gta_plot_wrapper(data = subset(fig1.data, exporter %in% c("LDCs","African Union")),
                   data.x = "order",
                   data.y = "coverages",
                   colour.labels = c("Harmful","Liberalising"),
                   colour.legend.title = "Intervention type",
                   y.left.limits = c(0,1),
                   colour.palette = c(gta_colour$red[1], gta_colour$green[1]),
                   x.bottom.name = "Destination markets",
                   x.bottom.labels = c(unique(fig1.data$importer)),
                   x.bottom.breaks = c(seq(1,6,1)),
                   colour.legend.col = 2,
                   fill.palette = c(gta_colour$green[1], gta_colour$red[1]))+
  gta_theme()

  # figex
  # 
  # gta_plot_saver(plot = figex,
  #                path = paste0(output.path),
  #                name = paste0("Example Figure with lines"),
  #                cairo_ps = T,
  #                width = 21)
  

# Create panels per sector ------------------------------------------------

  fig1 <- fig1.create()
  fig2 <- fig2.create()
  
  figA <- grid.arrange(fig1, fig2, nrow=2)

  gta_plot_saver(plot = fig1,
                 path = paste0(output.path),
                 name = paste0("Figure 1.1 - ",this.chapter),
                 cairo_ps = T,
                 width = 21)
  
  gta_plot_saver(plot = fig2,
                 path = paste0(output.path),
                 name = paste0("Figure 1.2 - ",this.chapter),
                 cairo_ps = T,
                 width = 21)
  
