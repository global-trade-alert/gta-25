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
                        wipe.figs=F)

data.path = directories$data.path
output.path = directories$figure.path
help.file.path=gsub("/data/Single & multi-country hits/","/help files/",data.path)

gta_colour_palette()

# Figure 1 create graph ------------------------------------------------------

# I would be grateful if you could make "GTA charts" out of the excel charts I 
# have prepared in the attached files. These charts relate to chapter 1.

# Read xlsx 1

fig8.data <- xlsx::read.xlsx(paste0(help.file.path,"Chapter 1 falling number of reforms.xlsx"),sheetIndex = 1)
fig8.data <- fig8.data[,c(1:2)]
names(fig8.data) <- c("year","interventions")

write.xlsx(fig8.data, file=paste0(output.path,"Table for Figure 8.xlsx"),row.names=F, sheetName = "Interventions")

fig8.create <- function() {
  
  fig8 <- ggplot()+
    geom_line(data = fig8.data, aes(x=year, y=interventions), colour=gta_colour$green[1], size=1)+    
    geom_point(data = fig8.data, aes(x=year, y=interventions), colour=gta_colour$green[1], size=3)+ 
    geom_text(data = subset(fig8.data, year %in% c(2010,2011,2013,2015,2018)), aes(x=year, y=interventions, label=interventions), nudge_y = 10, vjust=0, colour=gta_colour$green[1], size=3)+    
    geom_text(data = subset(fig8.data, ! year %in% c(2010,2011,2013,2015,2018)), aes(x=year, y=interventions, label=interventions), nudge_y = -10, vjust=1, colour=gta_colour$green[1], size=3)+    
    gta_plot_wrapper(data=fig8.data,
             data.x = "year",
             data.y = "interventions",
             x.bottom.name = "Year",
             x.bottom.breaks = seq(2009,2019,1),
             x.bottom.expand = c(0.05,0.05),
             y.left.breaks = seq(0,350,100),
             y.left.limits = c(0,350),
             y.left.name = "Number of liberalising measures implemented \nfrom 1 January to 15 November of year in question")+
    gta_theme()
  
  return(fig8)
  }


# Read xlsx 2

fig9.data <- xlsx::read.xlsx(paste0(help.file.path,"Chapter 1 rising number of trade distortions.xlsx"),sheetIndex = 1)
fig9.data <- fig9.data[c(1:11),]
fig9.data$All <- as.numeric(as.character(fig9.data$All))
fig9.data$years <- as.numeric(as.character(fig9.data$years))
fig9.data$Harmful <- as.numeric(as.character(fig9.data$Harmful))
fig9.data$percentage.discriminating <- fig9.data$Harmful / fig9.data$All

write.xlsx(fig9.data, file=paste0(output.path,"Table for Figure 9.xlsx"),row.names=F, sheetName = "Interventions")

fig9.create <- function() {
  scale.var <- max(fig9.data$Harmful)
  fig9 <- ggplot()+
    geom_bar(data=fig9.data, aes(x=years, y=Harmful), stat="identity", fill=gta_colour$red[2])+
    geom_line(data = fig9.data, aes(x=years, y=percentage.discriminating*scale.var), colour=gta_colour$red[1], size=1)+    
    geom_text(data = subset(fig9.data, ! years %in% c(2000)), aes(x=years, y=Harmful, label=Harmful), nudge_y = -10, vjust=1, colour="#FFFFFF", size=3)+
    gta_plot_wrapper(data=fig9.data,
                     data.x = "years",
                     data.y = "Harmful",
                     x.bottom.name = "Year",
                     x.bottom.breaks = seq(2009,2019,1),
                     y.left.name = "Number of discriminatory commercial policy interventions \nimplemented 1 January to 15 November of year in question",
                     y.right.transform = 1/scale.var,
                     y.right.labels = percent,
                     y.right.name = "Percentage of commercial policy interventions that \ndiscrimninate against foreign commercial interests (right axis)")+
    gta_theme()
  
  return(fig9)
}


# Read xlsx 3

fig10.data <- xlsx::read.xlsx(paste0(help.file.path,"Services goods figure chapter 1.xlsx"),sheetIndex = 1)
fig10.data <- fig10.data[c(4:5),c(2:13)]
names(fig10.data) <- c("type",2009:2019)
fig10.data <- pivot_longer(fig10.data, cols = c(paste0(2009:2019)), names_to = "year", values_to = "share")
fig10.data$year <- as.numeric(fig10.data$year)

openxlsx::write.xlsx(fig10.data, file=paste0(output.path,"Table for Figure 10.xlsx"),rowNames=F)

fig10.create <- function() {
  
  fig10 <- ggplot()+
    geom_line(data=fig10.data, aes(x=year, y=share, colour=type), size=1)+
    scale_y_continuous(limits=c(0,10), breaks=seq(0,10,1), name = "Ratio of discriminatory policy interventions to liberalising \npolicy interventions implemented in a given year",
                       sec.axis = sec_axis(~.,name="Ratio of discriminatory policy interventions to liberalising \npolicy interventions implemented in a given year", breaks=seq(0,10,1)))+
    scale_x_continuous(breaks=seq(2009,2019,1), name = "Year")+
    scale_color_manual(values=gta_colour$qualitative[c(1:2)])+
    guides(colour=guide_legend(title.position = "top",title = "Type"))+
    gta_theme()
  fig10
  return(fig10)
}

# Create panels per sector ------------------------------------------------
  
  fig8 <- fig8.create()
  fig9 <- fig9.create()
  fig10 <- fig10.create()

  gta_plot_saver(plot = fig8,
                 path = paste0(output.path),
                 name = paste0("Figure 8 - Number of reforms"),
                 cairo_ps = T,
                 width = 21)
  
  gta_plot_saver(plot = fig9,
                 path = paste0(output.path),
                 name = paste0("Figure 9 - Number of discriminatory measures"),
                 cairo_ps = T,
                 width = 21)
  
  gta_plot_saver(plot = fig10,
                 path = paste0(output.path),
                 name = paste0("Figure 10 - Services and goods"),
                 cairo_ps = T,
                 width = 21)
  
