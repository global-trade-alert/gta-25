rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(xlsx)
library(Hmisc)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')

directories=gta25_setup(internal.name="Single & multi-country hits",
                        in.dev=F,
                        author=NULL,
                        wipe.data=F,
                        wipe.figs=F)

data.path = directories$data.path
output.path = directories$figure.path

gta_colour_palette()

# Figure 1 create graph ------------------------------------------------------

### NEW Figure 1, 6.12.2019 via phone call SE<->JF
# Create a total of seven figures, all seven are timelines on the horizontal axis.
# The timeline counts the number of months since the start of the period.
# The vertical axis show the share of trade affected.
# 
# For every month, we calculate the instrument-targeting combination of the original graph except the single-affected export incentives:
# (1) World trade affected by tariff measures affecting a single nation in the home market
# (2) World trade affected by tariff measures affecting mulitple nations in the home market
# (3) World trade affected by all other measures affecting single nation in the home market
# (4) World trade affected by all other measures affecting multiple nations in the home market
# (5*) World trade affected by export incentives affecting multiple nations in a foreign market
# (6*) World trade affected by all instruments and any number of affected nations.

# The seventh chart shows the different instruments/targeting all in one plot but for the populist era only.

load(paste0(data.path,"Multiple nation coverages - period 1.Rdata"))
period1 <- single.multi.data
load(paste0(data.path,"Multiple nation coverages - period 2.Rdata"))
period2 <- single.multi.data
load(paste0(data.path,"Multiple nation coverages - period 3.Rdata"))
period3 <- single.multi.data

fig.data <- rbind(period1, period2, period3)

write.xlsx(fig.data, file=paste0(output.path,"Table for Figure 1-7.xlsx"),row.names=F, sheetName = "Coverages")

# Cosmetics
fig.data$target[fig.data$target == "multi"] <- "multiple nations"
fig.data$target[fig.data$target == "single"] <- "single nation"
fig.data$name <- paste0(capitalize(fig.data$instrument)," measures \naffecting ",fig.data$target)
fig.data$name[fig.data$instrument=="export incentive"] <- paste0(fig.data$name[fig.data$instrument=="export incentive"]," in foreign market")
fig.data$name[! fig.data$instrument=="export incentive"] <- paste0(fig.data$name[! fig.data$instrument=="export incentive"]," in home market")

types <- c("tariff","non-tariff","export incentive","all")

fig.create <- function(tp,trg) {

    set = subset(fig.data, instrument == types[tp] & target == targets[trg])
    name <- unique(set$name)
    
    fig <- ggplot(data=set)+
      geom_line(aes(x=month.count, y=trade.share, colour=as.factor(period.id)),size=1)+
      gta_plot_wrapper(data=set,
                       data.x="month.count",
                       data.y="trade.share",
                       x.bottom.name = "Month in period",
                       x.bottom.labels = c(seq(0,max(set$month.count),5),34),
                       x.bottom.breaks = c(seq(0,max(set$month.count),5),34),
                       y.left.name = paste0("World trade affected by ",tolower(name)),
                       y.left.labels = percent,
                       y.left.limits = c(0,max(set$trade.share)*1.05),
                       y.left.expand = c(0.002,0.002),
                       colour.legend.col = 3,
                       colour.legend.title = "Periods",
                       colour.labels = c("2017-2019", "2014-2016","2009-2011"))+
    gta_theme()
  fig  
  return(fig)
  }


fig7.create <- function() {
  
  set = subset(fig.data, period.id == 1)
  set$grouping <- paste0(set$instrument," measures affecting ",set$target)
  
  fig <- ggplot(data=set)+
    geom_line(aes(x=month.count, y=trade.share, colour=grouping),size=1)+
    gta_plot_wrapper(data=set,
                     data.x="month.count",
                     data.y="trade.share",
                     x.bottom.name = "Month in period",
                     x.bottom.labels = c(seq(0,max(set$month.count),5),34),
                     x.bottom.breaks = c(seq(0,max(set$month.count),5),34),
                     y.left.name = paste0("World trade affected by from \nJanuary 1st 2017 to November 15th 2019"),
                     y.left.labels = percent,
                     y.left.limits = c(0,max(set$trade.share)*1.05),
                     colour.legend.col = 2,
                     colour.legend.title = "Measure type and target")+
    gta_theme()
  fig
  return(fig)
}

# Create panels per sector ------------------------------------------------

# save plot 1-6
counter = 1
for (tp in 1:length(types)) {
  
  targets <- unique(subset(fig.data, instrument == types[tp])$target)
  
  for (trg in 1:length(targets)) {
    fig <- fig.create(tp,trg)
    fig
  
    gta_plot_saver(plot = fig,
                   path = paste0(output.path),
                   name = paste0("Figure ",counter," - ",types[tp], " measures affecting ",targets[trg]),
                   cairo_ps = T,
                   width = 21)
    
    counter = counter + 1
  
  }
}

# save plot 7
gta_plot_saver(plot = fig7.create(),
               path = paste0(output.path),
               name = paste0("Figure 7 - All measure types and targets in populist era"),
               cairo_ps = T,
               width = 21)

