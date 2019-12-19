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


#### THIS was split into several chapters. I will thus have to change the output path several times below.


### CHAPTER "Shifting commercial policy"

directories=gta25_setup(internal.name="Single & multi-country hits - trade shares",
                        in.dev=F,
                        author=NULL,
                        wipe.data=F,
                        wipe.figs=F)

data.path = gsub(" - trade shares","",directories$data.path)
output.path = directories$figure.path

gta_colour_palette()

# Figure 1 create graph ------------------------------------------------------

### NEW Figure 1, 6.12.2019 via phone call SE<->JF
# Create a total of seven figures, all seven are timelines on the horizontal axis.
# The timeline counts the number of months since the start of the period.
# The vertical axis show the share of trade affected.
# 
# For every month, we calculate the instrument-targeting combination of the original graph except the single-affected export incentives:
# (1) World trade affected by tariff interventions affecting a single nation in the home market
# (2) World trade affected by tariff interventions affecting mulitple nations in the home market
# (3) World trade affected by all other interventions affecting single nation in the home market
# (4) World trade affected by all other interventions affecting multiple nations in the home market
# (5*) World trade affected by export incentives affecting multiple nations in a foreign market
# (6*) World trade affected by all instruments and any number of affected nations.

# The seventh chart shows the different instruments/targeting all in one plot but for the populist era only.

load(paste0(data.path,"Multiple nation coverages.Rdata"))

fig.data <- rbind(single.multi.data)

write.xlsx(fig.data, file=paste0(output.path,"Table for Figure 1-7.xlsx"),row.names=F, sheetName = "Coverages")

# Cosmetics
fig.data$target[fig.data$target == "multi"] <- "multiple exporters"
fig.data$target[fig.data$target == "single"] <- "single exporter"
fig.data$target[fig.data$target == "all"] <- "all nations"
fig.data$name <- paste0(capitalize(fig.data$instrument)," interventions \naffecting ",fig.data$target)
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
                       x.bottom.name = "Months after 1 January of interval",
                       x.bottom.labels = c(seq(0,max(set$month.count),5),34),
                       x.bottom.breaks = c(seq(0,max(set$month.count),5),34),
                       y.left.name = paste0("World trade affected by ",tolower(name)),
                       y.left.labels = percent,
                       y.left.limits = c(0,max(set$trade.share)*1.05),
                       y.left.expand = c(0.002,0.002),
                       colour.legend.col = 3,
                       colour.legend.title = "Interval",
                       colour.labels = c("2017-2019", "2014-2016","2009-2011"))+
    gta_theme()
  fig  
  return(fig)
  }


fig7.create <- function() {
  
  set = subset(fig.data, period.id == 1)
  set$grouping <- paste0(set$instrument," interventions affecting ",set$target)
  
  fig <- ggplot(data=set)+
    geom_line(aes(x=month.count, y=trade.share, colour=grouping),size=1)+
    gta_plot_wrapper(data=set,
                     data.x="month.count",
                     data.y="trade.share",
                     x.bottom.name = "Months after 1 January 2017",
                     x.bottom.labels = c(seq(0,max(set$month.count),5),34),
                     x.bottom.breaks = c(seq(0,max(set$month.count),5),34),
                     y.left.name = paste0("World trade affected from \nJanuary 2017 to November 2019"),
                     y.left.labels = percent,
                     y.left.limits = c(0,max(set$trade.share)*1.05),
                     colour.legend.col = 2,
                     colour.legend.title = "Intervention type and target")+
    gta_theme()
  fig
  return(fig)
}

# Create panels per sector ------------------------------------------------

# save plot 1-6
# counter = 1
# for (tp in 1:length(types)) {
#   
#   targets <- unique(subset(fig.data, instrument == types[tp])$target)
#   
#   for (trg in 1:length(targets)) {
#     fig <- fig.create(tp,trg)
#     fig
#   
#     gta_plot_saver(plot = fig,
#                    path = paste0(output.path),
#                    name = paste0("Figure ",counter," - ",types[tp], " interventions affecting ",targets[trg]),
#                    cairo_ps = T,
#                    width = 21)
#     
#     counter = counter + 1
#   
#   }
# }


## Figure 1
counter=1
tp=1
trg=1

targets <- unique(subset(fig.data, instrument == types[tp])$target)

fig <- fig.create(tp,trg)
     fig
   gta_plot_saver(plot = fig,
                  path = paste0(output.path),
                  name = paste0("Figure ",counter," - ",types[tp], " interventions affecting ",targets[trg]),
                  cairo_ps = T,
                  pdf = T,
                  width = 21)

# Figure 2
gta_plot_saver(plot = fig7.create(),
               path = paste0(output.path),
               name = paste0("Figure 2 - All intervention types and targets in populist era"),
               cairo_ps = T,
               pdf = T,
               width = 21)


## Figure 3
counter=3
tp=4
trg=1
targets <- unique(subset(fig.data, instrument == types[tp])$target)

fig <- fig.create(tp,trg)
fig
gta_plot_saver(plot = fig,
               path = paste0(output.path),
               name = paste0("Figure ",counter," - ",types[tp], " interventions affecting ",targets[trg]),
               cairo_ps = T,
               pdf = T,
               width = 21)

