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
                        wipe.figs=F)

data.path = directories$data.path
output.path = directories$figure.path

gta_colour_palette()

dest.markets <- list(c(156),c(840),c(eu.members))
dest.markets.names <- c("China","USA", "EU")
dest.markets.names.2 <- c("Chinese","US","EU")

stat.values <- data.frame()

# Figure 1 create graph ------------------------------------------------------

# Graph 1: A scatter plot for all G20 (except destination market). Y-axis shows the
# % bilateral exports to destination market (China, USA, or EU) affected by 
# harmful interventions implemented by destination market since 2017-01-01. 
# X-axis shows the amount of exports to the destination market divided 
# by the GDP of the exporter (share of GDP being exported to the destination market).

# In each of the six charts in this chapter please add the correlation coefficient
# between the variable in the vertical axis against the variable in the horizontal
# axis. Perhaps you can put the label for the R2 in the top right hand corner of each chart?

load(paste0(data.path,"Populist era g20 targeting.Rdata"))

fig1.data <- coverages
openxlsx::write.xlsx(fig1.data, file=paste0(output.path,"Table for Figure 1.xlsx"), rowNames=F)
fig1.data$importer <- as.character(fig1.data$importer)
fig1.data$exporter <- as.character(fig1.data$exporter)
fig1.data$importer[fig1.data$importer == "United States of America"] <- "USA"
fig1.data$exporter[fig1.data$exporter == "United States of America"] <- "USA"

# FOR LDCs AND AFRICAN UNION

fig1.create <- function(dst, scaling.x, scaling.y) {
  
  y.name = paste0("Percentage of exports to ",dest.markets.names[dst],"\naffected by harmful interventions")
  x.name = paste0("Share of GDP exported to ",dest.markets.names[dst])
  
  fig1.lm <- lm(gdp.share ~ harmful, subset(fig1.data, importer == dest.markets.names[dst]))
  stat.values <<- rbind(stat.values, data.frame(r.squared = summary(fig1.lm)$r.squared,
                                                t.value = summary(fig1.lm)$coefficients[2,3],
                                                p.value = summary(fig1.lm)$coefficients[2,4],
                                                formula = "gdp.share ~ harmful",
                                                plot=paste0("Graph 1 - Destination market ",dest.markets.names[dst])))
  
  fig1 <- ggplot(data=subset(fig1.data, importer == dest.markets.names[dst]))+
    geom_text(aes(x=gdp.share, y=harmful, label=exporter), nudge_x = 0.02*scaling.x, hjust = 0, vjust=0.5, color = gta_colour$grey[1], size=3)+
    geom_point(aes(x=gdp.share, y=harmful), color = gta_colour$blue[1], size=2)+
    scale_y_continuous(name=y.name, limits = c(0,scaling.y), labels = percent, breaks=seq(0,1,0.1),
                       sec.axis = sec_axis(trans = ~., name=y.name, labels = percent,breaks=seq(0,1,0.1)))+
    geom_label(aes(x=Inf, y=Inf, label=paste0("R-Squared: ",format(round(summary(fig1.lm)$r.squared, 3), nsmall = 3))), hjust=1.1, vjust=1.5)+
    scale_x_continuous(name=x.name, labels = percent, limits=c(0,scaling.x), breaks=seq(0,1,0.05))+
    coord_cartesian(clip="off")+
    coord_fixed(ratio=scaling.x/scaling.y)+
    gta_theme()
  fig1
  return(fig1)
}

# Figure 2 create graph ------------------------------------------------------


## Graph 2: 
## Simple scatter of y-axis from graph 1 to % bilateral exports to destination benefitting from liberalisations.

# Please add the country labels to the plots in the bottom panels of this chart and the other two in this chapter. 

fig2.create <- function(dst, scaling.x, scaling.y) {
  
  y.name = paste0("Percentage of exports to ",dest.markets.names[dst],"\naffected by harmful interventions")
  x.name = paste0("Percentage of exports to ",dest.markets.names[dst],"\nbenefiting from liberalising interventions")
  
  fig2.lm <- lm(liberalising ~ harmful, subset(fig1.data, importer == dest.markets.names[dst]))
  stat.values <<- rbind(stat.values, data.frame(r.squared = summary(fig2.lm)$r.squared,
                                                t.value = summary(fig2.lm)$coefficients[2,3],
                                                p.value = summary(fig2.lm)$coefficients[2,4],
                                                formula = "harmful ~ liberalising",
                                                plot=paste0("Graph 2 - Destination market ",dest.markets.names[dst])))
  
  fig2 <- ggplot(data=subset(fig1.data, importer == dest.markets.names[dst]),aes(x=liberalising, y=harmful))+
    geom_text(aes(x=liberalising, y=harmful, label=exporter), nudge_x = 0.02*scaling.x, hjust = 0, vjust=0.5, color = gta_colour$grey[1], size=3)+
    geom_point(color = gta_colour$blue[1], size=2)+
    geom_abline(intercept=0, slope=1, linetype="dashed")+
    geom_label(aes(x=Inf, y=Inf, label=paste0("R-Squared: ",format(round(summary(fig2.lm)$r.squared, 3), nsmall = 3))), hjust=1.1, vjust=1.5)+
    scale_y_continuous(name=y.name, labels = percent, breaks=seq(0,1,0.1),
                       sec.axis = sec_axis(trans = ~., name=y.name, labels = percent, breaks=seq(0,1,0.1)), limits = c(0,scaling.y))+
    scale_x_continuous(name=x.name, labels = percent, limits = c(0,scaling.x),breaks=seq(0,1,0.1))+
    coord_cartesian(clip="off")+
    coord_fixed(ratio=scaling.x/scaling.y)+
    gta_theme()
  fig2
  return(fig2)
}


# Create panels per sector ------------------------------------------------
limits.x.1 <- c(0.10,0.3,0.1)
limits.y.1 <- c(0.75,0.75,0.5)
limits.x.2 <- c(1,0.75,0.5)
limits.y.2 <- c(0.75,0.75,0.5)
for (dst in 1:length(dest.markets.names)) {
  
  fig1 <- fig1.create(dst, scaling.x=limits.x.1[dst], scaling.y=limits.y.1[dst])
  fig2 <- fig2.create(dst, scaling.x=limits.x.2[dst], scaling.y=limits.y.2[dst])
  
  figA <- grid.arrange(fig1, fig2, nrow=2)
  figA
  
  gta_plot_saver(plot = figA,
                 path = paste0(output.path),
                 name = paste0("Figure 1 - Destination market ",dest.markets.names[dst]),
                 cairo_ps = T,
                 pdf = T,
                 height = 29.7,
                 width = 21)
}

openxlsx::write.xlsx(stat.values, file=paste0(output.path,"Statistical results.xlsx"), rowNames=F)

