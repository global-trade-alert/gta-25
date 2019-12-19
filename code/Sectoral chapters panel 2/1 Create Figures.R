rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(plyr)
library(data.table)
extrafont::loadfonts(device="win")
library(ggplot2)
library(openxlsx)
library(gridExtra)
library(stringr)

################ NO NEED TO RUN THIS CODE
################ NO NEED TO RUN THIS CODE
################ NO NEED TO RUN THIS CODE
################ NO NEED TO RUN THIS CODE
# 191216 Removed from final version








gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
chapter.folders=gta25_setup(internal.name = 'Sectoral chapters panel 2',
                            in.dev = F,
                            author='ks',
                            wipe.data = F,
                            wipe.figs = T)
data.path=chapter.folders$data.path
figure.path=chapter.folders$figure.path

gta_colour_palette()
symbol.size=2

## loading data
load(paste0(data.path,'fig 5.Rdata'))
load(paste0(data.path,'fig 6.Rdata'))
load(paste0(data.path,'fig 7.Rdata'))
load(paste0(data.path,'fig 8.Rdata'))

## setting data limits for OLS
outlier.fig5.ceiling=.7
outlier.fig6.ceiling=2
outlier.fig7.ceiling=.6
outlier.fig8.floor=-0.06


## setting figure paths
first.sector.chapter=8
sector.path=paste0(str_extract(figure.path,"^.+?figures/"),paste0(first.sector.chapter:(first.sector.chapter+length(sectors)-1), " - Sector ", sectors,"/"))
wipe.sector.path=F




# Chart 5 -----------------------------------------------------------------

#Request: Scattter plot for all G20. Y-axis shows change in sectoral import share protected from 2017-2019. X-axis shows national import share from 2016



fig5.create=function(sct){ 
  
  #cheap way to remove outliers
  fig5.lm <<- lm(cov.change ~ sct.share, subset(data.fig5, cpc==sct & cov.change<outlier.fig5.ceiling))
  
  stat.values <<- rbind(stat.values, data.frame(r.squared = summary(fig5.lm)$r.squared,
                                                t.value = summary(fig5.lm)$coefficients[2,3],
                                                p.value = summary(fig5.lm)$coefficients[2,4],
                                                formula = "cov.change ~ incentives.change",
                                                plot=paste0("Graph 5 - Sector ",sct)))
  
  fig5 <- ggplot(data=subset(data.fig5, cpc==sct)) + geom_point(aes(x=sct.share, y=cov.change, size=symbol.size))+guides(size="none") +
    gta_theme() + xlab('National import share in 2016') + ylab('Sectoral imports share protected during Populist era') + 
    geom_label(aes(x=Inf, y=Inf, label=paste0("R-Squared: ",round(summary(fig5.lm)$r.squared, 3))), hjust=1.1, vjust=1.5)
  
 
  return(fig5)
  
}

# Chart 6 -----------------------------------------------------------------

#Chart 6: Scatter plot for all G20. Y-axis shows change in sectoral import share protected from 2017-2019. 
#X-axis shows the change in the exchange rate of the national currency against USD from 2017-2019

fig6.create=function(sct){  
  #cheap way to remove outliers
  fig6.lm <<- lm(cov.change ~ curr.rel.change, subset(data.fig6, sector==sct & curr.rel.change<outlier.fig6.ceiling))
  
  stat.values <<- rbind(stat.values, data.frame(r.squared = summary(fig6.lm)$r.squared,
                                                t.value = summary(fig6.lm)$coefficients[2,3],
                                                p.value = summary(fig6.lm)$coefficients[2,4],
                                                formula = "cov.change ~ incentives.change",
                                                plot=paste0("Graph 6 - Sector ",sct)))
  
  fig6 <- ggplot(data=subset(data.fig6, sector==sct)) + geom_point(aes(x=curr.rel.change, y=cov.change, size=symbol.size))+guides(size="none") +
    gta_theme() + xlab('Relative currency change (ratio of average in 2019 to average in 2016)') + ylab('Sectoral imports share protected during Populist era') + 
    geom_label(aes(x=Inf, y=Inf, label=paste0("R-Squared: ",round(summary(fig6.lm)$r.squared, 3))), hjust=1.1, vjust=1.5)+
    scale_x_continuous(limits = c(.95,1.2))
 
  return(fig6)
  
}


# Chart 7 -----------------------------------------------------------------

#Chart 7: Scatter plot for G20. Y-axis shows share of national sectoral import share affected by non-tariff measures.
#the X axis is the sectoral trade balance divided by total sectoral trade. Let X be the total exports of a country in a given sector, 
#let Y be total imports of the same country in the same sector. Then the measure I have in mind is (X-Y)/(X+Y). Note that this measure can be negative but the values will always lie between -1 and +1.


fig7.create=function(sct){  
  #cheap way to remove outliers
  fig7.lm <<- lm(cov.change ~ sect.trade.share, subset(data.fig7, sector==sct & cov.change<outlier.fig7.ceiling))
  
  stat.values <<- rbind(stat.values, data.frame(r.squared = summary(fig7.lm)$r.squared,
                                                t.value = summary(fig7.lm)$coefficients[2,3],
                                                p.value = summary(fig7.lm)$coefficients[2,4],
                                                formula = "cov.change ~ incentives.change",
                                                plot=paste0("Graph 7 - Sector ",sct)))
  
  fig7 <- ggplot(data=subset(data.fig7, sector==sct)) + geom_point(aes(x=sect.trade.share, y=cov.change, size=symbol.size))+guides(size="none") +
    gta_theme() + xlab('Sectoral trade balance divided by total sectoral trade in 2016') + ylab('Sectoral imports share protected during Populist era') + 
    geom_label(aes(x=Inf, y=Inf, label=paste0("R-Squared: ",round(summary(fig7.lm)$r.squared, 3))), hjust=1.1, vjust=1.5)
  
  return(fig7)
  
  
}

# Chart 8 -----------------------------------------------------------------

#Chart 8: Scatter plot for G20. Y-axis same as graph 5 and 6. X-axis shows the share of sectoral exports that benefit from incentives.

fig8.create=function(sct){  
  
  fig8.lm <<- lm(cov.change ~ incentives.change, subset(data.fig8, sector==sct & incentives.change>outlier.fig8.floor))
  
  stat.values <<- rbind(stat.values, data.frame(r.squared = summary(fig8.lm)$r.squared,
                                                t.value = summary(fig8.lm)$coefficients[2,3],
                                                p.value = summary(fig8.lm)$coefficients[2,4],
                                                formula = "cov.change ~ incentives.change",
                                                plot=paste0("Graph 8 - Sector ",sct)))
  
  fig8 <- ggplot(data=subset(data.fig8, sector==sct)) + geom_point(aes(x=incentives.change, y=cov.change, size=symbol.size))+guides(size="none") +
    gta_theme() + xlab('Share of sectoral exports that benefit from incentives in 2016') + ylab('Sectoral imports share protected during Populist era') + 
    geom_label(aes(x=Inf, y=Inf, label=paste0("R-Squared: ",round(summary(fig8.lm)$r.squared, 3))), hjust=1.1, vjust=1.5)+
    scale_x_continuous(limits = c(0,min(1, max(subset(data.fig8, sector==sct & incentives.change>outlier.fig8.floor)$incentives.change)+.05)))
  
  
  return(fig8)
  
}

# Create panels per sector ------------------------------------------------
stat.values=data.frame()

for (sct in sectors){
  
  s.path=sector.path[grepl(paste0(sct,"/$"),sector.path)]
  
  dir.create(file.path(s.path), showWarnings = FALSE)
  
  if(wipe.sector.path){
    wipe.all= list.files(s.path, include.dirs = F, full.names = T, recursive = T)
    file.remove(wipe.all)
    rm(wipe.all)
  }
  
  
  fig5 <- fig5.create(sct)
  fig6 <- fig6.create(sct)
  figA <- grid.arrange(fig5, fig6, nrow=2)
  gta_plot_saver(plot = figA,
                 path = s.path,
                 name = paste0("Panel 2 A (Fig 5 & 6) - Sector ",sct),
                 cairo_ps = T,
                 pdf = T,
                 height = 29.7,
                 width = 21)
  
  
  fig7 <- fig7.create(sct)
  fig8 <- fig8.create(sct)
  figB <- grid.arrange(fig7, fig8, nrow=2)
  gta_plot_saver(plot = figB,
                 path = s.path,
                 name = paste0("Panel 2 B (Fig 7 & 8) - Sector ",sct),
                 cairo_ps = T,
                 pdf = T,
                 height = 29.7,
                 width = 21)
  
  write.xlsx(subset(data.fig5, cpc==sct),
             file=paste0(s.path,'Fig 5 data.xlsx'))
  
  write.xlsx(subset(data.fig6, sector==sct),
             file=paste0(s.path,'Fig 6 data.xlsx'))
  
  
  write.xlsx(subset(data.fig7, sector==sct),
             file=paste0(s.path,'Fig 7 data.xlsx'))
  
  write.xlsx(subset(data.fig8, sector=sct),
             file=paste0(s.path,'Fig 8 data.xlsx'))
  
  openxlsx::write.xlsx(subset(stat.values, grepl(paste0(sct,"$"),plot)), file=paste0(s.path,"Statistical results for figures 5-8.xlsx"), rowNames=F)
}

