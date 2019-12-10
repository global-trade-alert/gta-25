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

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
chapter.folders=gta25_setup(internal.name = 'Sectoral chapters panel 2',
                            in.dev = T,
                            author='ks',
                            wipe.data = F,
                            wipe.figs = T)
data.path=chapter.folders$data.path
figure.path=chapter.folders$figure.path

gta_colour_palette()
symbol.size=2


# Chart 5 -----------------------------------------------------------------

#Request: Scattter plot for all G20. Y-axis shows change in sectoral import share protected from 2017-2019. X-axis shows national import share from 2016

load(paste0(data.path,'fig 5.Rdata'))

write.xlsx(lapply(unique(data.fig5$cpc),function(x) subset(data.fig5, cpc==x)),
           file=paste0(figure.path,'fig 5 data.xlsx'))



fig5.create=function(sct){ 
  
  #cheap way to remove outliers
  cor5=round(cor(subset(data.fig5, cpc==sct & cov.change<0.7)$sct.share, subset(data.fig5, cpc==sct & cov.change<0.7)$cov.change),2)
  
  fig5 <- ggplot(data=subset(data.fig5, cpc==sct)) + geom_point(aes(x=sct.share, y=cov.change, size=symbol.size))+guides(size="none") +
    gta_theme() + xlab('National import share in 2016') + ylab('Change in sectoral import share protected 2017-2019') + 
    geom_text(aes(x=Inf,y=Inf),hjust=1.05,vjust=4.5,size=4,label=paste0('Correlation: ',cor5))
  # gta_plot_saver(fig5, 
  #                paste0(path,'tables & figures/'),
  #                paste0('Figure 5 - ',sct))
  
  return(fig5)
  
}

# Chart 6 -----------------------------------------------------------------

#Chart 6: Scatter plot for all G20. Y-axis shows change in sectoral import share protected from 2017-2019. 
#X-axis shows the change in the exchange rate of the national currency against USD from 2017-2019

load(paste0(data.path,'fig 6.Rdata'))


write.xlsx(lapply(unique(data.fig6$sector),function(x) subset(data.fig6, sector==x)),
           file=paste0(figure.path,'fig 6 data.xlsx'))


fig6.create=function(sct){  
  #cheap way to remove outliers
  cor6=round(cor(subset(data.fig6, sector==sct & curr.rel.change<0.7)$curr.rel.change, subset(data.fig6, sector==sct & curr.rel.change<0.7)$cov.change),2)
  
  fig6 <- ggplot(data=subset(data.fig6, sector==sct)) + geom_point(aes(x=curr.rel.change, y=cov.change, size=symbol.size))+guides(size="none") +
    gta_theme() + xlab('Relative currency change (ratio of average in 2019 to average in 2016)') + ylab('Change in sectoral import share protected 2017-2019') + 
    geom_text(aes(x=Inf,y=Inf),hjust=1.05,vjust=4.5,size=4,label=paste0('Correlation: ',cor6))
  
  # gta_plot_saver(fig6, 
  #                paste0(path,'tables & figures/'),
  #                paste0('Figure 6 - ',sct))
  
  return(fig6)
  
}


# Chart 7 -----------------------------------------------------------------

#Chart 7: Scatter plot for G20. Y-axis shows share of national sectoral import share affected by non-tariff measures.
#the X axis is the sectoral trade balance divided by total sectoral trade. Let X be the total exports of a country in a given sector, 
#let Y be total imports of the same country in the same sector. Then the measure I have in mind is (X-Y)/(X+Y). Note that this measure can be negative but the values will always lie between -1 and +1.

load(paste0(data.path,'fig 7.Rdata'))

write.xlsx(lapply(unique(data.fig7$sector),function(x) subset(data.fig7, sector==x)),
           file=paste0(figure.path,'fig 7 data.xlsx'))

fig7.create=function(sct){  
  #cheap way to remove outliers
  cor7=round(cor(subset(data.fig7, sector==sct & cov.change<0.6)$sect.trade.share, subset(data.fig7, sector==sct & cov.change<0.6)$cov.change),2)
  
  fig7 <- ggplot(data=subset(data.fig7, sector==sct)) + geom_point(aes(x=sect.trade.share, y=cov.change, size=symbol.size))+guides(size="none") +
    gta_theme() + xlab('Sectoral trade balance divided by total sectoral trade in 2016') + ylab('Change in sectoral import share protected 2017-2019') + 
    geom_text(aes(x=Inf,y=Inf),hjust=1.05,vjust=4.5,size=4,label=paste0('Correlation: ',cor7))
  
  # gta_plot_saver(fig7, 
  #                paste0(path,'tables & figures/'),
  #                paste0('Figure 7 - ',sct))
  
  return(fig7)
  
  
}

# Chart 8 -----------------------------------------------------------------

#Chart 8: Scatter plot for G20. Y-axis same as graph 5 and 6. X-axis shows the share of sectoral exports that benefit from incentives.

load(paste0(data.path,'fig 8.Rdata'))

write.xlsx(lapply(unique(data.fig8$sector),function(x) subset(data.fig8, sector==x)),
           file=paste0(figure.path,'fig 8 data.xlsx'))

fig8.create=function(sct){  
  cor8=round(cor(subset(data.fig8, sector==sct & incentives.change>-0.06)$incentives.change, subset(data.fig8, sector==sct & incentives.change>-0.06)$cov.change),2)
  
  fig8 <- ggplot(data=subset(data.fig8, sector==sct)) + geom_point(aes(x=incentives.change, y=cov.change, size=symbol.size))+guides(size="none") +
    gta_theme() + xlab('Share of sectoral exports that benefit from incentives in 2016') + ylab('Change in sectoral import share protected 2017-2019') + 
    geom_text(aes(x=Inf,y=Inf),hjust=1.05,vjust=4.5,size=4,
              label=paste0('Correlation: ',cor8))
  
  # gta_plot_saver(fig8, 
  #                paste0(path,'tables & figures/'),
  #                paste0('Figure 8 - ',sct))
  
  return(fig8)
  
}

# Create panels per sector ------------------------------------------------

for (sct in sectors) {
  
  fig5 <- fig5.create(sct)
  fig6 <- fig6.create(sct)
  figA <- grid.arrange(fig5, fig6, nrow=2)
  gta_plot_saver(plot = figA,
                 path = figure.path,
                 name = paste0("Figure Panel 2 A (5-6) - Sector ",sct),
                 cairo_ps = T,
                 height = 29.7,
                 width = 21)
  
  
  fig7 <- fig7.create(sct)
  fig8 <- fig8.create(sct)
  figB <- grid.arrange(fig7, fig8, nrow=2)
  gta_plot_saver(plot = figB,
                 path = figure.path,
                 name = paste0("Figure Panel 2 B (7-8) - Sector ",sct),
                 cairo_ps = T,
                 height = 29.7,
                 width = 21)
  
}




