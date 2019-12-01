rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(plyr)
library(data.table)
library(ggplot2)
library(openxlsx)

gta_setwd()
dev.path='0 dev/gta-25-ks/code/'
main.path='0 report production/GTA 25/'
this.chapter='Sectoral chapters panel 2'
source(paste0(main.path,'help files/Producer console.R'))
source(paste0(main.path,'help files/GTA 25 cutoff and definitions.R'))
path=paste0(ifelse(subset(producer.console,chapter.name==this.chapter)$is.dev==T, dev.path, main.path),this.chapter,'/')

gta_colour_palette()
# Chart 5 -----------------------------------------------------------------

#Request: Scatter plot for all G20. Y-axis shows change in sectoral import share protected from 2017-2019. X-axis shows national import share from 2016

load(paste0(path,'data/fig 5.Rdata'))

write.xlsx(lapply(unique(data.fig5$cpc),function(x) subset(data.fig5, cpc==x)),
           file=paste0(path,'tables & figures/fig 5 data.xlsx'))

fig5=function(sct){  
  fig5 <- ggplot(data=subset(data.fig5, cpc==sct)) + geom_point(aes(x=sct.share, y=cov.change)) +
    gta_theme() + xlab('National import share 2016') + ylab('Change in sectoral import share protected 2017-2019')
  
  gta_plot_saver(fig5, 
                 paste0(path,'tables & figures/'),
                 paste0('Figure 5 - ',sct))
}

if (5 %in% unlist(subset(producer.console, chapter.name == this.chapter)$output.fig)){
  
for(sct in sectors) fig5(sct)  
  
}



# Chart 6 -----------------------------------------------------------------

#Chart 6: Scatter plot for all G20. Y-axis shows change in sectoral import share protected from 2017-2019. 
#X-axis shows the change in the exchange rate of the national currency against USD from 2017-2019

load(paste0(path,'data/fig 6.Rdata'))

write.xlsx(lapply(unique(data.fig6$sector),function(x) subset(data.fig6, sector==x)),
           file=paste0(path,'tables & figures/fig 6 data.xlsx'))

fig6=function(sct){  
  fig6 <- ggplot(data=subset(data.fig6, sector==sct)) + geom_point(aes(x=curr.rel.change, y=cov.change)) +
    gta_theme() + xlab('Relative currency change (avg 2019 versus 2017)') + ylab('Change in sectoral import share protected 2017-2019')
  
  gta_plot_saver(fig6, 
                 paste0(path,'tables & figures/'),
                 paste0('Figure 6 - ',sct))
}

if (6 %in% unlist(subset(producer.console, chapter.name == this.chapter)$output.fig)){
  
  for(sct in sectors) fig6(sct)  
  
}


# Chart 7 -----------------------------------------------------------------

#Chart 7: Scatter plot for G20. Y-axis shows share of national sectoral import share affected by non-tariff measures.
#the X axis is the sectoral trade balance divided by total sectoral trade. Let X be the total exports of a country in a given sector, 
#let Y be total imports of the same country in the same sector. Then the measure I have in mind is (X-Y)/(X+Y). Note that this measure can be negative but the values will always lie between -1 and +1.

load(paste0(path,'data/fig 7.Rdata'))

write.xlsx(lapply(unique(data.fig7$sector),function(x) subset(data.fig7, sector==x)),
           file=paste0(path,'tables & figures/fig 7 data.xlsx'))

fig7=function(sct){  
  fig7 <- ggplot(data=subset(data.fig7, sector==sct)) + geom_point(aes(x=sect.trade.share, y=change.sct.imp.share)) +
    gta_theme() + xlab('Sectoral trade balance divided by total sectoral trade') + ylab('National sectoral import affected by non-tariff measures')
  
  gta_plot_saver(fig7, 
                 paste0(path,'tables & figures/'),
                 paste0('Figure 7 - ',sct))
}

if (7 %in% unlist(subset(producer.console, chapter.name == this.chapter)$output.fig)){
  
  for(sct in sectors) fig7(sct)  
  
}


# Chart 8 -----------------------------------------------------------------

#Chart 8: Scatter plot for G20. Y-axis same as graph 5 and 6. X-axis shows the share of sectoral exports that benefit from incentives.

load(paste0(path,'data/fig 8.Rdata'))

write.xlsx(lapply(unique(data.fig8$sector),function(x) subset(data.fig8, sector==x)),
           file=paste0(path,'tables & figures/fig 8 data.xlsx'))

fig8=function(sct){  
  fig8 <- ggplot(data=subset(data.fig8, sector==sct)) + geom_point(aes(x=incentives.change, y=cov.change)) +
    gta_theme() + xlab('Share of sectoral exports that benefit from incentives') + ylab('Change in sectoral import share protected 2017-2019')
  
  gta_plot_saver(fig8, 
                 paste0(path,'tables & figures/'),
                 paste0('Figure 8 - ',sct))
}

if (8 %in% unlist(subset(producer.console, chapter.name == this.chapter)$output.fig)){
  
  for(sct in sectors) fig8(sct)  
  
}
