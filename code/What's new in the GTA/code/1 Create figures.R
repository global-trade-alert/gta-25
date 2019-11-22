rm(list=ls())

library(gtalibrary)
library(stringr)
library(ggplot2)
library(openxlsx)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
chapter.path='0 dev/gta-25/code/'
this.chapter="What's new in the GTA"
data.path=paste0(chapter.path, this.chapter, '/data/')
figures.path=paste0(chapter.path, this.chapter, '/tables & figures/')
gta_colour_palette()

# Figure 1 ----------------------------------------------------------------
load(paste0(data.path, 'published interventions.Rdata'))
if (1 %in% unlist(subset(producer.console, chapter.name == this.chapter)$output.fig)){
  
  fig1 = ggplot() + geom_line(data=published.ids, aes(x=published.by, y=intervention.count, col=group), size=1.1) + 
    scale_x_date(name = 'Documented by',date_breaks = "1 year") + 
    scale_y_continuous(name= 'Total harmful interventions', breaks=seq(0,900,100), labels=seq(0,900,100)) + 
    gta_theme() + labs(color='Documented by dates') 
  
  gta_plot_saver(fig1, 
                 figures.path,
                 'Figure 1')
  
  write.xlsx(published.ids, paste0(figures.path, 'Figure 1 data.xlsx'))
  
}



