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



# Figure 2 (CURRENTLY UNDER REVISION) ----------------------------------------------------------------
# "Graph showing the total of interventions in the database for each year splitted by 
# (1) Interventions having at least one official source, 
# (2) Interventions coming from company self-declaration and 
# (3) coming from other non-official sources."

load(paste0(data.path, 'state act sources.Rdata'))
if (1 %in% unlist(subset(producer.console, chapter.name == this.chapter)$output.fig)){
  
  fig2=ggplot(sa.src.yr, aes(x=as.factor(year), y=sa.count, fill=as.factor(is.source.official)))+
    geom_bar(stat="identity")+
    scale_fill_manual(values=c(gta_colour$qualitative[c(3,1)]))+
    gta_theme()+
    labs(x="reporting year",y="number of state acts\npublished")+
    scale_y_continuous(sec.axis = dup_axis())+
    guides(fill="none")
  
  gta_plot_saver(fig2, 
                 figures.path,
                 'Figure 2')
  
  write.xlsx(sa.src.yr, paste0(figures.path, 'Figure 2 data.xlsx'))
  
}



