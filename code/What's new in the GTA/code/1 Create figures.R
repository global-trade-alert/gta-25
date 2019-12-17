rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(plyr)
library(data.table)
library(ggplot2)
library(openxlsx)
library(gridExtra)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
chapter.folders=gta25_setup(internal.name = "What's new",
                            in.dev = F,
                            author=NA,
                            wipe.data = F,
                            wipe.figs = T)
data.path=chapter.folders$data.path
figure.path=chapter.folders$figure.path

gta_colour_palette()

# Figure 1 ----------------------------------------------------------------
load(paste0(data.path, 'published interventions.Rdata'))


published.ids$cols="bla"
published.ids$reporting.deadline=year(as.Date(published.ids$reporting.deadline))
published.ids$position=published.ids$intervention.count+25

fig1=ggplot(published.ids, aes(x=as.factor(reporting.deadline),y=intervention.count, fill=cols))+
  geom_bar(stat="identity")+
  geom_text(aes(x=as.factor(reporting.deadline), y=position, label=intervention.count), colour="black",size=3.5)+
  gta_theme()+
  scale_fill_manual(values=c(gta_colour$qualitative[c(1)]))+
  scale_y_continuous(sec.axis = dup_axis())+
  guides(fill="none")+
  labs(x="... and reported by September of given year", y="Number of interventions implemented\nin Q1 to Q3 2009 ...")


gta_plot_saver(fig1, 
               figure.path,
               'Figure 2',
               cairo_ps = T,
               pdf = T)

published.ids$colsNULL
published.ids$position=NULL
write.xlsx(published.ids, paste0(figure.path, 'Figure 2 data.xlsx'))

# Figure 2 (CURRENTLY UNDER REVISION) ----------------------------------------------------------------
# "Graph showing the total of interventions in the database for each year

load(paste0(data.path, 'state act sources.Rdata'))

sa.src.yr$cols="bla"

fig2=ggplot(sa.src.yr, aes(x=as.factor(year), y=sa.count,  fill=cols))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c(gta_colour$qualitative[c(1)]))+
  gta_theme()+
  labs(x="calendar year",y="Total number of reports on state interventions\npublished in a given calendar year")+
  scale_y_continuous(sec.axis = dup_axis())+
  guides(fill="none") 
    

gta_plot_saver(fig2, 
               figure.path,
               'Figure 1',
               cairo_ps = T,
               pdf = T)

write.xlsx(sa.src.yr, paste0(figure.path, 'Figure 1 data.xlsx'))
