rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(plyr)
library(data.table)
library(lubridate)


gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
chapter.folders=gta25_setup(internal.name = 'front matter',
                            in.dev = F,
                            author=NULL,
                            wipe.data = T,
                            wipe.figs = T)
data.path=chapter.folders$data.path
figure.path=chapter.folders$figure.path

# SE requests two maps
# The first map would show the number of times each country's 
# commercial interests have been hit from 1 Jan 2017 to 15 Nov 2019. 

gta_data_slicer(gta.evaluation = c("Red","Amber"),
                implementation.period = c(as.character(as.Date(break.date)+1),cutoff),
                keep.implementation.na = F)

master.sliced$year <- year(master.sliced$date.implemented)
master.sliced <- subset(master.sliced, year<=2019)

nr.hits <- aggregate(intervention.id ~ affected.jurisdiction+a.un, master.sliced, function(x) length(unique(x)))

save(nr.hits, file=paste0(data.path,"Nr of hits in populist era.Rdata"))

# The second map would show the share of a country's exports that 
# are affected by harmful foreign measures implemented from 1 Jan 2017 to 15 Nov 2019. 

gta_trade_coverage(gta.evaluation=c("Red","Amber"),
                   group.exporters = F,
                   implementation.period = c(as.character(as.Date(break.date)+1),cutoff),
                   coverage.period = c(2019,2019),
                   intervention.ids = c(71578, 71561), # manually removed by simon
                   keep.interventions = F,
                   trade.data = 2016) # trade data year used by simon

ex.coverages <- trade.coverage.estimates                   
ex.coverages <- merge(ex.coverages, gtalibrary::country.names[,c("un_code","name")], by.x="Exporting country", by.y="name")
ex.coverages <- ex.coverages[,c("Exporting country","un_code","Trade coverage estimate for 2019")]
names(ex.coverages) <- c("affected.jurisdiction","un","coverage")

save(ex.coverages, file=paste0(data.path,"Exports affected in populist era.Rdata"))

