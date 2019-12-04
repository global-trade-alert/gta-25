rm(list=ls())

library(gtalibrary)
library(stringr)
library(plyr)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
chapter.path='0 dev/gta-25/code/'
this.chapter="What's new in the GTA"
data.path=paste0(chapter.path, this.chapter, '/data/')

# Figure 1 data prep ------------------------------------------------------
# Request: Graph showing the number of new discriminatory measures documented 
# from 01.01.09 to 15.09.09 found by Sep. 15 2010, 2011, ..., 2019.
# Add two lines, describing the same, but for Oct. 15 and Nov. 15.

gta_data_slicer(gta.evaluation = c('Amber','Red'))
master.sliced=master.sliced[!is.na(master.sliced$date.implemented),]
dates=c(as.Date(c('2009-01-01',paste0(2009:2020,'-09-15'))),
        as.Date(c('2009-01-01',paste0(2009:2020,'-10-15'))),
        as.Date(c('2009-01-01',paste0(2009:2020,'-11-15'))))

published.ids=data.frame(published.by=dates[1:length(dates)], 
                         intervention.count=unlist( 
                           lapply(dates[1:length(dates)], function(x){
                             length(unique(subset(master.sliced, 
                                                  date.implemented >= dates[1] & 
                                                    date.implemented <= dates[2] & 
                                                    date.published < dates[match(x, dates)])$intervention.id))
                           })
                           , use.names = F),
                         group=mapvalues(str_sub(as.character(dates),6,7), c('09','10','11'), c('September 15', 'October 15', 'November 15'))
)

published.ids=published.ids[str_sub(as.character(published.ids$published.by),3,4)!=20,]
published.ids$group[str_sub(as.character(published.ids$published.by),6,7)=='01']=published.ids$group[1+which(str_sub(as.character(published.ids$published.by),6,7)=='01')]

save(published.ids, file=paste0(data.path,'published interventions.Rdata'))

