rm(list=ls())

library(gtalibrary)
library(stringr)
library(plyr)
library(data.table)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
chapter.folders=gta25_setup(internal.name = "What's new",
                            in.dev = F,
                            author=NA,
                            wipe.data = T,
                            wipe.figs = T)
data.path=chapter.folders$data.path
figure.path=chapter.folders$figure.path

# Figure 1 data prep ------------------------------------------------------
# Request: Graph showing the number of new discriminatory measures documented 
# from 01.01.09 to 15.09.09 found by Sep. 15 2010, 2011, ..., 2019.
# Add two lines, describing the same, but for Oct. 15 and Nov. 15.

gta_data_slicer(gta.evaluation = c('Amber','Red'))

cutoffs=c(paste(2009:2019, "09-15", sep="-"))

published.ids=data.frame()

for(cut in cutoffs){
  published.ids=rbind(published.ids,
                      data.frame(reporting.deadline=cut,
                                 intervention.count=length(unique(subset(master.sliced, date.published <=cut & date.implemented>="2009-01-01" & date.implemented <="2009-09-15")$intervention.id)),
                                 stringsAsFactors = F))
}



save(published.ids, file=paste0(data.path,'published interventions.Rdata'))


# Figure 2 (CURRENTLY UNDER REVISION)
# "Graph showing the total of interventions in the database for each year splitted by (1) Interventions having at least one official source, 
# (2) Interventions coming from company self-declaration and 
# (3) coming from other non-official sources."

library(gtalibrary)
library(gtasql)
library(pool)
library(RMariaDB)
library(data.table)
library(stringr)

gta_sql_pool_open(db.title="gtamain",
                  db.host = gta_pwd("gtamain")$host,
                  db.name = gta_pwd("gtamain")$name,
                  db.user = gta_pwd("gtamain")$user,
                  db.password = gta_pwd("gtamain")$password,
                  table.prefix = "gta_")

gta.sa=gta_sql_load_table("measure")


gta.sa$source.type="Official source"
gta.sa$source.type[gta.sa$is.source.official==F]="News or other reports"
gta.sa$source.type[grepl("wind.com.cn", gta.sa$source, ignore.case = T)]="Chinese company filing"

## number of official sources per year
sa.src.yr=aggregate(id ~ year(creation.date) + source.type, subset(gta.sa, status.id==4), function(x) length(unique(x)))
names(sa.src.yr)=c("year","source.type","sa.count")

save(sa.src.yr, file=paste0(data.path,'state act sources.Rdata'))

gta_sql_pool_close()
gta_sql_kill_connections()
