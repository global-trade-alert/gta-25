rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(PRMTools)
library(lubridate)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')


directories=gta25_setup(internal.name="Single & multi-country hits",
                        in.dev=F,
                        author=NULL,
                        wipe.data=F,
                        wipe.figs=F)

data.path = directories$data.path


run.calc=T

trade.data.year = "base"

# LIST OF PERIODS TO BE ITERATED
periods <- list(c("2017-01-01",cutoff),c("2014-01-01","2016-11-15"),c("2009-01-01","2011-11-15"))

# LIST OF MAST CHAPTERS TO BE ITERATED AND IF THEY SHOULD BE KEPT OR NOT
mast.chapters <- list(c("TARIFF",T), c("TARIFF",F))
export.subsidies=int.mast.types$intervention.type[int.mast.types$is.export.promotion==1]

### NEW Figure 1, 6.12.2019 via phone call SE<->JF
# Create a total of seven figures, all seven are timelines on the horizontal axis.
# The timeline counts the number of months since the start of the period.
# The vertical axis show the share of trade affected.
# 
# For every month, we calculate the instrument-targeting combination of the original graph except the single-affected export incentives:
# (1) World trade affected by tariff interventions affecting a single nation in the home market
# (2) World trade affected by tariff interventions affecting mulitple nations in the home market
# (3) World trade affected by all other interventions affecting single nation in the home market
# (4) World trade affected by all other interventions affecting multiple nations in the home market
# (5*) World trade affected by export incentives affecting multiple nations in a foreign market
# (6*) World trade affected by all instruments and any number of affected nations.

# The seventh chart shows the different instruments/targeting all in one plot but for the populist era only.


single.multi.data=data.frame()

for(period in 1:length(periods)){
  period.start=as.Date(periods[[period]][[1]])
  period.end=as.Date(periods[[period]][[2]])
  period.months=seq.Date(period.start, period.end, by = "month")
  
  
  for(monat in period.months){
    monat=as.Date(monat, origin="1970-01-01")
    
    print(paste("starting",format(monat, "%y-%m")))
    # (1) World trade affected by tariff interventions affecting a single nation in the home market
    
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       affected.flows = c("inward"),
                       coverage.period = c(year(monat),year(monat)),
                       implementation.period = c(period.start,(monat+months(1)-1)),
                       mast.chapters = "TARIFF",
                       keep.mast = T,
                       nr.exporters = c(1,1),
                       trade.data = trade.data.year)
    
    
    
    if(exists("trade.coverage.estimates")){
      
      trade.value=as.numeric(trade.coverage.estimates[,ncol(trade.coverage.estimates)])
      rm(trade.coverage.estimates)
    } else {
      
      trade.value=0  
    }
    
    
    single.multi.data=rbind(single.multi.data,
                            data.frame(period.id=period,
                                       month=format(monat, "%y-%m"),
                                       month.count=mondf(period.start, monat),
                                       instrument="tariff",
                                       target="single",
                                       trade.share=trade.value,
                                       stringsAsFactors = F))
    
    
    rm(trade.value)
    
    # (2) World trade affected by tariff interventions affecting mulitple nations in the home market
    
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       affected.flows = c("inward"),
                       coverage.period = c(year(monat),year(monat)),
                       implementation.period = c(period.start,(monat+months(1)-1)),
                       mast.chapters = "TARIFF",
                       keep.mast = T,
                       nr.exporters = c(2,999999),
                       trade.data = trade.data.year)
    
    
    if(exists("trade.coverage.estimates")){
      
      trade.value=as.numeric(trade.coverage.estimates[,ncol(trade.coverage.estimates)])
      rm(trade.coverage.estimates)
    } else {
      
      trade.value=0  
    }
    
    single.multi.data=rbind(single.multi.data,
                            data.frame(period.id=period,
                                       month=format(monat, "%y-%m"),
                                       month.count=mondf(period.start, monat),
                                       instrument="tariff",
                                       target="multi",
                                       trade.share=trade.value,
                                       stringsAsFactors = F))
    
    rm(trade.value)
    
    
    # (3) World trade affected by all other interventions affecting single nation in the home market
    
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       affected.flows = c("inward"),
                       coverage.period = c(year(monat),year(monat)),
                       implementation.period = c(period.start,(monat+months(1)-1)),
                       mast.chapters = "TARIFF",
                       keep.mast = F,
                       nr.exporters = c(1,1),
                       trade.data = trade.data.year)
    
    
    if(exists("trade.coverage.estimates")){
      
      trade.value=as.numeric(trade.coverage.estimates[,ncol(trade.coverage.estimates)])
      rm(trade.coverage.estimates)
    } else {
      
      trade.value=0  
    }
    
    single.multi.data=rbind(single.multi.data,
                            data.frame(period.id=period,
                                       month=format(monat, "%y-%m"),
                                       month.count=mondf(period.start, monat),
                                       instrument="non-tariff",
                                       target="single",
                                       trade.share=trade.value,
                                       stringsAsFactors = F))
    
    rm(trade.value)
    
    
    # (4) World trade affected by all other interventions affecting multiple nations in the home market
    
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       affected.flows = c("inward"),
                       coverage.period = c(year(monat),year(monat)),
                       implementation.period = c(period.start,(monat+months(1)-1)),
                       mast.chapters = "TARIFF",
                       keep.mast = F,
                       nr.exporters = c(2,99999),
                       trade.data = trade.data.year)
    
    
    if(exists("trade.coverage.estimates")){
      
      trade.value=as.numeric(trade.coverage.estimates[,ncol(trade.coverage.estimates)])
      rm(trade.coverage.estimates)
    } else {
      
      trade.value=0  
    }
    
    single.multi.data=rbind(single.multi.data,
                            data.frame(period.id=period,
                                       month=format(monat, "%y-%m"),
                                       month.count=mondf(period.start, monat),
                                       instrument="non-tariff",
                                       target="multi",
                                       trade.share=trade.value,
                                       stringsAsFactors = F))
    
    rm(trade.value)
    
    
    # (5*) World trade affected by export incentives affecting multiple nations in a foreign market
    
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       affected.flows = c("outward subsidy"),
                       coverage.period = c(year(monat),year(monat)),
                       implementation.period = c(period.start,(monat+months(1)-1)),
                       intervention.types = export.subsidies,
                       keep.type = T,
                       trade.data = trade.data.year)
    
    
    if(exists("trade.coverage.estimates")){
      
      trade.value=as.numeric(trade.coverage.estimates[,ncol(trade.coverage.estimates)])
      rm(trade.coverage.estimates)
    } else {
      
      trade.value=0  
    }
    
    single.multi.data=rbind(single.multi.data,
                            data.frame(period.id=period,
                                       month=format(monat, "%y-%m"),
                                       month.count=mondf(period.start, monat),
                                       instrument="export incentive",
                                       target="multi",
                                       trade.share=trade.value,
                                       stringsAsFactors = F))
    
    rm(trade.value)
    
    
    # (6*) World trade affected by all instruments and any number of affected nations.
    
    
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       affected.flows = c("outward subsidy", "inward"),
                       coverage.period = c(year(monat),year(monat)),
                       implementation.period = c(period.start,(monat+months(1)-1)),
                       trade.data = trade.data.year)
    
    
    if(exists("trade.coverage.estimates")){
      
      trade.value=as.numeric(trade.coverage.estimates[,ncol(trade.coverage.estimates)])
      rm(trade.coverage.estimates)
    } else {
      
      trade.value=0  
    }
    
    single.multi.data=rbind(single.multi.data,
                            data.frame(period.id=period,
                                       month=format(monat, "%y-%m"),
                                       month.count=mondf(period.start, monat),
                                       instrument="all",
                                       target="all",
                                       trade.share=trade.value,
                                       stringsAsFactors = F))
    
    rm(trade.value)
    
    
  }
  
  
  
}

save(single.multi.data, file=paste0(data.path,"Multiple nation coverages.Rdata"))
