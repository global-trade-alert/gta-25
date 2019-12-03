rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
source('0 report production/GTA 25/help files/GTA 25 cutoff and definitions.R')

this.chapter="Single and multiple nations affected"

wdpath = "0 dev/gta-25-pb/code/Single and multiple nations affected/"
data.path = paste0(wdpath,"data/")

run.calc=T

trade.data.year = "base"

# LIST OF PERIODS TO BE ITERATED
periods <- list(c("2017-01-01",cutoff),c("2014-01-01",break.date),c("2009-01-01",break.date))

# LIST OF MAST CHAPTERS TO BE ITERATED AND IF THEY SHOULD BE KEPT OR NOT
mast.chapters <- list(c("TARIFF",T), c("TARIFF",F))

# Figure 1 data prep ------------------------------------------------------

# The 3 plot shows the share of world trade facing harmful measures 
# implemented (1) from 2017-2019, (2) from 2014-2016 and (3) from NA-to end of 2016.
# 
# (1) World trade affected by tariff measures affecting a single nation in the home market
# (2) World trade affected by tariff measures affecting mulitple nations in the home market
# (3) World trade affected by all other measures affecting single nation in the home market
# (4) World trade affected by all other measures affecting multiple nations in the home market
# (5) World trade affected by export incentives affecting single nation in a foreign market
# (6) World trade affected by export incentives affecting multiple nations in a foreign market

# Calculate trade affected by tariff measures against single nations - harmful ------------------------------------------------------
if (run.calc) {
  single.nation.cov = data.frame()
  for (p in 1:length(periods)) {
    for (m in 1:length(mast.chapters)) {
      gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                         affected.flows = c("inward"),
                         coverage.period = c(year(periods[[p]][1]),year(periods[[p]][2])),
                         implementation.period = c(periods[[p]][1],periods[[p]][2]),
                         mast.chapters = mast.chapters[[m]][1],
                         keep.mast = mast.chapters[[m]][2]==T,
                         nr.exporters = c(1,1),
                         incl.exporters.strictness = "ONE",
                         trade.data = trade.data.year)
      
      mast.chapter = mast.chapters[[m]][1]
      if(mast.chapters[[m]][2]==F) {
        mast.chapter <- paste0("NOT.",mast.chapter)
      }
      #Average coverage
      cov.avg <- mean(as.numeric(trade.coverage.estimates[,c(4:ncol(trade.coverage.estimates))]))
      single.nation.cov <- rbind(single.nation.cov, data.frame(mast.chapter=mast.chapter,
                                                               nations.affected="one",
                                                               period=p,
                                                               coverages=cov.avg))
    }
    
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       affected.flows = c("outward subsidy"),
                       coverage.period = c(year(periods[[p]][1]),year(periods[[p]][2])),
                       implementation.period = c(periods[[p]][1],periods[[p]][2]),
                       nr.exporters = c(1,1),
                       incl.exporters.strictness = "ONE",
                       trade.data = trade.data.year)
    
    cov.avg <- mean(as.numeric(trade.coverage.estimates[,c(4:ncol(trade.coverage.estimates))]))
    
    single.nation.cov <- rbind(single.nation.cov, data.frame(mast.chapter="export incentives",
                                                             nations.affected="one",
                                                             period=p,
                                                             coverages=cov.avg))
    
    
  }
  rm(trade.coverage.estimates)
  save(single.nation.cov, file=paste0(data.path,"Single nation coverages.Rdata"))
}
load(paste0(data.path,"Single nation coverages.Rdata"))


# Calculate trade affected by tariff measures against multiple nations - harmful ------------------------------------------------------

if (run.calc) {
  multiple.nation.cov = data.frame()
  for (p in 1:length(periods)) {
    for (m in 1:length(mast.chapters)) {
      gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                         affected.flows = c("inward"),
                         coverage.period = c(year(periods[[p]][1]),year(periods[[p]][2])),
                         implementation.period = c(periods[[p]][1],periods[[p]][2]),
                         mast.chapters = mast.chapters[[m]][1],
                         keep.mast = mast.chapters[[m]][2]==T,
                         nr.exporters = c(2,999),
                         trade.data = trade.data.year)
      
      mast.chapter = mast.chapters[[m]][1]
      if(mast.chapters[[m]][2]==F) {
        mast.chapter <- paste0("NOT.",mast.chapter)
      }
      
      cov.avg <- mean(as.numeric(trade.coverage.estimates[,c(4:ncol(trade.coverage.estimates))]))
      
      multiple.nation.cov <- rbind(multiple.nation.cov, data.frame(mast.chapter=mast.chapter,
                                                               nations.affected="multiple",
                                                               period=p,
                                                               coverages=cov.avg))
    }
    
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       affected.flows = c("outward subsidy"),
                       coverage.period = c(year(periods[[p]][1]),year(periods[[p]][2])),
                       implementation.period = c(periods[[p]][1],periods[[p]][2]),
                       nr.exporters = c(2,999),
                       trade.data = trade.data.year)
    
    cov.avg <- mean(as.numeric(trade.coverage.estimates[,c(4:ncol(trade.coverage.estimates))]))
    
    multiple.nation.cov <- rbind(multiple.nation.cov, data.frame(mast.chapter="export incentives",
                                                                 nations.affected="multiple",
                                                                 period=p,
                                                                 coverages=cov.avg))
    
  }
  rm(trade.coverage.estimates)
  save(multiple.nation.cov, file=paste0(data.path,"Multiple nation coverages.Rdata"))
}
load(paste0(data.path,"Multiple nation coverages.Rdata"))

