rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
source('0 report production/GTA 25/help files/GTA 25 cutoff and definitions.R')

this.chapter=c(paste0("Sectoral chapters - Sector ",paste0(sectors)))

wdpath = "0 dev/gta-25-pb/code/Country groups targeted by destination markets/"
data.path = paste0(wdpath,"data/")

run.calc=T

trade.data.year = 2018

# Figure 1 data prep ------------------------------------------------------

# Graph 1: Y-axis shows the share of bilateral exports of LDCs/African Union
# facing (1) harmful measures implemented by destination markets and (2) reforming/liberalising
# measures implemented by destination market. The X-axis shows the destination markets. (see attachement)

dest.markets <- list(c(840), c(eu.members), c(840), c(g7.members), c(brics.members), c(g20.members))
dest.markets.names <- c("China","EU","USA","G7","BRICS","G20")
targeted <- list(c(ldc.countries),c(au.members),c(wb.low.mid.ctry),c(wb.upp.mid.ctry))
targeted.names <- c("LDCs","African Union","Lower middle income countries","Upper middle income countries")

# Calculate trade affected by destination market - harmful ------------------------------------------------------
if (run.calc) {
  dest.markets.cov <- data.frame()
  
  for (dst in 1:length(dest.markets)) {
    for(trg in 1:length(targeted)) {
      
      gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                         exporters = targeted[[trg]],
                         keep.exporters = T,
                         importers = dest.markets[[dst]],
                         keep.importers = T,
                         implementer.role = "importer",
                         coverage.period = c(2019,2019),
                         trade.data = trade.data.year)
      
      dest.markets.cov <- rbind(dest.markets.cov, data.frame(importer = dest.markets.names[dst],
                                                             exporter = targeted.names[trg],
                                                             type = "harmful",
                                                             order = dst,
                                                             coverages = as.numeric(trade.coverage.estimates[1,ncol(trade.coverage.estimates)])))
      rm(trade.coverage.estimates)
      
      gta_trade_coverage(gta.evaluation = c("Green"),
                         exporters = targeted[[trg]],
                         keep.exporters = T,
                         importers = dest.markets[[dst]],
                         keep.importers = T,
                         implementer.role = "importer",
                         coverage.period = c(2019,2019),
                         trade.data = trade.data.year)
      
      dest.markets.cov <- rbind(dest.markets.cov, data.frame(importer = dest.markets.names[dst],
                                                             exporter = targeted.names[trg],
                                                             type = "liberalising",
                                                             order = dst,
                                                             coverages = as.numeric(trade.coverage.estimates[1,ncol(trade.coverage.estimates)])))
      rm(trade.coverage.estimates)
    }
  }
  save(dest.markets.cov, file=paste0(data.path,"Destination markets targeting.Rdata"))
}
load(paste0(data.path,"Destination markets targeting.Rdata"))


