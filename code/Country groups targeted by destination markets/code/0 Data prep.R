rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)


gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')


directories=gta25_setup(internal.name="Targeting LDCs etc",
                        in.dev=F,
                        author=NULL,
                        wipe.data=T,
                        wipe.figs=T)

data.path = directories$data.path


run.calc=T

trade.data.year = "base"
period <- c(as.character(as.Date(break.date)+1), cutoff)
# Figure 1 data prep ------------------------------------------------------

# Graph 1: Y-axis shows the share of bilateral exports of LDCs/African Union
# facing (1) harmful measures implemented by destination markets and (2) reforming/liberalising
# measures implemented by destination market. The X-axis shows the destination markets. (see attachement)
bris.members=setdiff(brics.members,156)

dest.markets <- list(c(156), c(eu.members), c(840), c(bris.members), c(g20.members))
dest.markets.names <- c("China","EU","USA","BRIS","G20")
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
                         implementation.period = period,
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
                         implementation.period = period,
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

