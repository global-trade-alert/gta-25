rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
source('0 report production/GTA 25/help files/GTA 25 cutoff and definitions.R')

this.chapter=c(paste0("Sectoral chapters - Sector ",paste0(sectors)))

wdpath = "0 dev/gta-25-pb/code/Sectoral chapters panel 1/"
data.path = paste0(wdpath,"data/")

run.calc=T

trade.data.year = 2018

# Figure 1 data prep ------------------------------------------------------

# Chart 1: Line graph showing share of total sectoral trade 
# affected by discriminatory (red) and liberalising (green) 
# measures. X-axis should divided into pre-populist era (2009-2016) 
# and populist era (2017-2019), by shading the populist era plot 
# background. Add two dots (green and red) in 2019 showing the 
# share of global trade affected by discriminatory and liberalising measures.

# Calculate trade affected by sectors - harmful ------------------------------------------------------
if (run.calc) {
  sct.cov.harmful <- data.frame()
  for (sct in sectors) {
    codes <- gta_cpc_code_expand(codes = sct)
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       cpc.sectors = codes,
                       keep.cpc = T,
                       implementation.period = c(NA,cutoff),
                       trade.data = trade.data.year)
    sct.cov.harmful <- rbind(sct.cov.harmful, data.frame(sector=sct,
                                                           year=seq(2009,2019,1),
                                                           coverages=as.numeric(trade.coverage.estimates[1,c(4:14)])))
    rm(trade.coverage.estimates)
  }
  save(sct.cov.harmful, file=paste0(data.path,"Sector coverages harmful.Rdata"))
}
load(paste0(data.path,"Sector coverages harmful.Rdata"))


# Calculate trade affected by sectors - liberalising ------------------------------------------------------

if (run.calc) {
  sct.cov.liberalising <- data.frame()
  for (sct in sectors) {
    codes <- gta_cpc_code_expand(codes = sct)
    gta_trade_coverage(gta.evaluation = c("Green"),
                       cpc.sectors = codes,
                       keep.cpc = T,
                       implementation.period = c(NA,cutoff),
                       trade.data = trade.data.year)
    sct.cov.liberalising <- rbind(sct.cov.liberalising, data.frame(sector=sct,
                                                           year=seq(2009,2019,1),
                                                           coverages=as.numeric(trade.coverage.estimates[1,c(4:14)])))
    rm(trade.coverage.estimates)
  }
  save(sct.cov.liberalising, file=paste0(data.path,"Sector coverages liberalising.Rdata"))
}
load(paste0(data.path,"Sector coverages liberalising.Rdata"))

#

# Calculate global trade affected - harmful ------------------------------------------------------

if (run.calc) {
  gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                     coverage.period = c(2019,2019),
                     implementation.period = c(NA,cutoff),
                     trade.data = trade.data.year)
  
  glo.cov.harmful <- trade.coverage.estimates[,ncol(trade.coverage.estimates)]
  
  save(glo.cov.harmful, file=paste0(data.path,"Global coverage harmful 2019.Rdata"))
}
load(paste0(data.path,"Global coverage harmful 2019.Rdata"))

# Calculate global trade affected - liberalising ------------------------------------------------------

if (run.calc){ 
  gta_trade_coverage(gta.evaluation = c("Green"),
                     coverage.period = c(2019,2019),
                     implementation.period = c(NA,cutoff),
                     trade.data = trade.data.year)
  
  glo.cov.liberalising <- trade.coverage.estimates[,ncol(trade.coverage.estimates)]
  
  save(glo.cov.liberalising, file=paste0(data.path,"Global coverage liberalising 2019.Rdata"))
}
load(paste0(data.path,"Global coverage liberalising 2019.Rdata"))


# Figure 2 data prep ------------------------------------------------------

# Chart 2: Line graph showing share of total sectoral trade affected
# by (1) all discriminatory measures, (2) export incentives 
# (3) All except export incentives, (4) subsidies and (5) tariff 
# incentives. Add shading as before for populist, pre-populist era.

# (1) is calculated in fig1 already


# Trade affected by intervention types ------------------------------------

if (run.calc) {
  
  chapters <- list(c("P"),
                   c(as.character(unique(gtalibrary::int.mast.types$mast.chapter.id[! gtalibrary::int.mast.types$mast.chapter.id %in% c("P")]))),
                   c("L"),
                   c("TARIFF"))
  
  type.names <- c("P",
                  "All except P",
                  "L",
                  "TARIFF")
  
  sct.cov.types.harmful <- data.frame()
  
  for (sct in sectors) {
    for (mst in 1:length(chapters)) {
      codes <- gta_cpc_code_expand(codes = sct)
      gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                         mast.chapters = chapters[[mst]],
                         keep.mast = T,
                         group.mast = T,
                         cpc.sectors = codes,
                         keep.cpc = T,
                         implementation.period = c(NA,cutoff),
                         trade.data = trade.data.year)
      sct.cov.types.harmful <- rbind(sct.cov.types.harmful, data.frame(sector=sct,
                                                                    year=seq(2009,2019,1),
                                                                    coverages=as.numeric(trade.coverage.estimates[1,c(4:14)]),
                                                                    type = type.names[mst]))
      rm(trade.coverage.estimates)
    }
  }
  save(sct.cov.types.harmful, file=paste0(data.path,"Sector coverages types harmful.Rdata"))
}

load(paste0(data.path,"Sector coverages types harmful.Rdata"))


# Figure 3 data prep ------------------------------------------------------

# Chart 3: Line graph showing share of total sectoral trade 
# affected by (1) all liberalising measures, (2) subsidy reductions 
# and (3) tariff reductions. Add shading for populist, pre-populist era.

# (1) is calculated in fig1 already

if (run.calc) {
  
  chapters <- list(c("L"),
                   c("TARIFF"))
  
  type.names <- c("L",
                  "TARIFF")
  
  sct.cov.types.liberalising <- data.frame()
  
  for (sct in sectors) {
    for (mst in 1:length(chapters)) {
      codes <- gta_cpc_code_expand(codes = sct)
      gta_trade_coverage(gta.evaluation = c("Green"),
                         mast.chapters = chapters[[mst]],
                         keep.mast = T,
                         group.mast = T,
                         cpc.sectors = codes,
                         keep.cpc = T,
                         implementation.period = c(NA,cutoff),
                         trade.data = trade.data.year)
      sct.cov.types.liberalising <- rbind(sct.cov.types.liberalising, data.frame(sector=sct,
                                                                   year=seq(2009,2019,1),
                                                                   coverages=as.numeric(trade.coverage.estimates[1,c(4:14)]),
                                                                   type = type.names[mst]))
      rm(trade.coverage.estimates)
    }
  }
  save(sct.cov.types.liberalising, file=paste0(data.path,"Sector coverages types liberalising.Rdata"))
}

load(paste0(data.path,"Sector coverages types liberalising.Rdata"))


# Figure 4 data prep ------------------------------------------------------

# Chart 4: Stacked bar chart (maybe there's a better way of visualising it?) showing 
# share of sectoral trade affected by harmful interventions hitting 1, 2, 3-5, 6-10 and 11+ 
# trading partners. Add populist and pre-populist era shading.

if (run.calc) {
  sct.cov.hit.brkts <- data.frame()
  for (sct in sectors) {
    codes <- gta_cpc_code_expand(codes = sct)
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       cpc.sectors = codes,
                       keep.cpc = T,
                       hit.brackets = c(1,1,2,2,3,5,6,10,11,9999999),
                       implementation.period = c(NA,cutoff),
                       trade.data = trade.data.year)
    
    names(trade.coverage.estimates) <- c("importer","exporter","hits",2009:2019)
    temp <- trade.coverage.estimates[,c("hits",2009:2019)]
    temp <- pivot_longer(data = temp, cols = c(2:ncol(temp)), names_to = "year", values_to = "coverages")
    sct.cov.hit.brkts <- rbind(sct.cov.hit.brkts, data.frame(sector=sct,
                                                             year=temp$year,
                                                             coverages=temp$coverages,
                                                             hits=temp$hits))
    rm(trade.coverage.estimates,temp)
    
  }
  sct.cov.hit.brkts$hits <- as.character(sct.cov.hit.brkts$hits)
  sct.cov.hit.brkts$hits[sct.cov.hit.brkts$hits == "1 - 1"] <- "1"
  sct.cov.hit.brkts$hits[sct.cov.hit.brkts$hits == "2 - 2"] <- "2"

  save(sct.cov.hit.brkts, file=paste0(data.path,"Sector coverages hit brackets.Rdata"))

  }
load(paste0(data.path,"Sector coverages hit brackets.Rdata"))


