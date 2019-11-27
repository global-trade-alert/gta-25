rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
source('0 report production/GTA 25/help files/GTA 25 cutoff and definitions.R')

this.chapter=c(paste0("Sectoral chapters - Sector ",paste0(sectors)))

wdpath = "0 dev/gta-25/code/Sectoral chapters panel 3-4/"
data.path = paste0(wdpath,"data/")

run.calc=T

# Figure 9 data prep ------------------------------------------------------

# Chart 9: Heat Map for all G20. Y-axis shows the importing nations, 
# X-axis shows the exporting nations. Place USA and China at beginning,
# (top left) and all other countries alphabetically after them. Graph 
# shows share of sectoral exports affected by trade distortions in force 
# today implemented by the importing nation. Show in shades of red.

# Calculate trade affected by sectors - harmful ------------------------------------------------------
if (run.calc) {
  
  sct.g20.harmful <- data.frame()
  for (sct in sectors) {
    codes <- gta_cpc_code_expand(codes = sct)
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       implementation.period = c(NA,cutoff),
                       implementer.role = "importer",
                       importers = g20.members,
                       keep.importers = T,
                       group.importers = F,
                       exporters = g20.members,
                       keep.exporters = T,
                       group.exporters = F,
                       cpc.sectors = codes,
                       keep.cpc = T,
                       coverage.period = c(2019,2019))
    
    names(trade.coverage.estimates) <- c("importer","exporter","hits",2019)
    temp <- trade.coverage.estimates[,c("importer","exporter",2019)]
    temp$sector=sct
    sct.g20.harmful <- rbind(sct.g20.harmful, temp)
  }
  save(sct.g20.harmful, file=paste0(data.path,"G20 sector exports coverages - harmful.Rdata"))
}

load(paste0(data.path,"G20 sector exports coverages - harmful.Rdata"))


# Figure 10 data prep ------------------------------------------------------

# Chart 10: Same as chart 9, but with liberalising measures.

# Calculate trade affected by sectors - liberalising ------------------------------------------------------
if (run.calc) {
  
  sct.g20.liberalising <- data.frame()
  for (sct in sectors) {
    codes <- gta_cpc_code_expand(codes = sct)
    gta_trade_coverage(gta.evaluation = c("Green"),
                       implementation.period = c(NA,cutoff),
                       implementer.role = "importer",
                       importers = g20.members,
                       keep.importers = T,
                       group.importers = F,
                       exporters = g20.members,
                       keep.exporters = T,
                       group.exporters = F,
                       cpc.sectors = codes,
                       keep.cpc = T,
                       coverage.period = c(2019,2019))
    
    names(trade.coverage.estimates) <- c("importer","exporter","hits",2019)
    temp <- trade.coverage.estimates[,c("importer","exporter",2019)]
    temp$sector=sct
    sct.g20.liberalising <- rbind(sct.g20.liberalising, temp)
  }
  save(sct.g20.liberalising, file=paste0(data.path,"G20 sector exports coverages - liberalising.Rdata"))
}

load(paste0(data.path,"G20 sector exports coverages - liberalising.Rdata"))


# Figure 11 data prep ------------------------------------------------------

# Chart 11: Heat Map for all G20. Y-axis shows the importing 
# nations, X-axis shows the exporting nations. Place USA and 
# China at beginning, (top left) and all other countries 
# alphabetically after them. Graph shows the change in share 
# of exports affected by trade distortions in force today 
# (cut off date) to distortions in force at 31.12.16. The 
# Graph will have 3 shades (green for negative change, 
# white for zero change and red for positive change).

if (run.calc) {
  
  sct.g20.change.harmful <- data.frame()
  for (sct in sectors) {
    codes <- gta_cpc_code_expand(codes = sct)
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       implementation.period = c(NA,break.date),
                       implementer.role = "importer",
                       importers = g20.members,
                       keep.importers = T,
                       group.importers = F,
                       exporters = g20.members,
                       keep.exporters = T,
                       group.exporters = F,
                       cpc.sectors = codes,
                       keep.cpc = T,
                       coverage.period = c(2019,2019))
    
    cov.parked <- trade.coverage.estimates
    cov.parked$type <- "pre.populist"
    
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       implementation.period = c(NA,cutoff),
                       implementer.role = "importer",
                       importers = g20.members,
                       keep.importers = T,
                       group.importers = F,
                       exporters = g20.members,
                       keep.exporters = T,
                       group.exporters = F,
                       cpc.sectors = codes,
                       keep.cpc = T,
                       coverage.period = c(2019,2019))
    
    trade.coverage.estimates$type <- "populist"
    
    temp <- rbind(cov.parked, trade.coverage.estimates)
    names(temp) <- c("importer","exporter","hits",2019, "type")
    temp <- temp[,c("importer","exporter","type",2019)]
    temp$sector=sct
    temp <- pivot_wider(data = temp, names_from = "type", values_from = "2019")
    temp$change <- (temp$populist/temp$pre.populist)-1
    sct.g20.change.harmful <- rbind(sct.g20.change.harmful, temp[,c("importer","exporter","change","sector","populist","pre.populist")])
  }
  save(sct.g20.change.harmful, file=paste0(data.path,"G20 sector coverage change - harmful.Rdata"))
}

load(paste0(data.path,"G20 sector coverage change - harmful.Rdata"))


# Figure 12 data prep ------------------------------------------------------

# Chart 12: Same as chart 11, but for liberalising measures.

if (run.calc) {
  
  sct.g20.change.liberalising <- data.frame()
  for (sct in sectors) {
    codes <- gta_cpc_code_expand(codes = sct)
    gta_trade_coverage(gta.evaluation = c("Green"),
                       implementation.period = c(NA,break.date),
                       implementer.role = "importer",
                       importers = g20.members,
                       keep.importers = T,
                       group.importers = F,
                       exporters = g20.members,
                       keep.exporters = T,
                       group.exporters = F,
                       cpc.sectors = codes,
                       keep.cpc = T,
                       coverage.period = c(2019,2019))
    
    cov.parked <- trade.coverage.estimates
    cov.parked$type <- "pre.populist"
    
    gta_trade_coverage(gta.evaluation = c("Green"),
                       implementation.period = c(NA,cutoff),
                       implementer.role = "importer",
                       importers = g20.members,
                       keep.importers = T,
                       group.importers = F,
                       exporters = g20.members,
                       keep.exporters = T,
                       group.exporters = F,
                       cpc.sectors = codes,
                       keep.cpc = T,
                       coverage.period = c(2019,2019))
    
    trade.coverage.estimates$type <- "populist"
    
    temp <- rbind(cov.parked, trade.coverage.estimates)
    names(temp) <- c("importer","exporter","hits",2019, "type")
    temp <- temp[,c("importer","exporter","type",2019)]
    temp$sector=sct
    temp <- pivot_wider(data = temp, names_from = "type", values_from = "2019")
    temp$change <- (temp$populist/temp$pre.populist)-1
    sct.g20.change.liberalising <- rbind(sct.g20.change.liberalising, temp[,c("importer","exporter","change","sector","populist","pre.populist")])
  }
  save(sct.g20.change.liberalising, file=paste0(data.path,"G20 sector coverage change - liberalising.Rdata"))
}

load(paste0(data.path,"G20 sector coverage change - liberalising.Rdata"))
