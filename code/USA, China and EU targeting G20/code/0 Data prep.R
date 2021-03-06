rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(WDI)
library(plyr)


gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')


directories=gta25_setup(internal.name="USA, China and EU targeting G20",
                        in.dev=F,
                        author=NULL,
                        wipe.data=T,
                        wipe.figs=T)

data.path = directories$data.path

period <- c(as.character(as.Date(break.date)+1), cutoff)
dest.markets <- list(c(156),c(840),c(eu.members))
dest.markets.names <- c("China","United States of America", "EU")

trade.data.year = "base"
gdp.year=2016

run.calc=T

# These are 3x2 scatter plots for the destination markets China, USA and EU.

# Figure 1 data prep ------------------------------------------------------

# Graph 1: A scatter plot for all G20 (except destination market). Y-axis shows the
# % bilateral exports to destination market (China, USA, or EU) affected by 
# harmful measures implemented by destination market since 2017-01-01. 
# X-axis shows the amount of exports to the destination market divided 
# by the GDP of the exporter (share of GDP being exported to the destination market).

## Graph 2: 
## Simple scatter of y-axis from graph 1 to % bilateral exports to destination benefitting from liberalisations.

# Get g20 GDP data from WDI
indicator <- WDIsearch(string="GDP")
indicator <- "NY.GDP.MKTP.CD"

gdp.countries <- WDI(indicator=indicator, start=gdp.year, end=gdp.year, extra=T)
gdp.countries <- merge(gdp.countries[,c("NY.GDP.MKTP.CD","iso3c")], gtalibrary::country.names[,c("un_code","iso_code")], by.x="iso3c", by.y="iso_code")
gdp.countries <- subset(gdp.countries, un_code %in% g20.members)[,c("NY.GDP.MKTP.CD","un_code")]
names(gdp.countries) <- c("gdp","un_code")

# Calculate g20 affected by destination market ------------------------------------------------------

if (run.calc) {
  coverages <- data.frame()
  
  for (dst in 1:length(dest.markets)) {
    
    target <- g20.members[! g20.members %in% dest.markets[[dst]]]
    
      gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                         exporters = target,
                         keep.exporters = T,
                         group.exporters = F,
                         importers = dest.markets[[dst]],
                         keep.importers = T,
                         implementer.role = "importer",
                         coverage.period = c(2019,2019),
                         implementation.period = period,
                         trade.data = trade.data.year)
      
      coverages <- rbind(coverages, data.frame(importer = dest.markets.names[dst],
                                               type = "harmful",
                                               exporter = trade.coverage.estimates$`Exporting country`,
                                               coverages = as.numeric(trade.coverage.estimates[,ncol(trade.coverage.estimates)])))
      rm(trade.coverage.estimates)
      
      gta_trade_coverage(gta.evaluation = c("Green"),
                         exporters = target,
                         keep.exporters = T,
                         group.exporters = F,
                         importers = dest.markets[[dst]],
                         keep.importers = T,
                         implementer.role = "importer",
                         coverage.period = c(2019,2019),
                         implementation.period = period,
                         trade.data = trade.data.year)
      
      coverages <- rbind(coverages, data.frame(importer = dest.markets.names[dst],
                                               type = "liberalising",
                                               exporter = trade.coverage.estimates$`Exporting country`,
                                               coverages = as.numeric(trade.coverage.estimates[,ncol(trade.coverage.estimates)])))
      rm(trade.coverage.estimates)
      
  }
  
  
   # Add trade data
  gta_trade_value_bilateral(exporting.country = g20.members,
                            keep.exporter = T,
                            importing.country = c(dest.markets[[1]], dest.markets[[2]],dest.markets[[3]]),
                            keep.importer = T,
                            trade.data = gdp.year)
  
  trade.base.bilateral$i.un[trade.base.bilateral$i.un %in% eu.members] <- 999
  trade.data <- aggregate(trade.value ~ a.un+i.un, trade.base.bilateral, function(x) sum(x))
  trade.data$importer <- mapvalues(trade.data$i.un,
                               from=c(156,840,999),
                               to=c("China","United States of America","EU"))
  trade.data$exporter <- mapvalues(trade.data$a.un,
                                   from=g20.members,
                                   to=g20.member.names)
  
  # Merge trade and gdp data
  gdp.countries$exporter <- mapvalues(gdp.countries$un_code,
                                       from = g20.members,
                                       to = g20.member.names)
  
  coverages <- merge(coverages, trade.data[,c("importer","trade.value","exporter")], by=c("importer","exporter"))
  coverages <- merge(coverages, gdp.countries[,c("gdp","exporter")], by="exporter")
  coverages$gdp.share <- coverages$trade.value/coverages$gdp
  
  # Add share difference between harmful and liberalising
  coverages <- pivot_wider(data=coverages, names_from = "type",values_from = "coverages")
  # coverages$coverage.difference = coverages$harmful - coverages$liberalising
  
  save(coverages, file=paste0(data.path,"Populist era g20 targeting.Rdata"))
}