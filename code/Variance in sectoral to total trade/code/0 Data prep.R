rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
source('0 report production/GTA 25/help files/GTA 25 cutoff and definitions.R')

this.chapter=c(paste0("Variance in sectoral to total trade"))

wdpath = "0 dev/gta-25-pb/code/Variance in sectoral to total trade/"
data.path = paste0(wdpath,"data/")

run.calc=T

# Figure 1 data prep ------------------------------------------------------

# For the years 2005 to 2018, setting the value of both series to 100 in 2007, 
# please produce a line chart showing the total global value of trade in USD 
# and the total value of the relevant sector's trade worldwide. In total, then, 
# there should be a chart for each sector showing how much the total value of 
# sectoral trade varied compared to that of world trade.

# Calculate total trade and trade per sectors ------------------------------------------------------
if (run.calc) {
  # Get Trade data for 2005 and 2006
  load("data/comtrade/comtrade replica 2005-2016 - HS 2012 - balanced.RData")
  head(comtrade.hs12)
  
  conversion <- read.csv("definitions/cpc-to-hs/hs 2012 to cpc 2_1.csv",sep=";")
  conversion$cpc <- sprintf("%03s",conversion$cpc)
  conversion$cpc <- as.numeric(substr(conversion$cpc, 1,2))
  
  # Subset to 2005,2006 and aggregate
  trade.0506 <- subset(comtrade.hs12, Period < 2007)
  trade.0506 <- aggregate(Trade.Value ~ hs+Period, trade.0506, function(x) sum(x))
  
  # Merge with cpc
  trade.0506 <- merge(trade.0506, conversion, by="hs")
  
  # Load rest of trade data with library
  trade.data <- data.frame()
  for (year in 2007:2018) {
    gta_trade_value_bilateral(trade.data = year)
    trade.temp <- aggregate(trade.value ~ hs6, trade.base.bilateral, function(x) sum(x))
    trade.data <- rbind(trade.data, data.frame(hs=trade.temp$hs6,
                                               Trade.Value=trade.temp$trade.value,
                                               Period=year))
  }
  
  # Merge trade.data with cpc
  trade.data <- merge(trade.data, conversion, by="hs")
  
  # Rbind trade.0506 with trade.data
  trade.data <- rbind(trade.data, trade.0506)
  
  
  # RUN SECTOR CALCULATIONS
  sct.trade.data <- data.frame()
  for (sct in sectors) {
    sct.trade <- subset(trade.data, cpc == sct)
    sct.trade <- aggregate(Trade.Value ~ Period+cpc, sct.trade, function(x) sum(x))
    sct.trade$index.2007 <- sct.trade$Trade.Value/sct.trade$Trade.Value[sct.trade$Period==2007]
    
    sct.trade.data <- rbind(sct.trade.data, sct.trade)
    rm(sct.trade)
  }
  save(sct.trade.data, file=paste0(data.path,"Trade per sector indexed 2007.Rdata"))
  
  # RUN GLOBAL CALCULATIONS
  global.trade <- aggregate(Trade.Value ~ Period, trade.data, function(x) sum(x))
  global.trade$index.2007 <- global.trade$Trade.Value/global.trade$Trade.Value[global.trade$Period==2007]
  
  save(global.trade, file=paste0(data.path,"Trade global indexed 2007.Rdata"))
  
}
load(paste0(data.path,"Trade per sector indexed 2007.Rdata"))
load(paste0(data.path,"Trade global indexed 2007.Rdata"))
