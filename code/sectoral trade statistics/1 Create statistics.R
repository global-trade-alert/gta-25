rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(xlsx)
library(scales)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')


directories=gta25_setup(internal.name="Sectoral trade statistics",
                        in.dev=F,
                        author=NULL,
                        wipe.data=F,
                        wipe.figs=T)


data.path = directories$data.path
output.path = directories$figure.path

# $X billion, accounting for Y% of world goods trade



## setting figure paths
first.sector.chapter=8
sector.path=paste0(str_extract(output.path,"^.+?figures/"),paste0(first.sector.chapter:(first.sector.chapter+length(sectors)-1), " - Sector ", sectors,"/"))
wipe.sector.path=F

# Create stats per sector ------------------------------------------------
gta_trade_value_bilateral(trade.data = 2018)
world.trade=sum(trade.base.bilateral$trade.value)

for (sct in sectors) {
  
  s.path=sector.path[grepl(paste0(sct,"/$"),sector.path)]
  
  dir.create(file.path(s.path), showWarnings = FALSE)
  
  
  sec.hs=gta_cpc_to_hs(gta_cpc_code_expand(sct))
  total.sec.trade=sum(subset(trade.base.bilateral, hs6 %in% sec.hs)$trade.value)
  
  sec.stats=data.frame("Total trade in 2018 (USD bn)"=round(total.sec.trade/1000000000,2),
                       "Share of 2018 world trade"=round(total.sec.trade/world.trade,4))
 
  write.xlsx(sec.stats, file=paste0(s.path,"Trade statistics for 2018 in sector ",sct,".xlsx"),row.names=F, sheetName = "sector trade stats")
  
  
}

