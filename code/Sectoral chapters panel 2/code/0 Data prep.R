rm(list=ls())

# Req: These are 4 graphs placed on the same page. Create these graphs for the following sectors: 49, 47, 43, 44, 46, 28, 29

library(gtalibrary)
library(stringr)
library(tidyverse)
library(plyr)
library(data.table)


gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
source('0 report production/GTA 25/help files/GTA 25 cutoff and definitions.R')

this.chapter=c(paste0("Sectoral chapters - Sector ",paste0(sectors)))

wdpath = "0 dev/gta-25/code/Sectoral chapters panel 1/"
data.path = paste0(wdpath,"data/")


# Fig 5  ----------------------------------------------------------------
# Req: Chart 5: Scatter plot for all G20. Y-axis shows change in sectoral import share protected from 2017-2019. X-axis shows change in national import share from 2017-2019


#y-axis
sct.cov.harmful <- data.frame()
for (sct in sectors) {
  codes <- gta_cpc_code_expand(codes = sct)
  gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                     cpc.sectors = codes,
                     keep.cpc = T,
                     importers = 'G20',
                     keep.importers = T,
                     group.importers = F, 
                     coverage.period = c(2016,2019))
  sct.cov.harmful <- rbind(sct.cov.harmful, data.frame(sector=sct,
                                                       imp.cty=trade.coverage.estimates$`Importing country`,
                                                       cov.2016=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2016`),
                                                       cov.2017=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2017`),
                                                       cov.2018=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2018`),
                                                       cov.2019=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2019`)
  ))
                                                       
  rm(trade.coverage.estimates)
}



save(sct.cov.harmful, file=paste0(data.path,"Sector coverages harmful.Rdata"))

#x-axis
gta_trade_value_bilateral(importing.country='G20',
                          keep.importer=T, 
                          trade.data = "2016")

trade.base.bilateral$a.un = NULL
trade.base.bilateral$cpc=mapvalues(trade.base.bilateral$hs6,cpc.to.hs$hs,cpc.to.hs$cpc)
cty.sct.trade=aggregate(trade.value~i.un+cpc ,trade.base.bilateral, sum)
sct.trade.base=expand.grid(i.un=country.names[country.names$is.g20==T,]$un_code,
                           cpc=sectors, 
                           trade.value=NA)

for(sct in sectors){
  temp=subset(cty.sct.trade, cpc==sct)
  if(nrow(temp)>0){
    sct.trade.base=merge(sct.trade.base,aggregate(trade.value~i.un+cpc,temp,sum),by=c('i.un','cpc'), all = T)
    sct.trade.base[is.na(sct.trade.base$trade.value.x),]$trade.value.x=sct.trade.base[is.na(sct.trade.base$trade.value.x),]$trade.value.y
    sct.trade.base$trade.value=NULL
    sct.trade.base$trade.value.y=NULL
  }
}  

sct.trade.base$sct.trade=sct.trade.base$trade.value.x
sct.trade.base$trade.value.x=NULL

sct.trade.base$sct.trade[is.na(sct.trade.base$sct.trade)]=0

sct.trade.base=merge(sct.trade.base, data.frame(i.un=aggregate(trade.value~i.un,cty.sct.trade,sum)$i.un,
                                      national.trade=aggregate(trade.value~i.un,cty.sct.trade,sum)$trade.value
                                      ), by='i.un')
sct.trade.base$sct.share=sct.trade.base$sct.trade/sct.trade.base$national.trade


load(paste0(data.path,"Sector coverages harmful.Rdata"))


# Fig 6  ------------------------------------------------------------------

# Req: Chart 6: Scatter plot for all G20. Y-axis shows change in sectoral import share protected from 2017-2019. X-axis shows the change in the exchange rate of the national currency against USD from 2017-2019
library(IMFData)
databaseID <- "IFS"

IFS.available.codes <- DataStructureMethod(databaseID)

g20.ifs=subset(country.names,is.g20==T)$name
g20.ifs=mapvalues(g20.ifs,c('Republic of Korea','United States of America','Russia'),c("Korea, Republic of","United States",'Russian Federation'))
g20.ifs=mapvalues(g20.ifs,IFS.available.codes$CL_AREA_IFS$CodeText,IFS.available.codes$CL_AREA_IFS$CodeValue)

startdate = '2017-01-01'
enddate = '2020-01-01'

queryfilter <- list(CL_FREA = "M", CL_AREA_IFS = c(g20.ifs, "U2") , CL_INDICATOR_IFS = c("ENDA_XDC_USD_RATE"))
currency.base <- CompactDataMethod(databaseID, queryfilter, startdate, enddate,
                                    checkquery=F)[,2:6]

# compute avg currency rate conversion vs usd in 2017 / 2019 (until and including october)
currency.base$`2017`=NA
for (i in 1:length(currency.base$Obs)) currency.base$`2017`[i] = mean(as.numeric(currency.base$Obs[[i]][str_sub(as.character(currency.base$Obs[[i]]$`@TIME_PERIOD`), 1,4)=='2017',]$`@OBS_VALUE`))
currency.base$`2019`=NA
for (i in 1:length(currency.base$Obs)) currency.base$`2019`[i] = mean(as.numeric(currency.base$Obs[[i]][str_sub(as.character(currency.base$Obs[[i]]$`@TIME_PERIOD`), 1,4)=='2019',]$`@OBS_VALUE`))

currency.base$rel.change=(currency.base$`2019`-currency.base$`2017`)/currency.base$`2017`

currency.base=subset(currency.base, select=c('@REF_AREA','rel.change'))
setnames(currency.base, names(currency.base)[1],c('cty'))
currency.base=rbind(subset(currency.base, cty!='U2'), 
               data.frame(cty=g20.ifs[!g20.ifs %in% currency.base$cty],
                          rel.change=subset(currency.base, cty=='U2')$rel.change))
currency.base$cty.name = mapvalues(currency.base$cty, IFS.available.codes$CL_AREA_IFS$CodeValue, IFS.available.codes$CL_AREA_IFS$CodeText)
#convert back to gta cty names
currency.base$cty.name = mapvalues(currency.base$cty.name,c("Korea, Republic of","United States",'Russian Federation'),c('Republic of Korea','United States of America','Russia'))
currency.base=subset(curency.base, select=c('cty.name','rel.change'))



# chart 8  ----------------------------------------------------------------
#Chart 8: Scatter plot for G20. Y-axis same as graph 5 and 6. X-axis shows the share of sectoral exports that benefit from incentives.
#s

sct.incentives <- data.frame()
for (sct in sectors) {
  codes <- gta_cpc_code_expand(codes = sct)
  gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                     cpc.sectors = codes,
                     keep.cpc = T,
                     importers = 'G20',
                     keep.importers = T,
                     group.importers = F, 
                     coverage.period = c(2016,2019))
  sct.cov.harmful <- rbind(sct.incentives, data.frame(sector=sct,
                                                       imp.cty=trade.coverage.estimates$`Importing country`,
                                                       cov.2016=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2016`),
                                                       cov.2017=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2017`),
                                                       cov.2018=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2018`),
                                                       cov.2019=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2019`)
  ))
  
  rm(trade.coverage.estimates)
}



save(sct.incentives, file=paste0(data.path,"Sector incentives harmful.Rdata"))

load(paste0(data.path,"Sector coverages harmful.Rdata"))
