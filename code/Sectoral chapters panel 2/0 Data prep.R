rm(list=ls())

# Req: Theese are 4 graphs placed on the same page. Create these graphs for the following sectors: 49, 47, 43, 44, 46, 28, 29

library(gtalibrary)
library(stringr)
library(tidyverse)
library(plyr)
library(data.table)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
chapter.folders=gta25_setup(internal.name = 'Sectoral chapters panel 2',
                            in.dev = F,
                            author='ks',
                            wipe.data = T,
                            wipe.figs = T)
data.path=chapter.folders$data.path
figure.path=chapter.folders$figure.path




# Fig 5  ----------------------------------------------------------------
# Req: Chart 5: Scatter plot for all G20. Y-axis shows change in sectoral import share protected from 2017-2019. X-axis shows change in national import share from 2017-2019

gta_data_slicer()

in.force.15.nov=unique(subset(master.sliced, date.implemented >= as.Date('2017-01-01') & date.implemented <= as.Date('2019-11-15') & 
                                (is.na(date.removed)|date.removed>=as.Date('2019-11-15')))$intervention.id)
neg.in.force.15.nov=setdiff(unique(master.sliced$intervention.id),in.force.15.nov)

#y-axis
sct.cov.harmful <- data.frame()
for (sct in sectors) {
  codes <- gta_cpc_code_expand(codes = sct)
  gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                     cpc.sectors = codes,
                     keep.cpc = T,
                     affected.flows = c("inward"),
                     importers = 'G20',
                     keep.importers = T,
                     group.importers = F,
                     coverage.period = c(2016,2019),
                     implementation.period = c('2017-01-01','2019-11-15'),
                     intervention.ids = neg.in.force.15.nov, # workaround for removed interventions
                     keep.interventions = F)
  sct.cov.harmful <- rbind(sct.cov.harmful, data.frame(sector=sct,
                                                       imp.cty=trade.coverage.estimates$`Importing country`,
                                                       cov.2016=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2016`),
                                                       cov.2017=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2017`),
                                                       cov.2018=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2018`),
                                                       cov.2019=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2019`)
  ))
  
  rm(trade.coverage.estimates)
}

sct.cov.harmful$cov.change=sct.cov.harmful$cov.2019
sct.cov.harmful$imp.cty=mapvalues(sct.cov.harmful$imp.cty,country.names$name,country.names$un_code)

save(sct.cov.harmful, file=paste0(data.path,"/Sector coverages harmful.Rdata"))


#x-axis
gta_trade_value_bilateral(importing.country='G20',
                          keep.importer=T,
                          trade.data = "2016")

trade.base.bilateral$cpc=mapvalues(trade.base.bilateral$hs6,cpc.to.hs$hs,cpc.to.hs$cpc)
#map back to 2 digit cpc since Simon's provided sectors are 2 digit
for(cpc.two.digit in sectors){
  expanded.codes=gta_cpc_code_expand(cpc.two.digit)
  trade.base.bilateral$cpc=mapvalues(trade.base.bilateral$cpc,expanded.codes,rep(cpc.two.digit, length(expanded.codes)))
}


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

load(paste0(data.path,"/Sector coverages harmful.Rdata"))
data.fig5=subset(merge(sct.trade.base,sct.cov.harmful,by.x=c('i.un','cpc'),by.y=c('imp.cty','sector'), all = T), 
                 select=c('i.un','cpc','sct.share','cov.change'))
data.fig5$cov.change[is.na(data.fig5$cov.change)] = 0 
setnames(data.fig5,'i.un','cty')
data.fig5$cpc.name=mapvalues(data.fig5$cpc,subset(cpc.names, cpc.digit.level==2)$cpc,as.character(subset(cpc.names, cpc.digit.level==2)$cpc.name))
save(data.fig5, file=paste0(data.path,'/fig 5.Rdata'))

# Fig 6  ------------------------------------------------------------------

# Req: Chart 6: Scatter plot for all G20. Y-axis shows change in sectoral import share protected from 2017-2019. X-axis shows the change in the exchange rate of the national currency against USD from 2017-2019
library(IMFData)
databaseID <- "IFS"

IFS.available.codes <- DataStructureMethod(databaseID)

g20.ifs=subset(country.names,is.g20==T)$name
g20.ifs=mapvalues(g20.ifs,c('Republic of Korea','United States of America','Russia'),c("Korea, Republic of","United States",'Russian Federation'))
g20.ifs=mapvalues(g20.ifs,IFS.available.codes$CL_AREA_IFS$CodeText,IFS.available.codes$CL_AREA_IFS$CodeValue)

startdate = '2016-01-01'
enddate = '2020-01-01'

queryfilter <- list(CL_FREA = "M", CL_AREA_IFS = c(g20.ifs, "U2") , CL_INDICATOR_IFS = c("ENDA_XDC_USD_RATE"))
currency.base <- CompactDataMethod(databaseID, queryfilter, startdate, enddate,
                                   checkquery=F)[,2:6]

# compute avg currency rate conversion vs usd in 2016 / 2019 (until and including october)
currency.base$`2016`=NA
for (i in 1:length(currency.base$Obs)) currency.base$`2016`[i] = mean(as.numeric(currency.base$Obs[[i]][str_sub(as.character(currency.base$Obs[[i]]$`@TIME_PERIOD`), 1,4)=='2016',]$`@OBS_VALUE`))
currency.base$`2019`=NA
for (i in 1:length(currency.base$Obs)) currency.base$`2019`[i] = mean(as.numeric(currency.base$Obs[[i]][str_sub(as.character(currency.base$Obs[[i]]$`@TIME_PERIOD`), 1,4)=='2019',]$`@OBS_VALUE`))

## These are LCU/USD exchange rates (e.g. FX-EUR is .9)
## this being negative implies that the currency rose against USD
currency.base$rel.change=currency.base$`2019`/currency.base$`2016`

currency.base=subset(currency.base, select=c('@REF_AREA','rel.change'))
setnames(currency.base, names(currency.base)[1],c('cty'))
currency.base=rbind(subset(currency.base, cty!='U2'), 
                    data.frame(cty=g20.ifs[!g20.ifs %in% currency.base$cty],
                               rel.change=subset(currency.base, cty=='U2')$rel.change))
currency.base$cty.name = mapvalues(currency.base$cty, IFS.available.codes$CL_AREA_IFS$CodeValue, IFS.available.codes$CL_AREA_IFS$CodeText)
#convert back to gta cty names
currency.base$cty.name = mapvalues(currency.base$cty.name,c("Korea, Republic of","United States",'Russian Federation'),c('Republic of Korea','United States of America','Russia'))
currency.base=subset(currency.base, select=c('cty.name','rel.change'))
currency.base$cty.name = mapvalues(currency.base$cty.name,country.names$name,country.names$un_code)
currency.base$curr.rel.change=currency.base$rel.change ; currency.base$rel.change=NULL 

load(paste0(data.path,"Sector coverages harmful.Rdata"))


data.fig6=merge(currency.base, subset(sct.trade.base, select = c("i.un", "cpc")), by.x = c("cty.name"), by.y = c("i.un"), all = T) #we need to add those observations with no measures
data.fig6=subset(merge(sct.cov.harmful,data.fig6,by.x=c('imp.cty', "sector"), by.y=c('cty.name', "cpc"), all = T),
                 select=c('imp.cty','cov.change','sector','curr.rel.change'))
setnames(data.fig6, "imp.cty", "cty.name")
data.fig6$sector.name=mapvalues(data.fig6$sector,subset(cpc.names, cpc.digit.level==2)$cpc,as.character(subset(cpc.names, cpc.digit.level==2)$cpc.name))
data.fig6[is.na(data.fig6)]=0

save(data.fig6, file=paste0(data.path,'/fig 6.Rdata'))

# chart 7  ----------------------------------------------------------------

#Req: Chart 7: Scatter plot for G20. Y-axis shows share of national sectoral import share affected by non-tariff measures. 
# the X axis is the sectoral trade balance divided by total sectoral trade. Let X be the total exports of a country in a given sector,
#let Y be total imports of the same country in the same sector. Then the measure I have in mind is (X-Y)/(X+Y). Note that this measure can be negative but the values will always lie between -1 and +1.

#KS: x-axis was made with 2016 data

#x-axis
gta_trade_value_bilateral(trade.data ='2016')
trade.base.bilateral$cpc=mapvalues(trade.base.bilateral$hs6,cpc.to.hs$hs,cpc.to.hs$cpc)
#map back from 3 digit cpc to 2 cpc since Simon's communicated sectors are 2 digit
for(cpc.two.digit in sectors){
  expanded.codes=gta_cpc_code_expand(cpc.two.digit)
  trade.base.bilateral$cpc=mapvalues(trade.base.bilateral$cpc,expanded.codes,rep(cpc.two.digit, length(expanded.codes)))
}

importer.base=subset(aggregate(trade.value~cpc+i.un,trade.base.bilateral,sum), i.un %in% subset(country.names, is.g20 ==T)$un_code,
                     select=c('trade.value','cpc','i.un'))
importer.base$state='import'
setnames(importer.base,'i.un','cty')
exporter.base=subset(aggregate(trade.value~cpc+a.un,trade.base.bilateral,sum), a.un %in% subset(country.names, is.g20 ==T)$un_code,
                     select=c('trade.value','cpc','a.un'))
exporter.base$state='export'
setnames(exporter.base,'a.un','cty')

base=merge(expand.grid(cpc=c(sectors,NA),
                       cty=subset(country.names, is.g20 ==T)$un_code,
                       state=c('import','export')),
           rbind(importer.base,exporter.base), by = c('cpc','cty','state'), all.x = T, all.y = F)
base$trade.value[is.na(base$trade.value)]=0
base=spread(base, state, trade.value)
base$sect.trade.share=(base$export-base$import)/(base$export+base$import)

load(paste0(data.path,"/Sector coverages harmful.Rdata"))
data.fig7=merge(sct.cov.harmful, 
                base, by.x = c('sector','imp.cty'), by.y=c('cpc','cty'), all = T)
data.fig7[is.na(data.fig7)]=0
data.fig7=data.fig7[data.fig7$sector!=0,]
data.fig7$change.sct.imp.share=data.fig7$cov.2019-data.fig7$cov.2017
data.fig7=subset(data.fig7, select=c('sector','imp.cty','cov.change','sect.trade.share'))
setnames(data.fig7,'imp.cty','cty')
data.fig7$sector.name=mapvalues(data.fig7$sector,subset(cpc.names, cpc.digit.level==2)$cpc,as.character(subset(cpc.names, cpc.digit.level==2)$cpc.name))

save(data.fig7, file=paste0(data.path,'fig 7.Rdata'))

# chart 8  ----------------------------------------------------------------
#Chart 8: Scatter plot for G20. Y-axis same as graph 5 and 6. X-axis shows the share of sectoral exports that benefit from incentives.

sct.incentives <- data.frame()


for (sct in sectors) {
  codes <- gta_cpc_code_expand(codes = sct)
  gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                     cpc.sectors = codes,
                     keep.cpc = T,
                     implementers = 'G20',
                     keep.implementer = T,
                     group.implementers = F,
                     implementer.trade = "export",
                     coverage.period = c(2016,2019),
                     affected.flows = 'outward subsidy',
                     mast.chapters = 'P',
                     keep.mast = T,
                     implementation.period = c('2017-01-01','2019-11-15'),
                     intervention.ids = neg.in.force.15.nov, # workaround for removed interventions
                     keep.interventions = F)
  
  sct.incentives <- rbind(sct.incentives, data.frame(sector=sct,
                                                     exp.cty=trade.coverage.estimates$`Exporting country`,
                                                     cov.2016=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2016`),
                                                     cov.2017=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2017`),
                                                     cov.2018=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2018`),
                                                     cov.2019=as.numeric(trade.coverage.estimates$`Trade coverage estimate for 2019`)
  ))
  
  rm(trade.coverage.estimates)
}

sct.incentives$exp.cty=mapvalues(sct.incentives$exp.cty,country.names$name,country.names$un_code)
sct.incentives$incentives.change=sct.incentives$cov.2019


data.fig8=merge(sct.incentives, 
                merge(sct.cov.harmful,
                      subset(cty.sct.trade, cpc %in% sectors)[,c("cpc","i.un")], 
                      by.x=c("imp.cty","sector"), by.y=c("i.un","cpc"), all.y=T), 
                by.x = c('sector','exp.cty'), by.y=c('sector','imp.cty'), all=T)
data.fig8=subset(data.fig8, select=c('sector','exp.cty','incentives.change','cov.change'))
data.fig8[is.na(data.fig8)]=0

setnames(data.fig8,'exp.cty','cty')
data.fig8$sector.name=mapvalues(data.fig8$sector,subset(cpc.names, cpc.digit.level==2)$cpc,as.character(subset(cpc.names, cpc.digit.level==2)$cpc.name))

save(data.fig8, file=paste0(data.path,'fig 8.Rdata'))
