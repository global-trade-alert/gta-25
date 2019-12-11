rm(list=ls())

## Req: Create bar chart with jumbos introduced in populist era

library(gtalibrary)
library(stringr)
library(tidyverse)
library(plyr)
library(data.table)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
chapter.folders=gta25_setup(internal.name = 'Jumbos in the populist era',
                            in.dev = F,
                            author=NULL,
                            wipe.data = T,
                            wipe.figs = F)
data.path=chapter.folders$data.path
figure.path=chapter.folders$figure.path

jumbo.data = xlsx::read.xlsx2(file = "0 report production/GTA 25/help files/trade per intervention_jumbos checked.xlsx", sheetName = "populist era")
jumbo.data$trade.value = as.numeric(as.character(jumbo.data$trade.value))
jumbo.data = subset(jumbo.data, is.na(trade.value)==F)
jumbo.data$date.implemented = as.Date(as.numeric(as.character(jumbo.data$date.implemented)), origin = "1970-01-01")
jumbo.data$date.implemented = as.character(jumbo.data$date.implemented)
jumbo.data$order = nrow(jumbo.data):1


save(jumbo.data, file = paste0(chapter.folders$data.path, "jumbo.data.Rdata"))

### SE: Calculate amount of trade affected by these jumbos overall, US & China

gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                   intervention.ids = jumbo.data$intervention.id,
                   keep.interventions = T,
                   affected.flows = c("inward", "outward","outward subsidy"),
                   coverage.period=c(2017,2019),
                   trade.data = 2016,
                   trade.statistic = "value")
#Overall: 2.64e+12	3.69E+12	3.03E+12

gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                   intervention.ids = subset(jumbo.data, implementing.jurisdiction=="United States of America")$intervention.id,
                   keep.interventions = T,
                   affected.flows = c("inward", "outward","outward subsidy"),
                   coverage.period=c(2017,2019),
                   trade.data = 2016,
                   trade.statistic = "value")
#USA:	63219931247	294237697282	541545362087

gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                   intervention.ids = subset(jumbo.data, implementing.jurisdiction=="China")$intervention.id,
                   keep.interventions = T,
                   affected.flows = c("inward", "outward","outward subsidy"),
                   coverage.period=c(2017,2019),
                   trade.data = 2016,
                   trade.statistic = "value")
#China: 433937520	370239107230	837360546774





# MAPS
jumbo.countries <- read.xlsx(paste0("0 report production/GTA 25/help files/trade per intervention_jumbos checked.xlsx"),sheetName ="populist era")
jumbo.countries <- subset(jumbo.countries, is.na(implementing.jurisdiction)==F)
unique1 <- unique(jumbo.countries$implementing.jurisdiction)
jumbo.countries <- as.data.frame(table(jumbo.countries$implementing.jurisdiction))
unique2 <- unique(jumbo.countries$Var1)
diff.unique <- unique1[! unique1 %in% unique2]
names(jumbo.countries) <- c("name","implemented")

countries <- gtalibrary::country.names
length(unique(jumbo.countries$name))
jumbo.countries <- merge(jumbo.countries, countries[,c("name","un_code")], by="name")

write.xlsx(jumbo.countries[,c("name","implemented")], file=paste0(figure.path,"Countries implementing jumbo measures.xlsx"),sheetName="implementers",row.names=F)

