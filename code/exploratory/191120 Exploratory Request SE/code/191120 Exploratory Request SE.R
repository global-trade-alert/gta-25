rm(list=ls())

# Request: Could you please identify the 10 biggest sectors by GDP/sales, employment, and trade (2017 data)?
# With respect to the former 2 criteria does that outfit https://data.wiiw.ac.at/ or UNIDO have this data? Also, please identify from the 2017 trade data the five largest sectors exported by the LDCs."
# From the above, exclude resources and raw materials in the trade figures, please.

# Can you please expand the top 10 to the top 25 but only for the GDP and employment numbers?

library(gtalibrary)
library(stringr)
library(bit64)
library(openxlsx)
library(splitstackshape)

wdpath = '0 report production/GTA 25/code/exploratory/191120 Exploratory Request SE/'
output.path = "0 report production/GTA 25/code/exploratory/191120 Exploratory Request SE/results/"
data.path = "0 report production/GTA 25/code/exploratory/191120 Exploratory Request SE/data/" 

gta_setwd()


# trade stats -------------------------------------------------------------

# load("data/comtrade/comtrade 2017 no corrections.RData")
# unique(trade.data$vintage)
# trade.data=subset(trade.data, Year==2017)

gta_trade_value_bilateral(trade.data = 2017)
trade.data <- trade.base.bilateral

#exclude resources and raw materials
# trade.data$hs6=NA
# trade.data$hs6[which(nchar(trade.data$Tariff.line)%%2==1)]=str_sub(trade.data$Tariff.line[which(nchar(trade.data$Tariff.line)%%2==1)], 1,5)
# trade.data$hs6[which(nchar(trade.data$Tariff.line)%%2==0)]=str_sub(trade.data$Tariff.line[which(nchar(trade.data$Tariff.line)%%2==0)], 1,6)
# trade.data$hs6=as.numeric(trade.data$hs6)
trade.data=subset(trade.data, !hs6 %in% hs.codes$hs.code[hs.codes$is.raw.material==T|hs.codes$is.intermediate==T])

#remove residuals
trade.data=subset(trade.data, hs6!=999999)

# Partner is exporter since data is import data 
trade.data=aggregate(trade.value~hs6+a.un, trade.data, sum)

# match hs codes to cpc sectors 
trade.data$cpc=plyr::mapvalues(trade.data$hs6, cpc.to.hs$hs, cpc.to.hs$cpc)

# ADD LEADING ZERO TO CPC AND CUT FOR digit level 2
trade.data$cpc <- as.character(trade.data$cpc)
trade.data$cpc <- sprintf("%03s",trade.data$cpc) 
trade.data$cpc <- substr(trade.data$cpc,1,2)


# remove oil altogether
trade.data=subset(trade.data, cpc!=12)


# LARGEST EXPORT SECTORS
exp.sectors=merge(aggregate(trade.value~cpc,trade.data,sum), subset(cpc.names, cpc.digit.level==2), by='cpc')

# LARGEST EXPORT SECTORS FOR LDC
trade.ldc=subset(trade.data, a.un %in% country.names$un_code[country.names$is.ldc==T])
ldc.exp.sectors=merge(aggregate(trade.value~cpc,trade.ldc,sum), subset(cpc.names, cpc.digit.level==2), by='cpc')

world.exp.sectors=exp.sectors[order(exp.sectors$trade.value, decreasing=T),]
world.exp.sectors[1:10,]
ldc.exp.sectors=ldc.exp.sectors[order(ldc.exp.sectors$trade.value, decreasing=T),]
ldc.exp.sectors[1:5,]

write.xlsx(list(world.exp.sectors=world.exp.sectors[1:10,c('cpc','trade.value','cpc.name')],
                ldc.exp.sectors=ldc.exp.sectors[1:10,c('cpc','trade.value','cpc.name')]), 
           paste0(output.path, 'Largest sectors in trade 2017.xlsx'))


# employment and GDP/sales --------------------------------------------------------------

# IMPORT WIOD SET
wiod <- read.xlsx(paste0(wdpath,"data/WIOD_SEA_Nov16.xlsx"),sheet = 2)
wiod <- wiod[,c("country","variable","code","2014")]

# REMOVE ROWS WITH CODE %in% c(ABC) - BECAUSE THESE ARE NOT NEEDED
# REMOVE CODE = R_S ROWS, R_S DOES NOT SEEM TO BE A VALID ISIC CODE
wiod <- subset(wiod, ! code %in% c(LETTERS,"R_S"))

# SUBSET TO VARIABLES EMPE AND GO
wiod <- subset(wiod, variable %in% c("EMPE","VA"))
row.names(wiod) <- NULL

# TRANSFORM code COLUMN TO PROPER ISIC CODES
wiod$isic.short <- gsub(paste0(LETTERS, collapse="|"),"",wiod$code)
wiod$isic.short <- gsub("_","-",wiod$isic.short)
setnames(wiod, "2014","value")

# EXPAND ROWS WITH CODES OF THE TYPE "37-39"
wiod.new <- wiod[NULL,]
for(i in 1:nrow(wiod)) {
  if(grepl("-", wiod$isic.short[i])) {
    range <- as.numeric(strsplit(wiod$isic.short[i],"-")[[1]])
    temp <- data.frame(country = wiod$country[i],
                       variable = wiod$variable[i],
                       code = wiod$code[i],
                       value = wiod$value[i],
                       isic.short = seq(range[1],range[2],1))
    wiod.new <- rbind(wiod.new,temp)
    rm(temp)
  } else {
    wiod.new <- rbind(wiod.new, wiod[i,])
  }
}

# SOME OF THE WIOD VALUES ARE GROUPED BY SEVERAL ISIC CODES (e.g. "C10-C12" = xx$)
# DIVIDING THE VALUES HERE BY THE NUMBER OF INCLUDED ISIC CODES FOR THAT VALUE
wiod.new.freq <- as.data.frame(paste0(wiod.new$country,".",wiod.new$variable,".",wiod.new$code))
wiod.new.freq <- as.data.frame(table(wiod.new.freq))
wiod.new.freq <- cSplit(wiod.new.freq, "wiod.new.freq",".")
names(wiod.new.freq) <- c("freq","country","variable","code")
wiod.new <- merge(wiod.new, wiod.new.freq, by=c("country","variable","code"))
wiod.new$value.new <- wiod.new$value/wiod.new$freq
wiod.new$freq <- NULL

# LOAD ISIC AND CPC CONVERSION TABLE
cpc.conversion <- read.csv("definitions/hs-to-sic/isic4 to cpc21.csv",sep=";")

# ADD LEADING ZEROS
cpc.conversion$isic4 <- as.character(cpc.conversion$isic4)
cpc.conversion$isic4 <- sprintf("%04s",cpc.conversion$isic4) 
cpc.conversion$isic4 <- substr(cpc.conversion$isic4,1,2)

cpc.conversion$cpc21 <- as.character(cpc.conversion$cpc21)
cpc.conversion$cpc21 <- sprintf("%05s",cpc.conversion$cpc21) 
cpc.conversion$cpc21 <- substr(cpc.conversion$cpc21,1,2)

# REMOVE DUPLICATED ROWS
cpc.conversion <- unique(cpc.conversion)

# MERGE WITH WIOD DATA
wiod.new <- merge(wiod.new, cpc.conversion, by.x = "isic.short", by.y = "isic4")

# ACCOUNTING FOR 1:n RELATIONSHIP BETWEEN CPC AND ISIC CODES
wiod.new.freq <- as.data.frame(paste0(wiod.new$country,".",wiod.new$variable,".",wiod.new$isic.short,".",wiod.new$code))
wiod.new.freq <- as.data.frame(table(wiod.new.freq))
wiod.new.freq <- cSplit(wiod.new.freq, "wiod.new.freq",".")
names(wiod.new.freq) <- c("freq","country","variable","isic.short","code")
wiod.new.freq$isic.short <- as.character(wiod.new.freq$isic.short)
wiod.new.freq$isic.short <- sprintf("%02s",wiod.new.freq$isic.short) 
wiod.new.freq$isic.short <- substr(wiod.new.freq$isic.short,1,2)
wiod.new <- merge(wiod.new, wiod.new.freq, by=c("country","variable","isic.short","code"))
wiod.new$value.new.2 <- wiod.new$value.new/wiod.new$freq

# CURRENCIES ARE LOCAL CURRENCIES (AS PER DESCRIPTION OF WIOD) CONVERT TO US DOLLAR
library("WDI")
countries <- gtalibrary::country.names

# CURRENCY IN THE YEAR 2016
year = 2016
xchange <- WDI(indicator="DPANUSSPB", start=year, end=year, extra=T)

# ADD HONG KONG ISO3
xchange$iso3c[xchange$country == "Hong Kong China"] <- "HKG"

xchange=subset(xchange, is.na(DPANUSSPB)==F & year == year & iso3c %in% unique(wiod.new$country))[,c("iso3c","DPANUSSPB","year")]

# Load taiwanese dollar and south korean exchange rate separately
korea <- read.csv(paste0(data.path,"exchange rates oecd 2016.csv"))
xchange <- rbind(xchange, data.frame(iso3c = "KOR",
                                     DPANUSSPB = korea$Value[korea$LOCATION == "KOR"],
                                     year = 2016))
twnd <- read.csv(paste0(data.path,"US per TWD xchange rate 2016.csv"),sep=";")
twnd$X0.029912 <- 1/twnd$X0.029912
xchange <- rbind(xchange, data.frame(iso3c = "TWN",
                                     DPANUSSPB = mean(twnd$X0.029912),
                                     year = 2016))

# MERGE XCHANGE RATES WITH WIOD.NEW
wiod.new <- merge(wiod.new, xchange, by.x="country",by.y="iso3c")

# ONLY UPDATE VALUES FOR "VA" AS EMPLOYEES MUSTN'T BE UPDATED
wiod.new$usd.value <- wiod.new$value.new.2
wiod.new$usd.value[wiod.new$variable=="VA"] <- (wiod.new$value.new.2[wiod.new$variable=="VA"]/wiod.new$DPANUSSPB[wiod.new$variable=="VA"])*1000000

# AGGREGATE RESULTS FOR CPC SECTORS AND GO/EMPE
wiod.new.all <- aggregate(usd.value ~ variable + cpc21, wiod.new, function(x) sum(x))
# wiod.new.ldc <- aggregate(usd.value ~ variable + cpc21, subset(wiod.new, country %in% subset(countries, is.ldc == T)$iso_code), function(x) sum(x))

# ADD CPC NAMES
wiod.new.all <- unique(merge(wiod.new.all, subset(cpc.names, cpc.digit.level == 2)[,c("cpc","cpc.name")], by.x="cpc21", by.y = "cpc"))

# ARE LDC COUNTRIES EVEN INCLUDED IN THE WIOD SET?
wiod.cty <- unique(wiod.new$country)
length(wiod.cty[wiod.cty %in% subset(countries, is.ldc == T)$iso_code])
# LDC COUNTRIES NOT INCLUDED IN WIOD SET

# GET TOP 10 FOR EMPLOYMENT (NUMBER OF EMPLOYEES)
wiod.empe <- subset(wiod.new.all, variable == "EMPE")
wiod.empe <- wiod.empe[with(wiod.empe, order(-usd.value)),]
row.names(wiod.empe) <- NULL
wiod.empe <- wiod.empe[c(1:25),]
wiod.empe$usd.value <- wiod.empe$usd.value*1000

names(wiod.empe) <- c("CPC","var","Number of Employees","CPC Name")
wiod.empe <- wiod.empe[,c("CPC","Number of Employees","CPC Name")]
write.xlsx(wiod.empe, file=paste0(output.path, "Largest sectors in employment 2014.xlsx"),sheetName="Number of employees", row.names=F)

# GET TOP 10 FOR GDP
wiod.gdp <- subset(wiod.new.all, variable == "VA")
wiod.gdp <- wiod.gdp[with(wiod.gdp, order(-usd.value)),]
row.names(wiod.gdp) <- NULL
wiod.gdp <- wiod.gdp[c(1:25),]

names(wiod.gdp) <- c("CPC","var","GDP","CPC Name")
wiod.gdp <- wiod.gdp[,c("CPC","GDP","CPC Name")]
write.xlsx(wiod.gdp, file=paste0(output.path, "Largest sectors in gdp 2014.xlsx"),sheetName="GDP", row.names=F)



