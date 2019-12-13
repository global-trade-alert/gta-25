rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)


gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')


directories=gta25_setup(internal.name="business concerns",
                        in.dev=F,
                        author=NULL,
                        wipe.data=F,
                        wipe.figs=F)

data.path = directories$data.path

##### Import and prepare business confidence time series
confidence.data = read.csv(file = '0 report production/GTA 25/help files/Business confidence.csv')
confidence.data$Flag.Codes <- NULL
confidence.data$INDICATOR <- NULL
confidence.data$SUBJECT <- NULL
confidence.data$MEASURE <- NULL
confidence.data$FREQUENCY <- NULL
names(confidence.data) = c("jurisdiction", "time", "value")

## Transform time variable into year-month variable
confidence.data$time = as.character(confidence.data$time)
confidence.data$date = as.Date(paste(substr(confidence.data$time,1,4),
                                     substr(confidence.data$time,6,7),
                                     "15",sep = "-"))
confidence.data$time = NULL

save(confidence.data, file=paste0(data.path,'confidence.data.Rdata'))


confidence.data$jurisdiction = as.character(confidence.data$jurisdiction)
ggplot(confidence.data, aes(x=date, y=value, col=jurisdiction)) +
  geom_line()



ggplot(subset(confidence.data, jurisdiction=="CHN"), aes(x=date, y=value)) +
  geom_line()


confidence.data$time = NULL
test = reshape(confidence.data, idvar = "date", timevar = "jurisdiction", direction = "wide")
ggplot(test, aes(x=date, y=value)) +
  geom_line()





