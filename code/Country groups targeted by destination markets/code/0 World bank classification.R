rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
source('0 report production/GTA 25/help files/GTA 25 cutoff and definitions.R')

this.chapter=c(paste0("Sectoral chapters - Sector ",paste0(sectors)))

wdpath = "0 dev/gta-25-pb/code/Country groups targeted by destination markets/"
data.path = paste0(wdpath,"data/")


# CREATE WORLD BANK LOWER AND UPPER MIDDLE INCOME COUNTRY SETS

# load wb classification set
wbclass <- xlsx::read.xlsx(paste0(data.path,"world bank classification.xls"),sheetIndex=1,startRow = 5,header = T)
wbclass <- wbclass[-c(1),]
wbclass <- wbclass[1:218,]
wbclass$Code <- as.character(wbclass$Code)
wbclass <- merge(wbclass, gtalibrary::country.names[,c("un_code","iso_code")], by.x="Code", by.y="iso_code")

# Subset for lower and upper middle income countries
low.mid.ctry <- subset(wbclass, Income.group == "Lower middle income")$un_code
upp.mid.ctry <- subset(wbclass, Income.group == "Upper middle income")$un_code

low.mid.ctry <- c(24, 50, 68, 64, 384, 120, 178, 174, 132, 262, 818, 583, 288, 340, 360, 699, 404, 417, 116, 296, 418, 426, 504, 498, 104, 496, 478, 566, 558, 586, 608, 598, 275, 729, 686, 90, 222, 678, 748, 626, 788, 804, 860, 704, 548, 894, 716)
upp.mid.ctry <- c(8, 32, 51, 16, 31, 100, 70, 112, 84, 76, 72, 156, 170, 188, 192, 212, 214, 12, 218, 242, 266, 268, 226, 308, 320, 328, 364, 368, 388, 400, 398, 422, 434, 662, 144, 462, 484, 584, 807, 499, 480, 458, 516, 520, 604, 600, 642, 643, 688, 740, 764, 795, 776, 792, 798, 670, 862, 882, 710)

