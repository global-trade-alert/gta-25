rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(plyr)
library(data.table)



gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
chapter.folders=gta25_setup(internal.name = 'front matter',
                            in.dev = F,
                            author=NULL,
                            wipe.data = T,
                            wipe.figs = T)
data.path=chapter.folders$data.path
figure.path=chapter.folders$figure.path

# SE requests two maps
# The first map would show the number of times each country's commercial interests have been hit from 1 Jan 2017 to 15 Nov 2019. 




# The second map would show the share of a country's exports that are affected by harmful foreign measures implemented from 1 Jan 2017 to 15 Nov 2019. 



