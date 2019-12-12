rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(xlsx)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')


directories=gta25_setup(internal.name="Annual trade variation",
                        in.dev=F,
                        author=NULL,
                        wipe.data=F,
                        wipe.figs=T)

data.path = directories$data.path
output.path = directories$figure.path



## setting figure paths
first.sector.chapter=5
sector.path=paste0(str_extract(output.path,"^.+?figures/"),paste0(first.sector.chapter:(first.sector.chapter+length(sectors)-1), " - Sector ", sectors,"/"))
wipe.sector.path=F



gta_colour_palette()

# Figure 1 create graph ------------------------------------------------------

# For the years 2005 to 2018, setting the value of both series to 100 in 2007, 
# please produce a line chart showing the total global value of trade in USD 
# and the total value of the relevant sector's trade worldwide. In total, then, 
# there should be a chart for each sector showing how much the total value of 
# sectoral trade varied compared to that of world trade.

load(paste0(data.path,"Trade per sector indexed 2007.Rdata"))
load(paste0(data.path,"Trade global indexed 2007.Rdata"))

sct.trade.data$type="sectoral"
global.trade$type="global"
global.trade$cpc=0

fig1.data <- rbind(sct.trade.data, global.trade)

fig1.create <- function(sct) {
  fig1 <- ggplot(data=subset(fig1.data, cpc %in% c(sct,0)))+
    geom_rect(data=data.frame(), aes(xmin=2017, xmax=Inf, ymin=-Inf, ymax=Inf), fill=pop.shade, alpha=0.3)+
    geom_text(aes(x=2017, y=0.6, label="Populist\n era"), hjust=-0.1, vjust=1.4, color = pop.text, lineheight = 1)+
    geom_text(aes(x=-Inf, y=Inf, label="Pre-populist era"), hjust=-0.1, vjust=2, color = gta_colour$grey[1], lineheight = 1)+
    geom_line(aes(x=Period, y=index.2007, colour=type), size = 1)+
    # geom_point(aes(x=2019-0.2, y=glo.cov.harmful),size=3, colour=gta_colour$red[1])+
    # geom_point(aes(x=2019-0.2, y=glo.cov.liberalising),size=3, colour=gta_colour$green[1])+
    gta_plot_wrapper(data=subset(fig1.data, cpc == sct),
                     data.x="Period",
                     data.y="index.2007",
                     x.bottom.name = "Year",
                     x.bottom.breaks = seq(2005,2018,1),
                     y.left.name = "Amount of sectoral and global trade indexed at 100 in 2007",
                     y.left.breaks = c(0.5,1,1.5),
                     y.left.labels = scales::percent(c(0.5,1,1.5)),
                     y.right.enable = F,
                     y.left.limits = c(0.5,round(max(fig1.data$index.2007,1)+.1)),
                     colour.legend.title = "Trade included",
                     colour.palette = c(gta_colour$blue[1],gta_colour$blue[4]),
                     colour.labels = c("Global",paste0("This sector")),
                     colour.legend.col = 2)+
    gta_theme()
  
  fig1
  return(fig1)
}


# Create panels per sector ------------------------------------------------

for (sct in sectors) {
  
  
  s.path=sector.path[grepl(paste0(sct,"/$"),sector.path)]
  
  dir.create(file.path(s.path), showWarnings = FALSE)
  
  if(wipe.sector.path){
    wipe.all= list.files(s.path, include.dirs = F, full.names = T, recursive = T)
    file.remove(wipe.all)
    rm(wipe.all)
  }
  
  
  fig1 <- fig1.create(sct)
  
  gta_plot_saver(plot = fig1,
                 path = s.path,
                 name = paste0("Figure 1 - Sector ",sct),
                 cairo_ps = T,
                 width = 21)
  
  write.xlsx(subset(fig1.data, cpc %in% c(sct,0)), file=paste0(s.path,"Table for Figure 1.xlsx"),row.names=F, sheetName = "Trade data")
  
  
}

