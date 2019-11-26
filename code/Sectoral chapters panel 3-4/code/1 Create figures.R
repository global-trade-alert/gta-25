rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(scales)
library(xlsx)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
source('0 report production/GTA 25/help files/GTA 25 cutoff and definitions.R')

this.chapter=c(paste0("Sectoral chapters - Sector ",paste0(sectors)))

wdpath = "0 dev/gta-25/"
output.path = paste0(wdpath,"tables & figures/Sectoral chapters panel 3-4/")
data.path = paste0(wdpath,"code/Sectoral chapters panel 3-4/data/")

gta_colour_palette()
order.names <- data.frame(country = c("United States of America","China",c(g20.member.names[! g20.member.names %in% c("United States of America", "China")])), order = c(seq(1,19,1)))
plot.names <- data.frame(country = c("USA","China",c(g20.member.names[! g20.member.names %in% c("United States of America", "China","United Kingdom")]),"UK"), order = c(seq(1,19,1)))
blank.set <- data.frame(order.y = rep(seq(1,19,1),19), order.x = rep(1:19, each=19))
blank.set.middle <- data.frame(order.x=seq(1,19,1),order.y=seq(1,19,1))

# Figure 9 create graph ------------------------------------------------------

# Chart 9: Heat Map for all G20. Y-axis shows the importing nations, 
# X-axis shows the exporting nations. Place USA and China at beginning,
# (top left) and all other countries alphabetically after them. Graph 
# shows share of sectoral exports affected by trade distortions in force 
# today implemented by the importing nation. Show in shades of red.

load(paste0(data.path,"G20 sector exports coverages - harmful.Rdata"))

fig9.data <- sct.g20.harmful
fig9.data <- merge(fig9.data, order.names, by.x = "importer", by.y = "country")
fig9.data <- merge(fig9.data, order.names, by.x = "exporter", by.y = "country")
fig9.data <- fig9.data[with(fig9.data, order(order.x,order.y)),]
row.names(fig9.data) <- NULL

fig9.xlsx <- fig9.data[,c("importer","exporter","2019","sector")]
names(fig9.xlsx) <- c("importer","exporter","coverage","sector")
write.xlsx(fig9.xlsx, file=paste0(output.path,"Table for Figure 9.xlsx"),row.names=F, sheetName = "Coverages")

fig9.create <- function(sct) {
    
  fig9 <- ggplot(data=subset(fig9.data, sector==sct))+
    geom_tile(data=blank.set, aes(x=order.x, y=order.y), fill=gta_colour$red[4], color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(aes(x=order.y, y=order.x, fill=`2019`), color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(data=blank.set.middle, aes(x=order.x, y=order.y), fill="#FFFFFF", color="#FFFFFF", size=0.2, na.rm = F)+
    gta_theme(x.bottom.angle = 45)+
    scale_fill_gradientn(name="Percentage of bilateral exports \nfacing importer\'s trade distortions", 
                         colours = c(gta_colour$red[4], gta_colour$red[1]), values=c(0,0.25,0.50,0.75,1), 
                         breaks=c(0,0.25,0.5,0.75,1), labels=c("0%","25%","50%","75%", "100%"),
                         limits=c(0,1),
                         guide=guide_colorbar(barwidth=15, title.position = "top", hjust=1, label.hjust=0.3))+
    scale_y_continuous(breaks=seq(1,max(fig9.data$order.y),1), labels=plot.names$country, sec.axis = sec_axis(~., breaks=seq(1,max(fig9.data$order.y),1), labels=plot.names$country, name = "Importing country"))+
    scale_x_continuous(breaks=seq(1,max(fig9.data$order.x),1),labels=plot.names$country)+
    labs(x="Exporting countries",y="Importing countries")+
    theme(panel.background = element_blank(), 
          panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
          axis.text.x.bottom = element_text(hjust = 1)
    )
  
  return(fig9)
  }

# Figure 10 create graph ------------------------------------------------------

# Chart 10: Same as chart 9, but with liberalising measures.

load(paste0(data.path,"G20 sector exports coverages - liberalising.Rdata"))

fig10.data <- sct.g20.liberalising
fig10.data <- merge(fig10.data, order.names, by.x = "importer", by.y = "country")
fig10.data <- merge(fig10.data, order.names, by.x = "exporter", by.y = "country")
fig10.data <- fig10.data[with(fig10.data, order(order.x, order.y)),]
row.names(fig10.data) <- NULL

fig10.xlsx <- fig10.data[,c("importer","exporter","2019","sector")]
names(fig10.xlsx) <- c("importer","exporter","coverage","sector")
write.xlsx(fig10.xlsx, file=paste0(output.path,"Table for Figure 10.xlsx"),row.names=F, sheetName = "Coverages")

fig10.create <- function(sct) {
  
  fig10 <- ggplot(data=subset(fig10.data, sector==sct))+
    geom_tile(data=blank.set, aes(x=order.x, y=order.y), fill=gta_colour$green[4], color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(aes(x=order.y, y=order.x, fill=`2019`), color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(data=blank.set.middle, aes(x=order.x, y=order.y), fill="#FFFFFF", color="#FFFFFF", size=0.2, na.rm = F)+
    gta_theme(x.bottom.angle = 45)+
    scale_fill_gradientn(name="Percentage of bilateral exports \nprofiting of importer\'s trade remedies", 
                         colours = c(gta_colour$green[4], gta_colour$green[1]), values=c(0,0.25,0.50,0.75,1), 
                         breaks=c(0,0.25,0.5,0.75,1), labels=c("0%","25%","50%","75%", "100%"),
                         limits=c(0,1),
                         guide=guide_colorbar(barwidth=15, title.position = "top", hjust=1, label.hjust=0.3))+
    scale_y_continuous(breaks=seq(1,max(fig10.data$order.y),1), labels=plot.names$country, sec.axis = sec_axis(~., breaks=seq(1,max(fig10.data$order.y),1), labels=plot.names$country, name = "Importing country"))+
    scale_x_continuous(breaks=seq(1,max(fig10.data$order.x),1),labels=plot.names$country)+
    labs(x="Exporting countries",y="Importing countries")+
    theme(panel.background = element_blank(), 
          panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
          axis.text.x.bottom = element_text(hjust = 1)
    )
  return(fig10)
}


# Figure 11 create graph ------------------------------------------------------

# Chart 11: Heat Map for all G20. Y-axis shows the importing 
# nations, X-axis shows the exporting nations. Place USA and 
# China at beginning, (top left) and all other countries 
# alphabetically after them. Graph shows the change in share 
# of exports affected by trade distortions in force today 
# (cut off date) to distortions in force at 31.12.16. The 
# Graph will have 3 shades (green for negative change, 
# white for zero change and red for positive change).

load(paste0(data.path,"G20 sector coverage change - harmful.Rdata"))

fig11.data <- sct.g20.change.harmful
fig11.data <- merge(fig11.data, order.names, by.x = "importer", by.y = "country")
fig11.data <- merge(fig11.data, order.names, by.x = "exporter", by.y = "country")
fig11.data <- fig11.data[with(fig11.data, order(order.x,order.y)),]
fig11.data$change[is.na(fig11.data$change)] <- 0
row.names(fig11.data) <- NULL

write.xlsx(fig11.data[,c("importer","exporter","change","sector","populist","pre.populist")],file=paste0(output.path,"Table for Figure 11.xlsx"),row.names=F, sheetName = "Coverages")

fig11.create <- function(sct) {
  
  fig11 <- ggplot(data=subset(fig11.data, sector==sct))+
    geom_tile(data=blank.set, aes(x=order.x, y=order.y), fill="#FFFFFF", color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(aes(x=order.y, y=order.x, fill=change), color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(data=blank.set.middle, aes(x=order.x, y=order.y), fill="#FFFFFF", color="#FFFFFF", size=0.2, na.rm = F)+
    gta_theme(x.bottom.angle = 45)+
    scale_fill_gradientn(name="Change in bilateral exports \nfacing importer\'s trade distortions \nfrom pre 2017 to today",
                         colours = c(gta_colour$green[1], "#FFFFFF", gta_colour$red[1]),values=rescale(c(min(subset(fig11.data, sector == sct)$change),0,max(subset(fig11.data, sector == sct)$change))), 
                         breaks=waiver(), labels = percent,
                         guide=guide_colorbar(barwidth=15, title.position = "top", hjust=1, label.hjust=0.3))+
    scale_y_continuous(breaks=seq(1,max(fig11.data$order.y),1), labels=plot.names$country, sec.axis = sec_axis(~., breaks=seq(1,max(fig11.data$order.y),1), labels=plot.names$country, name = "Importing country"))+
    scale_x_continuous(breaks=seq(1,max(fig11.data$order.x),1),labels=plot.names$country)+
    labs(x="Exporting countries",y="Importing countries")+
    theme(panel.background = element_blank(),
          panel.border=element_rect(size=1, colour="grey",fill = "transparent"),
          axis.text.x.bottom = element_text(hjust = 1)
    )
  
  return(fig11)
}



# Figure 11 create graph ------------------------------------------------------

# Chart 11: Heat Map for all G20. Y-axis shows the importing 
# nations, X-axis shows the exporting nations. Place USA and 
# China at beginning, (top left) and all other countries 
# alphabetically after them. Graph shows the change in share 
# of exports affected by trade distortions in force today 
# (cut off date) to distortions in force at 31.12.16. The 
# Graph will have 3 shades (green for negative change, 
# white for zero change and red for positive change).

load(paste0(data.path,"G20 sector coverage change - liberalising.Rdata"))

fig12.data <- sct.g20.change.liberalising
fig12.data <- merge(fig12.data, order.names, by.x = "importer", by.y = "country")
fig12.data <- merge(fig12.data, order.names, by.x = "exporter", by.y = "country")
fig12.data <- fig12.data[with(fig12.data, order(order.x,order.y)),]
fig12.data$change[is.na(fig12.data$change)] <- 0
row.names(fig12.data) <- NULL

write.xlsx(fig12.data[,c("importer","exporter","change","sector","populist","pre.populist")],file=paste0(output.path,"Table for Figure 12.xlsx"),row.names=F, sheetName = "Coverages")

fig12.create <- function(sct) {
  
  fig12 <- ggplot(data=subset(fig12.data, sector==sct))+
    geom_tile(data=blank.set, aes(x=order.x, y=order.y), fill="#FFFFFF", color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(aes(x=order.y, y=order.x, fill=change), color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(data=blank.set.middle, aes(x=order.x, y=order.y), fill="#FFFFFF", color="#FFFFFF", size=0.2, na.rm = F)+
    gta_theme(x.bottom.angle = 45)+
    scale_fill_gradientn(name="Change in bilateral exports \nfacing importer\'s trade remedies from pre 2017 to today",
                         colours = c(gta_colour$red[1], "#FFFFFF", gta_colour$green[1]),values=rescale(c(min(subset(fig12.data, sector == sct)$change),0,max(subset(fig12.data, sector == sct)$change))), 
                         breaks=waiver(), labels = percent,
                         guide=guide_colorbar(barwidth=15, title.position = "top", hjust=1, label.hjust=0.3))+
    scale_y_continuous(breaks=seq(1,max(fig12.data$order.y),1), labels=plot.names$country, sec.axis = sec_axis(~., breaks=seq(1,max(fig12.data$order.y),1), labels=plot.names$country, name = "Importing country"))+
    scale_x_continuous(breaks=seq(1,max(fig12.data$order.x),1),labels=plot.names$country)+
    labs(x="Exporting countries",y="Importing countries")+
    theme(panel.background = element_blank(), 
          panel.border=element_rect(size=1, colour="gray",fill = "transparent"), 
          axis.text.x.bottom = element_text(hjust = 1)
    )
  return(fig12)
}



# Create panels per sector ------------------------------------------------

for (sct in sectors) {
  
  fig9 <- fig9.create(sct)
  fig10 <- fig10.create(sct)
  fig11 <- fig11.create(sct)
  fig12 <- fig12.create(sct)
  
  figA <- grid.arrange(fig9, fig11, nrow=2)
  figB <- grid.arrange(fig10, fig12, nrow=2)

  gta_plot_saver(plot = figA,
                 path = paste0(output.path),
                 name = paste0("Figure Panel 3 - Sector ",sct),
                 cairo_ps = T,
                 height = 29.7,
                 width = 21)
  
  gta_plot_saver(plot = figB,
                 path = paste0(output.path),
                 name = paste0("Figure Panel 4 - Sector ",sct),
                 cairo_ps = T,
                 height = 29.7,
                 width = 21)
  

}

