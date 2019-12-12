rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(scales)
library(xlsx)
library(data.table)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')


directories=gta25_setup(internal.name="Sectoral chapters panel 3-4",
                        in.dev=F,
                        author=NULL,
                        wipe.data=F,
                        wipe.figs=T)


data.path = directories$data.path
output.path = directories$figure.path

gta_colour_palette()
order.names <- data.frame(country = c("United States of America","China",c(g20.member.names[! g20.member.names %in% c("United States of America", "China")])), order = c(seq(1,19,1)))
order.names$country <- as.character(order.names$country)
order.names$country[order.names$country=="South Korea"] <- "Republic of Korea"
plot.names <- data.frame(country = c("USA","China",c(g20.member.names[! g20.member.names %in% c("United States of America", "China","United Kingdom")]),"UK"), order = c(seq(1,19,1)))
blank.set <- data.frame(order.y = rep(seq(1,19,1),19), order.x = rep(1:19, each=19))
blank.set.middle <- data.frame(order.x=seq(1,19,1),order.y=seq(1,19,1))



## setting figure paths
first.sector.chapter=5
sector.path=paste0(str_extract(output.path,"^.+?figures/"),paste0(first.sector.chapter:(first.sector.chapter+length(sectors)-1), " - Sector ", sectors,"/"))
wipe.sector.path=F




# Figure 9 create graph ------------------------------------------------------

# Chart 9: Heat Map for all G20. Y-axis shows the importing nations, 
# X-axis shows the exporting nations. Place USA and China at beginning,
# (top left) and all other countries alphabetically after them. Graph 
# shows share of sectoral exports affected by trade distortions in force 
# today implemented by the importing nation. Show in shades of red.

# Please add to the title of the top panel after the word «distortions» 
# the following words «in force on 15 November 2019»

# At the bottom of the panels (or the bottom of the page) please 
# make a note that a grey cell implies there is no bilateral trade 
# observed in this sector.

load(paste0(data.path,"G20 sector exports coverages - harmful.Rdata"))

fig9.data <- sct.g20.harmful
fig9.data <- merge(fig9.data, order.names, by.x = "importer", by.y = "country")
fig9.data <- merge(fig9.data, order.names, by.x = "exporter", by.y = "country")
fig9.data <- fig9.data[with(fig9.data, order(order.x,order.y)),]
row.names(fig9.data) <- NULL

fig9.xlsx <- fig9.data[,c("importer","exporter","2019","sector")]
names(fig9.xlsx) <- c("importer","exporter","coverage","sector")

fig9.create <- function(sct) {
    
  fig9 <- ggplot(data=subset(fig9.data, sector==sct))+
    geom_tile(data=blank.set, aes(x=order.x, y=order.y, color=as.factor(1)), fill=gta_colour$grey[4], size=0.2, na.rm = F)+
    geom_tile(aes(x=order.y, y=order.x, fill=`2019`), color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(data=blank.set.middle, aes(x=order.x, y=order.y), fill="#FFFFFF", color="#FFFFFF", size=0.2, na.rm = F)+
    geom_rect(data=data.frame(),aes(xmax=0.5, xmin=2.5, ymin = 0.5, ymax = 2.5), size=0.8, color=gta_colour$blue[1], fill="transparent")+
    gta_theme(x.bottom.angle = 45)+
    scale_fill_gradientn(name="", 
                         colours = c(gta_colour$red[4], gta_colour$red[1]), values=c(0,0.25,0.50,0.75,1), 
                         breaks=c(0,0.25,0.5,0.75,1), labels=c("0%","25%","50%","75%", "100%"),
                         limits=c(0,1),
                         guide=guide_colorbar(barwidth=15, label.hjust = 0.5))+
    scale_colour_manual(values="#FFFFFF", label="No trade affected")+
    scale_y_continuous(breaks=seq(1,max(fig9.data$order.y),1), labels=plot.names$country, sec.axis = sec_axis(~., breaks=seq(1,max(fig9.data$order.y),1), labels=plot.names$country, name = "Importing country"))+
    scale_x_continuous(breaks=seq(1,max(fig9.data$order.x),1),labels=plot.names$country)+
    labs(x="Exporting country",y="Importing country")+
    ggtitle("Percentage of bilateral exports facing importer\'s \ntrade distortions in force on 15 November 2019")+
    guides(colour=guide_legend(title=NULL, position="right",barwidth=1, label.position = "bottom",keywidth = 0,hjust=0, label.hjust=0))+
    theme(panel.background = element_blank(), 
          panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
          axis.text.x.bottom = element_text(hjust = 1)
    )
  
  return(fig9)
  }

# Figure 10 create graph ------------------------------------------------------

# Chart 10: Same as chart 9, but with liberalising interventions.

# Please add to the title of the top panel after the word «distortions» 
# the following words «in force on 15 November 2019»

# At the bottom of the panels (or the bottom of the page) please 
# make a note that a grey cell implies there is no bilateral trade 
# observed in this sector. 

load(paste0(data.path,"G20 sector exports coverages - liberalising.Rdata"))

fig10.data <- sct.g20.liberalising
fig10.data <- merge(fig10.data, order.names, by.x = "importer", by.y = "country")
fig10.data <- merge(fig10.data, order.names, by.x = "exporter", by.y = "country")
fig10.data <- fig10.data[with(fig10.data, order(order.x, order.y)),]
row.names(fig10.data) <- NULL

fig10.xlsx <- fig10.data[,c("importer","exporter","2019","sector")]
names(fig10.xlsx) <- c("importer","exporter","coverage","sector")

fig10.create <- function(sct) {
  
  fig10 <- ggplot(data=subset(fig10.data, sector==sct))+
    geom_tile(data=blank.set, aes(x=order.x, y=order.y, colour=as.factor(1)), fill=gta_colour$grey[4], size=0.2, na.rm = F)+
    geom_tile(aes(x=order.y, y=order.x, fill=`2019`), color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(data=blank.set.middle, aes(x=order.x, y=order.y), fill="#FFFFFF", color="#FFFFFF", size=0.2, na.rm = F)+
    geom_rect(data=data.frame(),aes(xmax=0.5, xmin=2.5, ymin = 0.5, ymax = 2.5), size=0.8, color=gta_colour$blue[1], fill="transparent")+
    gta_theme(x.bottom.angle = 45)+
    scale_fill_gradientn(name="", 
                         colours = c(gta_colour$green[4], gta_colour$green[1]), values=c(0,0.25,0.50,0.75,1), 
                         breaks=c(0,0.25,0.5,0.75,1), labels=c("0%","25%","50%","75%", "100%"),
                         limits=c(0,1),
                         guide=guide_colorbar(barwidth=15, label.hjust = 0.5))+
    scale_colour_manual(values="#FFFFFF", label="No trade affected")+
    scale_y_continuous(breaks=seq(1,max(fig10.data$order.y),1), labels=plot.names$country, sec.axis = sec_axis(~., breaks=seq(1,max(fig10.data$order.y),1), labels=plot.names$country, name = "Importing country"))+
    scale_x_continuous(breaks=seq(1,max(fig10.data$order.x),1),labels=plot.names$country)+
    labs(x="Exporting country",y="Importing country")+
    ggtitle("Percentage of bilateral exports profiting from \nimporter\'s reforms in force on 15 November 2019")+
    guides(colour=guide_legend(title=NULL, position="right",barwidth=1, label.position = "bottom",keywidth = 0,hjust=0, label.hjust=0))+
    theme(panel.background = element_blank(), 
          panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
          axis.text.x.bottom = element_text(hjust = 1)
    )
  fig10
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
fig11.data <- subset(fig11.data, is.na(change)==F)
fig11.data <- fig11.data[with(fig11.data, order(order.x,order.y)),]
row.names(fig11.data) <- NULL



fig11.create <- function(sct) {
  
  if (min(subset(fig11.data, sector == sct)$change)==0) {
    values = c(0,max(subset(fig11.data, sector == sct)$change))
    colors = c("#FFFFFF", gta_colour$red[1])
  } else {
    values = c(min(subset(fig11.data, sector == sct)$change),0,max(subset(fig11.data, sector == sct)$change))
    colors = c(gta_colour$green[1], "#FFFFFF", gta_colour$red[1])
    }
  
  fig11 <- ggplot(data=subset(fig11.data, sector==sct))+
    geom_tile(data=blank.set, aes(x=order.x, y=order.y), fill=gta_colour$grey[4], color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(aes(x=order.y, y=order.x, fill=change), color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(data=blank.set.middle, aes(x=order.x, y=order.y), fill="#FFFFFF", color="#FFFFFF", size=0.2, na.rm = F)+
    geom_rect(data=data.frame(),aes(xmax=0.5, xmin=2.5, ymin = 0.5, ymax = 2.5), size=0.8, color=gta_colour$blue[1], fill="transparent")+
    gta_theme(x.bottom.angle = 45)+
    scale_fill_gradientn(name="Change in bilateral export share from end of 2016 \nto today facing importer\'s distortions",
                         colours = colors,values=rescale(values), 
                         breaks=waiver(), labels = percent,
                         guide=guide_colorbar(barwidth=15, title.position = "top", hjust=1, label.hjust=0.3))+
    scale_y_continuous(breaks=seq(1,max(fig11.data$order.y),1), labels=plot.names$country, sec.axis = sec_axis(~., breaks=seq(1,max(fig11.data$order.y),1), labels=plot.names$country, name = "Importing country"))+
    scale_x_continuous(breaks=seq(1,max(fig11.data$order.x),1),labels=plot.names$country)+
    labs(x="Exporting country",y="Importing country")+
    theme(panel.background = element_blank(),
          panel.border=element_rect(size=1, colour="grey",fill = "transparent"),
          axis.text.x.bottom = element_text(hjust = 1)
    )
  fig11
  return(fig11)
}



# Figure 12 create graph ------------------------------------------------------

# Chart 12: Heat Map for all G20. Y-axis shows the importing 
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
fig12.data <- subset(fig12.data, is.na(change)==F)
fig12.data <- fig12.data[with(fig12.data, order(order.x,order.y)),]
row.names(fig12.data) <- NULL


fig12.create <- function(sct) {
  
  if (min(subset(fig12.data, sector == sct)$change)==0) {
    values = c(0,max(subset(fig12.data, sector == sct)$change))
    colors = c("#FFFFFF", gta_colour$green[1])
  } else {
    values = c(min(subset(fig12.data, sector == sct)$change),0,max(subset(fig12.data, sector == sct)$change))
    colors = c(gta_colour$red[1], "#FFFFFF", gta_colour$green[1])
  }
  
  fig12 <- ggplot(data=subset(fig12.data, sector==sct))+
    geom_tile(data=blank.set, aes(x=order.x, y=order.y), fill=gta_colour$grey[4], color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(aes(x=order.y, y=order.x, fill=change), color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(data=blank.set.middle, aes(x=order.x, y=order.y), fill="#FFFFFF", color="#FFFFFF", size=0.2, na.rm = F)+
    geom_rect(data=data.frame(),aes(xmax=0.5, xmin=2.5, ymin = 0.5, ymax = 2.5), size=0.8, color=gta_colour$blue[1], fill="transparent")+
    gta_theme(x.bottom.angle = 45)+
    scale_fill_gradientn(name="Change in bilateral export share from end of 2016 \nto today profiting from importer\'s reforms",
                         colours = colors, values=rescale(values), 
                         breaks=waiver(), labels = percent,
                         guide=guide_colorbar(barwidth=15, title.position = "top", hjust=1, label.hjust=0.3))+
    scale_y_continuous(breaks=seq(1,max(fig12.data$order.y),1), labels=plot.names$country, sec.axis = sec_axis(~., breaks=seq(1,max(fig12.data$order.y),1), labels=plot.names$country, name = "Importing country"))+
    scale_x_continuous(breaks=seq(1,max(fig12.data$order.x),1),labels=plot.names$country)+
    labs(x="Exporting country",y="Importing country")+
    theme(panel.background = element_blank(), 
          panel.border=element_rect(size=1, colour="gray",fill = "transparent"), 
          axis.text.x.bottom = element_text(hjust = 1)
    )
  
  return(fig12)
}

# Figure 13, 14 additional create figure, trade affected in populist era ------------------------------------------------------

# Can you please produce a version that reveals the share of
# trade affected during the populist era (that is from 2017-1-1 
# until 2019-11-15)? I am not sure I will use this new version 
# but it would be good to see what the results look like.

# In the bottom panel please recalculate the heatmap to reveal 
# the percentage of bilateral exports facing importer’s trade 
# distortions implemented since 1 January 2017. I want to show 
# what shares of bilateral trade are affected by harmful import
# distortions implemented during the populist era. Please change 
# the title of the chart to «Bilateral export share today facing 
# importer’s trade distortions implemented since 1 January 2017» 

# At the bottom of the panels (or the bottom of the page) please 
# make a note that a grey cell implies there is no bilateral trade 
# observed in this sector. 


load(paste0(data.path,"G20 sector exports coverages - populist era.Rdata"))
setnames(sct.g20.populist,"2019","coverages")
fig13.data <- subset(sct.g20.populist, type=="harmful")
fig13.data <- merge(fig13.data, order.names, by.x = "importer", by.y = "country")
fig13.data <- merge(fig13.data, order.names, by.x = "exporter", by.y = "country")
fig13.data <- fig13.data[with(fig13.data, order(order.x,order.y)),]
row.names(fig13.data) <- NULL

fig13.create <- function(sct) {
  
  fig13 <- ggplot(data=subset(fig13.data, sector==sct))+
    geom_tile(data=blank.set, aes(x=order.x, y=order.y, color=as.factor(1)), fill=gta_colour$grey[4], size=0.2, na.rm = F)+
    geom_tile(aes(x=order.y, y=order.x, fill=coverages), color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(data=blank.set.middle, aes(x=order.x, y=order.y), fill="#FFFFFF", color="#FFFFFF", size=0.2, na.rm = F)+
    geom_rect(data=data.frame(),aes(xmax=0.5, xmin=2.5, ymin = 0.5, ymax = 2.5), size=0.8, color=gta_colour$blue[1], fill="transparent")+
    gta_theme(x.bottom.angle = 45)+
    scale_fill_gradientn(name="",
                         colours = c(gta_colour$red[4], gta_colour$red[1]), values=c(0,0.25,0.50,0.75,1), 
                         breaks=c(0,0.25,0.5,0.75,1), labels=c("0%","25%","50%","75%", "100%"),
                         limits=c(0,1),
                         guide=guide_colorbar(barwidth=15, label.hjust = 0.5))+
    scale_colour_manual(values="#FFFFFF", label="No trade affected")+
    ggtitle("Bilateral export share today facing importer’s \ntrade distortions implemented since 1 January 2017")+
    scale_y_continuous(breaks=seq(1,max(fig13.data$order.y),1), labels=plot.names$country, sec.axis = sec_axis(~., breaks=seq(1,max(fig13.data$order.y),1), labels=plot.names$country, name = "Importing country"))+
    scale_x_continuous(breaks=seq(1,max(fig13.data$order.x),1),labels=plot.names$country)+
    labs(x="Exporting country",y="Importing country")+
    guides(colour=guide_legend(title=NULL, position="right",barwidth=1, label.position = "bottom",keywidth = 0,hjust=0, label.hjust=0))+
    theme(panel.background = element_blank(), 
          panel.border=element_rect(size=1, colour="gray",fill = "transparent"), 
          axis.text.x.bottom = element_text(hjust = 1)
    )
  
  return(fig13)
}


fig14.data <- subset(sct.g20.populist, type=="liberalising")
fig14.data <- merge(fig14.data, order.names, by.x = "importer", by.y = "country")
fig14.data <- merge(fig14.data, order.names, by.x = "exporter", by.y = "country")
fig14.data <- fig14.data[with(fig14.data, order(order.x,order.y)),]
row.names(fig14.data) <- NULL

fig14.create <- function(sct) {
  
  fig14 <- ggplot(data=subset(fig14.data, sector==sct))+
    geom_tile(data=blank.set, aes(x=order.x, y=order.y, color=as.factor(1)), fill=gta_colour$grey[4], size=0.2, na.rm = F)+
    geom_tile(aes(x=order.y, y=order.x, fill=coverages), color="#FFFFFF", size=0.2, na.rm = F)+
    geom_tile(data=blank.set.middle, aes(x=order.x, y=order.y), fill="#FFFFFF", color="#FFFFFF", size=0.2, na.rm = F)+
    geom_rect(data=data.frame(),aes(xmax=0.5, xmin=2.5, ymin = 0.5, ymax = 2.5), size=0.8, color=gta_colour$blue[1], fill="transparent")+
    gta_theme(x.bottom.angle = 45)+
    scale_fill_gradientn(name="",
                         colours = c(gta_colour$green[4], gta_colour$green[1]), values=c(0,0.25,0.50,0.75,1), 
                         breaks=c(0,0.25,0.5,0.75,1), labels=c("0%","25%","50%","75%", "100%"),
                         limits=c(0,1),
                         guide=guide_colorbar(barwidth=15, label.hjust = 0.5))+
    scale_colour_manual(values="#FFFFFF", label="No trade affected")+
    scale_y_continuous(breaks=seq(1,max(fig14.data$order.y),1), labels=plot.names$country, sec.axis = sec_axis(~., breaks=seq(1,max(fig14.data$order.y),1), labels=plot.names$country, name = "Importing country"))+
    scale_x_continuous(breaks=seq(1,max(fig14.data$order.x),1),labels=plot.names$country)+
    labs(x="Exporting country",y="Importing country")+
    ggtitle("Bilateral export share today benefiting from importer’s \ntrade reforms implemented since 1 January 2017")+
    guides(colour=guide_legend(title=NULL, position="right",barwidth=1, label.position = "bottom",keywidth = 0,hjust=0, label.hjust=0))+
    theme(panel.background = element_blank(), 
          panel.border=element_rect(size=1, colour="gray",fill = "transparent"), 
          axis.text.x.bottom = element_text(hjust = 1)
    )
  
  return(fig14)
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
  
  
  fig9 <- fig9.create(sct)
  fig10 <- fig10.create(sct)
  fig11 <- fig11.create(sct)
  fig12 <- fig12.create(sct)
  fig13 <- fig13.create(sct)
  fig14 <- fig14.create(sct)
  
  figA <- grid.arrange(fig9, fig13, nrow=2)
  figB <- grid.arrange(fig10, fig14, nrow=2)
  figC <- grid.arrange(fig11, fig12, nrow=2)

  gta_plot_saver(plot = figA,
                 path = s.path,
                 name = paste0("Panel 3 - Sector ",sct, " (Fig 9 & 13)"),
                 cairo_ps = T,
                 height = 29.7,
                 width = 21)
  
  gta_plot_saver(plot = figB,
                 path = s.path,
                 name = paste0("Panel 4 - Sector ",sct, " (Fig 10 & 14)"),
                 cairo_ps = T,
                 height = 29.7,
                 width = 21)
  
  gta_plot_saver(plot = figC,
                 path = s.path,
                 name = paste0("Panel 5 - Sector ",sct, " (Fig 11 & 12)"),
                 cairo_ps = T,
                 height = 29.7,
                 width = 21)
  fig9.xlsx
  write.xlsx(subset(fig9.xlsx, sector==sct), file=paste0(s.path,"Table for Figure 9.xlsx"),row.names=F, sheetName = "Coverages")
  write.xlsx(subset(fig10.xlsx, sector==sct), file=paste0(s.path,"Table for Figure 10.xlsx"),row.names=F, sheetName = "Coverages")
  write.xlsx(subset(fig11.data, sector==sct)[,c("importer","exporter","change","sector","populist","pre.populist")],file=paste0(s.path,"Fig 11 data.xlsx"),row.names=F, sheetName = "Coverages")
  write.xlsx(subset(fig12.data, sector==sct)[,c("importer","exporter","change","sector","populist","pre.populist")],file=paste0(s.path,"Fig 12 data.xlsx"),row.names=F, sheetName = "Coverages")
  write.xlsx(subset(fig13.data, sector==sct)[,c("importer","exporter","sector","coverages")],file=paste0(s.path,"Fig 13 data.xlsx"),row.names=F, sheetName = "Coverages")
  write.xlsx(subset(fig14.data, sector==sct)[,c("importer","exporter","sector","coverages")],file=paste0(s.path,"Fig 14 data.xlsx"),row.names=F, sheetName = "Coverages")

}
