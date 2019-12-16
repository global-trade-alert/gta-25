rm(list=ls())

library(gtalibrary)
library(stringr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(xlsx)
library(scales)

gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')


directories=gta25_setup(internal.name="Sectoral chapters panel 1",
                        in.dev=F,
                        author=NULL,
                        wipe.data=F,
                        wipe.figs=T)


data.path = directories$data.path
output.path = directories$figure.path

gta_colour_palette()



## setting figure paths
first.sector.chapter=8
sector.path=paste0(str_extract(output.path,"^.+?figures/"),paste0(first.sector.chapter:(first.sector.chapter+length(sectors)-1), " - Sector ", sectors,"/"))
wipe.sector.path=F

x.bottom.angle = 45
x.bottom.align = 1


# Figure 1 create graph ------------------------------------------------------

# Chart 1: Line graph showing share of total sectoral trade 
# affected by discriminatory (red) and liberalising (green) 
# interventions. X-axis should divided into pre-populist era (2009-2016) 
# and populist era (2017-2019), by shading the populist era plot 
# background. Add two dots (green and red) in 2019 showing the 
# share of global trade affected by discriminatory and liberalising interventions.

load(paste0(data.path,"Global coverage harmful 2019.Rdata"))
load(paste0(data.path,"Global coverage liberalising 2019.Rdata"))
load(paste0(data.path,"Sector coverages harmful.Rdata"))
load(paste0(data.path,"Sector coverages liberalising.Rdata"))

sct.cov.harmful$type="harmful"
sct.cov.liberalising$type="liberalising"

fig1.data <- rbind(sct.cov.harmful, sct.cov.liberalising)

fig1.create <- function(sct) {
    
    fig1 <- ggplot(data=subset(fig1.data, sector == sct))+
    geom_rect(data=data.frame(), aes(xmin=2017, xmax=Inf, ymin=-Inf, ymax=Inf), fill=pop.shade, alpha=0.3)+
    geom_text(aes(x=2017, y=Inf, label="Populist\n era"), hjust=-0.1, vjust=1.4, color = pop.text, lineheight = 1)+
    geom_text(aes(x=-Inf, y=Inf, label="Pre-populist era"), hjust=-0.1, vjust=2, color = gta_colour$grey[1], lineheight = 1)+
    geom_line(aes(x=year, y=coverages, colour=type), size = 1)+
    geom_point(aes(x=2019-0.2, y=glo.cov.harmful),size=3, colour=gta_colour$red[1])+
    geom_point(aes(x=2019-0.2, y=glo.cov.liberalising),size=3, colour=gta_colour$green[1])+
    geom_text(aes(x=2019-0.2, y=glo.cov.harmful, label=paste0("Share of global\nexports affected: ",round(glo.cov.harmful,3)*100,"%")), colour=gta_colour$red[1], hjust=1, nudge_x = -0.3)+
    geom_text(aes(x=2019-0.2, y=glo.cov.liberalising, label=paste0("Share of global\nexports affected: ",round(glo.cov.liberalising,3)*100,"%")), colour=gta_colour$green[1], hjust=1, nudge_x = -0.3)+
    gta_plot_wrapper(data=subset(fig1.data, sector == sct),
                     data.x="year",
                     data.y="coverages",
                     x.bottom.name = "Year",
                     x.bottom.breaks = seq(2009,2019,1),
                     y.left.name = "Share of sectoral exports affected",
                     y.left.limits = c(0,1),
                     y.left.labels = percent,
                     y.right.enable = F,
                     colour.labels = c("Discriminatory","Liberalising"),
                     colour.legend.title = "Type of intervention",
                     colour.palette = c(gta_colour$red[1],gta_colour$green[1]),
                     colour.legend.col = 2)+
      gta_theme(x.bottom.angle = x.bottom.angle, x.bottom.align = x.bottom.align)
    
  return(fig1)
  }

# Figure 2 create graph ------------------------------------------------------

# Chart 2: Line graph showing share of total sectoral trade affected
# by (1) all discriminatory interventions, (2) export incentives 
# (3) All except export incentives, (4) subsidies and (5) tariff 
# incentives. Add shading as before for populist, pre-populist era.

load(paste0(data.path,"Sector coverages types harmful.Rdata"))
sct.cov.harmful$type="all"
fig2.data <- rbind(sct.cov.types.harmful, sct.cov.harmful)

fig2.create <- function(sct) {

  fig2 <- ggplot(data=subset(fig2.data, sector == sct))+
    geom_rect(data=data.frame(), aes(xmin=2017, xmax=Inf, ymin=-Inf, ymax=Inf), fill=pop.shade, alpha=0.3)+
    geom_text(aes(x=2017, y=Inf, label="Populist\n era"), hjust=-0.1, vjust=1.4, color = pop.text, lineheight = 1)+
    geom_text(aes(x=-Inf, y=Inf, label="Pre-populist era"), hjust=-0.1, vjust=2, color = gta_colour$grey[1], lineheight = 1)+
    geom_line(aes(x=year, y=coverages, colour=type), size = 1)+
    gta_plot_wrapper(data=subset(fig2.data, sector == sct),
                     data.x="year",
                     data.y="coverages",
                     x.bottom.name = "Year",
                     x.bottom.breaks = seq(2009,2019,1),
                     y.left.name = "Share of sectoral exports affected",
                     y.left.limits = c(0,1),
                     y.left.labels = percent,
                     y.right.enable = F,
                     colour.labels = c("Export incentives","All except export incentives","Subsidies", "Tariffs","All"),
                     colour.legend.title = "Trade distortions in force",
                     colour.palette = c(gta_colour$qualitative[c(1:4)],gta_colour$red[1]),
                     colour.legend.col = 2)+
    gta_theme(x.bottom.angle = x.bottom.angle, x.bottom.align = x.bottom.align)
  
  return(fig2)
}

# Figure 3 create graph ------------------------------------------------------

# Chart 3: Line graph showing share of total sectoral trade 
# affected by (1) all liberalising interventions, (2) subsidy reductions 
# and (3) tariff reductions. Add shading for populist, pre-populist era.

load(paste0(data.path,"Sector coverages types liberalising.Rdata"))
sct.cov.liberalising$type="all"
fig3.data <- rbind(sct.cov.types.liberalising, sct.cov.liberalising)

fig3.create <- function(sct) {
  
  fig3 <- ggplot(data=subset(fig3.data, sector == sct))+
    geom_rect(data=data.frame(), aes(xmin=2017, xmax=Inf, ymin=-Inf, ymax=Inf), fill=pop.shade, alpha=0.3)+
    geom_text(aes(x=2017, y=Inf, label="Populist\n era"), hjust=-0.1, vjust=1.4, color = pop.text, lineheight = 1)+
    geom_text(aes(x=-Inf, y=Inf, label="Pre-populist era"), hjust=-0.1, vjust=2, color = gta_colour$grey[1], lineheight = 1)+
    geom_line(aes(x=year, y=coverages, colour=type), size = 1)+
    gta_plot_wrapper(data=subset(fig3.data, sector == sct),
                     data.x="year",
                     data.y="coverages",
                     x.bottom.name = "Year",
                     x.bottom.breaks = seq(2009,2019,1),
                     y.left.name = "Share of sectoral exports affected",
                     y.left.limits = c(0,1),
                     y.left.labels = percent,
                     y.right.enable = F,
                     colour.labels = c("Subsidies","Tariffs", "All"),
                     colour.legend.title = "Commercial policy reforms",
                     colour.palette = c(gta_colour$qualitative[c(1:2)],gta_colour$green[1]),
                     colour.legend.col = 2)+
    gta_theme(x.bottom.angle = x.bottom.angle, x.bottom.align = x.bottom.align)
  
  return(fig3)
}


# Figure 4 create graph ------------------------------------------------------

# Chart 4: Stacked bar chart (maybe there's a better way of visualising it?) showing 
# share of sectoral trade affected by harmful interventions hitting 1, 2, 3-5, 6-10 and 11+ 
# trading partners. Add populist and pre-populist era shading.

load(paste0(data.path,"Sector coverages hit brackets.Rdata"))

fig4.data <- sct.cov.hit.brkts
fig4.data$order[fig4.data$hits == "1"] <- 5
fig4.data$order[fig4.data$hits == "2"] <- 4
fig4.data$order[fig4.data$hits == "3 - 5"] <- 3
fig4.data$order[fig4.data$hits == "6 - 10"] <- 2
fig4.data$order[fig4.data$hits == "11 or more"] <- 1
fig4.data <- fig4.data[with(fig4.data, order(-order)),]
row.names(fig4.data) <- NULL
fig4.data$year <- as.numeric(as.character(fig4.data$year))


fig4.create <- function(sct) {
  
  fig4 <- ggplot(data=subset(fig4.data, sector == sct))+
    geom_rect(data=data.frame(),aes(xmin=2017, xmax=Inf, ymin=-Inf, ymax=Inf), fill=pop.shade, alpha=0.3)+
    geom_text(aes(x=2017, y=Inf, label="Populist\n era"), hjust=-0.1, vjust=1.4, color = pop.text, lineheight = 1)+
    geom_text(aes(x=-Inf, y=Inf, label="Pre-populist era"), hjust=-0.1, vjust=2, color = gta_colour$grey[1], lineheight = 1)+
    geom_bar(aes(x=year, y=coverages, fill=forcats::fct_inorder(hits)), stat="identity", position="stack")+
    gta_plot_wrapper(data=subset(fig4.data, sector == sct),
                     data.x="year",
                     data.y="coverages",
                     x.bottom.name = "Year",
                     x.bottom.breaks = seq(2009,2019,1),
                     x.bottom.limits = c(2008.5,2019.5),
                     y.left.name = "Share of sectoral exports affected",
                     y.left.labels = percent,
                     y.left.limits = c(0,1),
                     y.right.enable = F,
                     fill.legend.title = "Number of times hit",
                     fill.palette = rev(c(gta_colour$red.shades(5))),
                     fill.legend.col = 3,
                     fill.labels = unique(fig4.data$hits))+
    gta_theme(x.bottom.angle = x.bottom.angle, x.bottom.align = x.bottom.align)
  
  return(fig4)
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
  fig2 <- fig2.create(sct)
  fig3 <- fig3.create(sct)
  fig4 <- fig4.create(sct)
  
  figA <- grid.arrange(fig1, fig4, nrow=2)
  figB <- grid.arrange(fig2, fig3, nrow=2)
  figC <- grid.arrange(fig1, fig2, fig3, fig4, nrow=2)

  gta_plot_saver(plot = figA,
                 path = s.path,
                 name = paste0("Panel 1 A (Fig 1 & 2) - Sector ",sct),
                 cairo_ps = T,
                 height = 29.7,
                 width = 21)
  
  gta_plot_saver(plot = figB,
                 path = s.path,
                 name = paste0("Panel 1 B (Fig 3 & 4) - Sector ",sct),
                 cairo_ps = T,
                 height = 29.7,
                 width = 21)
  
  # Four plots per page, not really feasible
  # 191216 Removed from final version
  # gta_plot_saver(plot = figC,
  #                path = s.path,
  #                name = paste0("Panel 1 C (Fig 1-4) - Sector ",sct),
  #                cairo_ps = T,
  #                height = 29.7,
  #                width = 21)
  # 
  write.xlsx(subset(fig1.data, sector==sct), file=paste0(s.path,"Fig 1 data.xlsx"),row.names=F, sheetName = "Coverages")
  write.xlsx(subset(fig2.data, sector==sct), file=paste0(s.path,"Fig 2 data.xlsx"),row.names=F, sheetName = "Coverages")
  write.xlsx(subset(fig3.data, sector==sct), file=paste0(s.path,"Fig 3 data.xlsx"),row.names=F, sheetName = "Coverages")
  write.xlsx(subset(fig4.data, sector==sct)[,c("sector","year","coverages","hits")], file=paste0(s.path,"Fig 4 data.xlsx"),row.names=F, sheetName = "Coverages")
  
}

