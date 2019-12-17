rm(list=ls())

library(ggplot2)
library(gtalibrary)
windowsFonts("OpenSans" = windowsFont("OpenSans"))
gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')
chapter.folders=gta25_setup(internal.name = 'business concerns',
                            in.dev = F,
                            author=NULL,
                            wipe.data = F,
                            wipe.figs = T)
data.path=chapter.folders$data.path
figure.path=chapter.folders$figure.path

gta_colour_palette()
symbol.size=2

## loading data
load(paste0(data.path,'confidence.data.Rdata'))



# Chart 1 -----------------------------------------------------------------

fig1 = ggplot(confidence.data) +
  geom_rect(data=data.frame(),aes(xmin=as.Date("2017-01-15"), xmax=as.Date("2019-11-15"), ymin=-Inf, ymax=Inf), fill=pop.shade, alpha=0.3)+
  geom_text(aes(x=as.Date("2017-01-15"), y=Inf, label="Populist era"), hjust=-0.1, vjust=1.4, color = pop.text, lineheight = 1)+
  geom_text(aes(x=as.Date("2008-01-15"), y=Inf, label="Pre-populist era"), hjust=-0.3, vjust=1.4, color = gta_colour$grey[1], lineheight = 1)+
  geom_line(aes(x=date, y=value,col=jurisdiction), size=1) +
  gta_theme() +
  theme(legend.title = element_blank())+
  scale_color_manual(labels = c("China", "Eurozone", "Japan", "USA"),
                     values = c(gta_colour$qualitative[1:4])) +
  labs(y="OECD Business confidence index\n(accessed 13 December 2019)", x ="Year") +
  scale_x_date(breaks = as.Date(paste0(c(2008:2019),'-01-15')), labels = scales::date_format("%Y"),
               limits = as.Date(c("2008-01-15","2019-11-15")), expand=c(0,0))


fig1



gta_plot_saver(plot = fig1,
               path = figure.path,
               name = paste0("Figure 1 - Business confidence index"),
               cairo_ps = T,
               pdf = T,
               height = 17,
               width = 21)


