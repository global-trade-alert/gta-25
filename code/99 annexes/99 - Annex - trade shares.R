library("xlsx")
library("gtalibrary")
rm(list = ls())


gta_setwd()
d.path="data/master_plus.Rdata"
r.path="data/database replica/database replica - parts - base.Rdata"

## setup
gta_setwd()
source('0 report production/GTA 25/help files/Producer console.R')

directories=gta25_setup(internal.name="annex - p. 1 - title tables",
                        in.dev=F,
                        author=NULL,
                        wipe.data=F,
                        wipe.figs=F)

data.path = directories$data.path
path <- directories$figure.path
gta_colour_palette()

chapters=c("D","E","F","G","I","L","M","P","TARIFF","X")
remove.ids = c(indian.2.3.exp.id)

for(cty in g20.member.names){
  if(cty=="South Korea"){cty="Republic of Korea"}
    gta_trade_coverage(coverage.period = c(2009,2019),
                     gta.evaluation = c("red","amber"),
                     implementation.period = c("2008-11-01",cutoff),
                     exporters=cty,
                     keep.exporters = T,
                     implementer.role = c("importer","3rd country"),
                     mast.chapters = chapters,
                     keep.mast = T,
                     group.mast = F,
                     intervention.ids = manually.removed.interventions,
                     keep.interventions = F,
                     data.path = d.path,
                     replica.path =r.path)
    
    trade.coverage.estimates=trade.coverage.estimates[,c(3,4,6:ncol(trade.coverage.estimates))]
    
    ## creating percentages
    for(i in 3:ncol(trade.coverage.estimates)){
      trade.coverage.estimates[,i]=sprintf("%.2f",round(trade.coverage.estimates[,i]*100,2))
    }
    
    # Adjusting names
    names(trade.coverage.estimates)[1:2]=c("UN MAST chapter", "Foreign discriminatory policy instrument")
    names(trade.coverage.estimates)=gsub("Trade coverage estimate for ","",names(trade.coverage.estimates))
    
    trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`Foreign discriminatory policy instrument`=="All included MAST chapters"]="All instruments"
    trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="D"]="Contingent trade protection"
    trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="E"]="Non-automatic licensing, quotas"
    trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="F"]="Price control measures"
    trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="G"]="Finance measures"
    trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="I"]="Trade-related investment measures"
    trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="L"]="Subsidies (excluding export subsidies)"
    trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="M"]="Government procurement"
    trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="P"]="Export measures"
    trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`Foreign discriminatory policy instrument`=="Tariff measures"]="Import tariff increases"
    trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`Foreign discriminatory policy instrument`=="Instrument unclear"]="Instrument unclassified"
    
    
    remove.ids=c("All included MAST chapters","X","TARIFF")
    trade.coverage.estimates$`UN MAST chapter`[trade.coverage.estimates$`UN MAST chapter` %in% remove.ids]=""
    
    
    xlsx::write.xlsx(trade.coverage.estimates, file=paste(path,cty,'.xlsx', sep=''), row.names = F)
    rm(trade.coverage.estimates)
  print(cty)
}

