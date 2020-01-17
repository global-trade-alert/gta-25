gta25_setup <- function(internal.name=NULL,
                        in.dev=F,
                        author=NULL,
                        wipe.data=F,
                        wipe.figs=T){
  
  
  ## Cutoffs and definitions
  source('0 report production/GTA 25/help files/GTA 25 cutoff and definitions.R')
  
  ## chapter settings incl. report name, report chapter number and figures to produce
  producer.console=data.frame(internal.name="Sectoral chapters panel 1",
                              report.number=99,
                              report.name="Contrasting the Populist era",
                              create.fig.script="1 Create figures.R",
                              create.fig.numbers=c(5:8),
                              stringsAsFactors = F)
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="Sectoral chapters panel 2",
                              report.number=99,
                              report.name="Contrasting the Populist era",
                              create.fig.script="1 Create figures.R",
                              create.fig.numbers=c(5:7),
                              stringsAsFactors = F))
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="Sectoral chapters panel 3-4",
                                    report.number=99,
                                    report.name="Intra-G20 barriers and reform",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="Single & multi-country hits",
                                    report.number=999,
                                    report.name="Three phases of targeted protectionism",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="Sectoral trade statistics",
                                    report.number=999,
                                    report.name="Sectoral trade statistics",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="Single & multi-country hits - shifting",
                                    report.number=2,
                                    report.name="Shifting commercial policy trends",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="Single & multi-country hits - trade shares",
                                    report.number=3,
                                    report.name="Populist era trade discrimination",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="USA, China and EU targeting G20",
                                    report.number=5,
                                    report.name="G20 exposure to major powers",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="Targeting LDCs etc",
                                    report.number=6,
                                    report.name="Harm to developing country interests",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="business concerns",
                                    report.number=7,
                                    report.name="Why business is so concerned",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="Annual trade variation",
                                    report.number=99,
                                    report.name="Variance in sectoral to total trade",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="What's new",
                                    report.number=15,
                                    report.name="What's new in the GTA",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="Jumbos in the populist era",
                                    report.number=4,
                                    report.name="Jumbo protectionism in the populist era",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="annex - p. 1 - title tables",
                                    report.number=99,
                                    report.name="annex - p. 1 - title tables",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="annex - p. 2 - maps",
                                    report.number=99,
                                    report.name="annex - p. 2 - maps",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="annex - p. 3 & 4 - top - criterion charts",
                                    report.number=99,
                                    report.name="annex - p. 3 & 4 - top - criterion charts",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="annex - p. 3 & 4 - bottom - bar charts",
                                    report.number=99,
                                    report.name="annex - p. 3 & 4 - bottom - bar charts",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  producer.console=rbind(producer.console,
                         data.frame(internal.name="front matter",
                                    report.number=0,
                                    report.name="front matter",
                                    create.fig.script="1 Create figures.R",
                                    create.fig.numbers=c(5:8),
                                    stringsAsFactors = F))
  
  ## set up data and figure folders
  
  if(in.dev){
    
    if(is.null(author)){
      c("ks","pb","pl", "jf")[menu(c("ks","pb","pl", "jf"), title="Identify thyself!")]
      
    } else{
      
      if(! tolower(author) %in% c("ks","pb","pl", "jf")){
        
        c("ks","pb","pl", "jf")[menu(c("ks","pb","pl", "jf"), title="Identify thyself!")]
        
      } else {
        
        root.path=paste0("0 dev/gta-25-",author)
        figure.path=paste0(root.path, "/tables & figures/",tolower(internal.name),"/")
          
      }
      
    }
    
    
  } else {
    
    root.path='0 report production/GTA 25'
    ch.nr=unique(producer.console$report.number[producer.console$internal.name==internal.name])
    ch.name=unique(producer.console$report.name[producer.console$internal.name==internal.name])
    
    figure.path=paste0(root.path, "/tables & figures/", ch.nr," - ",ch.name,"/")
    
  }
  
  data.path=paste0(root.path, "/data/",internal.name,"/")
  
  
  # ensuring paths are there
  dir.create(file.path(data.path), showWarnings = FALSE)
  dir.create(file.path(figure.path), showWarnings = FALSE)
  
  # wipe figures
  if(wipe.figs){
    
    wipe.all= list.files(figure.path, include.dirs = F, full.names = T, recursive = T)
    file.remove(wipe.all)
    rm(wipe.all)
    
  }
  
  # wipe data
  if(wipe.data){
    
    wipe.all= list.files(data.path, include.dirs = F, full.names = T, recursive = T)
    file.remove(wipe.all)
    rm(wipe.all)
    
  }
  
  output.paths=list("data.path"=data.path,
                    "figure.path"=figure.path)
  
  return(output.paths)
}



gta25_sync_prd <- function(){
  unlink('../GTA report production/GTA 25/tables & figures', recursive = T)
  file.copy('0 report production/GTA 25/tables & figures', '../GTA report production/GTA 25', recursive=TRUE)
  
}

