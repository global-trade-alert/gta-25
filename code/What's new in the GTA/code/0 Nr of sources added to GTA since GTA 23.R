
library(gtalibrary)
library(gtasql)
library(pool)
library(RMariaDB)
library(data.table)

gta_setwd()

bt_extract_url = function(string){

  urls=unique(unlist(stringr::str_extract_all(string, "((https?://)|(www\\.))[A-Za-z/\\.\\-_0-9]+")))

  if(length(urls)==0){urls=NA}

  if(get.tld){tld=str_extract(urls,"((https?://)|(www\\.))[A-Za-z\\.\\-_0-9]+")}

  return(urls)

}

gta_sql_pool_open(db.title="gtamain",
                  db.host = gta_pwd("gtamain")$host,
                  db.name = gta_pwd("gtamain")$name,
                  db.user = gta_pwd("gtamain")$user,
                  db.password = gta_pwd("gtamain")$password,
                  table.prefix = "gta_")

gta.sa=gta_sql_load_table("measure")

new=subset(gta.sa, creation.date>="2018-11-01")
new.urls=data.frame()


for(i in 1:nrow(new)){
  new.urls=rbind(new.urls,
                 data.frame(state.act.id=new$id[i],
                            url=bt_extract_url(new$source[i]),
                            stringsAsFactors = F))

  print(i)
}


new.urls$tld=str_extract(new.urls$url,"((https?://)|(www\\.))[A-Za-z\\.\\-_0-9]+")

new.urls$tld=gsub("(https?://)","",new.urls$tld)


length(unique(new.urls$url))
length(unique(new.urls$tld))

tlds=aggregate(url ~ tld, new.urls, function(x) length(unique(x)))
tlds=merge(tlds,
           aggregate(state.act.id ~ tld, new.urls, function(x) length(unique(x))),
           by="tld", all.x=T)
