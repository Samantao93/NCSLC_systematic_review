setwd("path/busqueda")
library(readxl)
library(dplyr)

pubmed <- read.csv("./csv-Lungcancer-set.csv")
scopus <- read.csv("./scopus.csv")
wos <- read_excel("./savedrecs.xls")

pubmed2<-select(pubmed,PMID,DOI,Title,Authors) %>%
  mutate(Document.Type=NA,database2='PUBMED')
# table(is.na(pubmed2$DOI))

scopus2<-select(scopus,PMID=PubMed.ID,DOI,Title,Authors,Document.Type) %>%
  mutate(database2='SCOPUS')
# table(is.na(scopus2$PMID_scopus))

wos2<-select(wos,PMID="Pubmed Id",DOI,Title="Article Title",Authors,Document.Type="Document Type") %>%
  mutate(database2='WOS')
# table(is.na(wos2$DOI))
wos2$DOI[wos2$DOI=='10.1016/j.ebiom2202.104143']<-'10.1016/j.ebiom.2022.104143'
wos2$DOI[is.na(wos2$DOI)]<-c(1,2,3,4)

final<-rbind(pubmed2,scopus2,wos2)

counts<- final %>% group_by(DOI) %>% reframe(total=n())

table(counts$total)

# final2<-final[!duplicated(final$DOI),]

final_join<-full_join(wos2,scopus2,by=c("DOI"))
final_join<-full_join(final_join,pubmed2,by="DOI")
write.table(final_join,"./total_paper_all_db.csv",na="",row.names = F,sep=";")


doi_w<-data.frame(DOI=wos2$DOI)
doi_s<-data.frame(DOI=scopus2$DOI)
doi_p<-data.frame(DOI=pubmed2$DOI)

doi<-rbind(doi_w,doi_p,doi_s)
doi<-filter(doi,!is.na(DOI))
write.table(doi,"DOI_total.csv",row.names = F,na='',sep=';')

final<-doi %>% group_by(DOI) %>% summarise(total=n())
sum(final>1)

all<-final_join
all$Title<- ifelse(!is.na(all$`Title.x`), all$`Title.x`, ifelse(!is.na(all$`Title.y`), all$`Title.y`,all$`Title`))
all$Author<- ifelse(!is.na(all$Authors.x), all$Authors.x, ifelse(!is.na(all$Authors.y), all$Authors.y,all$Authors))
all$Database <- apply(all[, c("database2", "database2.x", "database2.y")], 1, function(x) paste(na.omit(x), collapse = " - "))
all$Document.Type<-ifelse(!is.na(all$Document.Type),all$Document.Type,ifelse(!is.na(all$Document.Type.x),all$Document.Type.x,ifelse(!is.na(all$Document.Type.y),all$Document.Type.y,"")))
all<-select(all,Document.Type,Title,Author,DOI,Database)

filtrado_bueno<-filter(all,Document.Type!= "Data set" & Document.Type!= "Meeting" & Document.Type!= "Meeting Abstract" & Document.Type!= "Conference paper" & Document.Type!= "Review" & Document.Type!= "Dissertation/Thesis" & Document.Type!= "Preprint" & Document.Type!= "Proceedings Paper")
write.table(filtrado_bueno,"./filtrado_bueno.csv",na="",row.names = F,sep=";")

filtrado_nobueno<-filter(all[!all$DOI %in% filtrado_bueno$DOI,])
write.table(filtrado_nobueno,"./filtrado_nobueno.csv",na="",row.names = F,sep=";")


