
dci.active.inactive.tables<- data.table::fread("~/Downloads/ActiveInactive.csv")
head(dci.active.inactive.tables)

unique.districts <- unique(final.res$State.District.ID)

dci.active.inactive.districts <- unique(dci.active.inactive.tables$`State District ID`)

intersect.list <- intersect(dci.active.inactive.districts,unique.districts)

analyze.res <- res
dis.id <- "MO-001090"

analyze.res.dis.id <- analyze.res[State.District.ID == dis.id,]

dis.analyze.res.dis.id <- dci.active.inactive.tables[`State District ID`== dis.id,]

analyze.res$status = ""
analyze.res$Districts  <- ""

for(i in 1:nrow(analyze.res)) {
  row <- analyze.res[i,]
  year <- as.integer(sub('.*(?=.{2}$)', '', row$period, perl=T))
  year <- year-13
  dci.active.inactive.row <-  dci.active.inactive.tables[dci.active.inactive.tables$`State District ID` == row$State.District.ID]
  analyze.res[i,]$status <- as.character(dci.active.inactive.row[[year]])
  analyze.res[i,]$Districts <- dci.active.inactive.row$DISTRICTS
}



analyze.res.pt <- analyze.res[with(analyze.res, order(periods.freq,-district.sum)), ]

periods<- factor(analyze.res.pt$period, level = c("Aug17-Feb18"  ,"Mar18-July18" ,
                                      "Aug18-Feb19", "Mar19-July19" , "Aug19-Feb20" , "Mar20-July20" ,
                                      "Aug20-Feb21","Mar21-July21" , "Aug21-Feb22"))

level = c("Aug17-Feb18"  ,"Mar18-July18" ,
          "Aug18-Feb19", "Mar19-July19" , "Aug19-Feb20" , "Mar20-July20" ,
          "Aug20-Feb21","Mar21-July21" , "Aug21-Feb22")

analyze.res.pt$period <- factor(pt$period, levels =level)



ongoing_now <- ggplot(analyze.res.pt,aes(x = period, y = Districts , fill = log10(n.coaching.sessions),color=status)) +
  geom_tile(color = "black") +
  theme(strip.text.y = element_text(angle = 0)) + 
  geom_text(aes(label = n.coaching.sessions),  size = 3)  +
  scale_fill_gradient(low="#FFFAE6", high="#FCD116")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme_bw()

res_on_gg<- ongoing_now + facet_grid(periods.freq ~., scales = "free", space ="free") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


png(filename="~/Desktop/08-12-2021-saph.png",700,2000,res= 100)
print(res_on_gg)
dev.off()  



