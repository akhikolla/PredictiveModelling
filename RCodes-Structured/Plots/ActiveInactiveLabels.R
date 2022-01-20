
dci.active.inactive.tables<- data.table::fread("~/Downloads/ActiveInactive.csv")
head(dci.active.inactive.tables)

unique.districts <- unique(final.res$State.District.ID)

dci.active.inactive.districts <- unique(dci.active.inactive.tables$`State District ID`)

intersect.list <- intersect(dci.active.inactive.districts,unique.districts)

final.res <- res

final.res$status <- ""
final.res$Districts <- ""
## avoid it and perform vectorize operations
for(i in 1:nrow(final.res)) {
  row <- final.res[i,]
  year <- as.integer(sub('.*(?=.{2}$)', '', row$period, perl=T))
  year <- year-13
  dci.active.inactive.row <-  dci.active.inactive.tables[dci.active.inactive.tables$`State District ID` == row$State.District.ID]
  final.res[i,]$status <- as.character(dci.active.inactive.row[[y]])
  final.res[i,]$Districts <- dci.active.inactive.row$DISTRICTS
}

## use melt, id = state.district,period, measure= cols(years)(use patterns may be(regex))
## match all columns that has years, and join it with the final.res table.
## create a new columm for periods to years


## perform a join instead of 


# year.list <- c(18,19,20,21,22)
# year.list.index <- c(5,6,7,8,9)
# 
#   sub('.*(?=.{2}$)', '', x, perl=T)

library("ggplot2")
library("dplyr")

res.sorted <- res[with(res, order(periods.freq,district.sum)), ] 

res[
  order( res[,4], res[,5] ),
]



ongoing_now <- ggplot(final.res,aes(x = period, y = Districts , fill = n.coaching.sessions,color=status)) +
  geom_tile(color = "black") +
  theme(strip.text.y = element_text(angle = 0)) + 
  geom_text(aes(label = n.coaching.sessions),  size = 3)  +
  scale_fill_gradient(low="grey90", high="black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme_bw()

res_on_ggks<- ongoing_now + facet_grid(periods.freq ~., scales = "free", space ="free") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


png(filename="~/Desktop/24NovColorCheck.png",700,2000,res= 90)
print(res_on_ggks)
dev.off()  


pt <- final.res[with(final.res, order(periods.freq,-district.sum)), ]

spt <- dplyr::arrange(final.res, periods.freq, (district.sum))

periods<- factor(pt$period, level = c("Aug17-Feb18"  ,"Mar18-July18" ,
                                           "Aug18-Feb19", "Mar19-July19" , "Aug19-Feb20" , "Mar20-July20" ,
                                           "Aug20-Feb21","Mar21-July21" , "Aug21-Feb22"))

level = c("Aug17-Feb18"  ,"Mar18-July18" ,
          "Aug18-Feb19", "Mar19-July19" , "Aug19-Feb20" , "Mar20-July20" ,
          "Aug20-Feb21","Mar21-July21" , "Aug21-Feb22")

pt$period <- factor(pt$period, levels =level)
## geom_tile  fill = -log10(n.coaching.sessions),color=status

ongoing_now <- ggplot(pt,aes(x = period, y = Districts , fill = -log10(n.coaching.sessions),color=status)) +
  geom_tile(color = "black") +
  theme(strip.text.y = element_text(angle = 0)) + 
  geom_text(aes(label = n.coaching.sessions),  size = 3)  +
  scale_fill_gradient(low="#101820FF", high="#F2AA4CFF")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme_bw()

res_on_ggksk<- ongoing_now + facet_grid(periods.freq ~., scales = "free", space ="free") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

test.graphs <- res_on_ggks + scale_x_discrete(expand=c(0,0),
                 breaks=c("Aug17-Feb18"  ,"Mar18-July18" ,
                          "Aug18-Feb19", "Mar19-July19" , "Aug19-Feb20" , "Mar20-July20" ,
                          "Aug20-Feb21","Mar21-July21" , "Aug21-Feb22"))+  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  theme_grey(base_size=8)+ theme(
    #bold font for legend text
    legend.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.4),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank())


png(filename="~/Desktop/pt24NovColorCheck.png",700,2000,res= 90)
print(res_on_ggks)
dev.off()  

png(filename="~/Desktop/saph4.png",700,2000,res= 100)
print(res_on_ggksk)
dev.off()  


ongoing_now <- ggplot(pt,aes(x = period, y = Districts , fill = log10(n.coaching.sessions),color=status)) +
  geom_tile(color = "black") +
  theme(strip.text.y = element_text(angle = 0)) + 
  geom_text(aes(label = n.coaching.sessions),  size = 3)  +
  scale_fill_gradient(low="#FFFAE6", high="#FCD116")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme_bw()

res_on_gg<- ongoing_now + facet_grid(periods.freq ~., scales = "free", space ="free") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


png(filename="~/Desktop/saph8.png",700,2000,res= 100)
print(res_on_gg)
dev.off()  


