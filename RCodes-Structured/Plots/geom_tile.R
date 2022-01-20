
ggk<-ggplot(summary.dt, aes(x = period, y = State.District.ID , fill = Duration.of.Event)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Duration.of.Event), color = "white", size = 3) +  coord_fixed()
#annotate("text",label="Duration.of.Event", color = "white", size = 0.5)+

melt(heat.data)
some.coaching <- data.table(
  State.District.ID=rownames(heat.data)[as.integer(row(heat.data))],
  period=colnames(heat.data)[as.integer(col(heat.data))],
  n.coaching.sessions=as.integer(heat.data)
)[n.coaching.sessions > 0]

ntestcol<-as.data.table(ordered.district.count.tracker[,1])
ntestcol$periods.freq <- ""
ntestcol$district.sum <- ""

heat.data.count <- list()
ord.count.tracker <- ordered.district.count.tracker$State.District.ID
for(heat in 1:length(ord.count.tracker)){
  row.value <- testcol[testcol$State.District.ID == ord.count.tracker[heat],]
  count = 0
  for(per in periods){
    if(nrow(row.value[row.value$period == per,]) > 0){
      count = count + 1
    }
  }
  ntestcol$district.sum[heat] <- sum(row.value$n.coaching.sessions)
  ntestcol$periods.freq[heat] <- count
}

ntestcol[order(rank(periods.freq,district.sum), )]

testcol <- some.coaching
testcol$periods.freq <- ""
testcol$district.sum <- ""
for(i in 1:nrow(testcol)){
  print(i)
  row.value <- testcol[i,]
  ntest.row.value <- ntestcol[ntestcol$State.District.ID == row.value$State.District.ID,]
  testcol$district.sum[i]  <- ntest.row.value$district.sum
  testcol$periods.freq[i] <- ntest.row.value$periods.freq
}




some.coachings<-some.coaching[order(-rank(periods.freq,periods.freq),)]
sort(some.coachings) 

res <- testcol[with(testcol, order(periods.freq)), ] 

data_ordered <- data                                 # Replicate example data
setorder(data_ordered, x2, x3)                       # Order data with data.table
data_ordered


library(forcats)

cols.value <-ordered.district.count.tracker$State.District.ID

rows.value <-c("Aug17-Feb18"  ,"Mar18-July18" ,
                        "Aug18-Feb19", "Mar19-July19" , "Aug19-Feb20" , "Mar20-July20" ,
                        "Aug20-Feb21","Mar21-July21" , "Aug21-Feb22")


graph.ms <- res %>%
  mutate(period = fct_reorder(period,rows.value),
         State.District.ID = fct_reorder(State.District.ID,cols.value)) %>%  
  
  
ongoing_ggks <- ggplot(res,aes(x = period, y = State.District.ID , fill = n.coaching.sessions)) +
  geom_tile(color = "black") +
  theme(strip.text.y = element_text(angle = 0)) + 
  geom_text(aes(label = n.coaching.sessions),  size = 3)  +
  scale_fill_gradient(low="grey90", high="red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme_bw()

res_on_ggks<- ongoing_ggks + facet_grid(periods.freq ~., scales = "free", space ="free") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#facet_wrap(~ set, ncol = 1,  drop=TRUE)
png(filename="~/Desktop/22NovNowGridPlot-heatmap.png",500,2000)
print(res_on_ggks)
dev.off()  

scale_color_manual(values = c("active" = "#808000", "not active" = "#008080")) + 
  
ongoing_now <- ggplot(final.res,aes(x = period, y = Districts , fill = n.coaching.sessions,color=status)) +
  geom_tile(color = "black") +
  theme(strip.text.y = element_text(angle = 0)) + 
  geom_text(aes(label = n.coaching.sessions),  size = 3)  +
  scale_fill_gradient(low="grey90", high="black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme_bw()

res_on_ggks<- ongoing_now + facet_grid(periods.freq ~., scales = "free", space ="free") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


png(filename="~/Desktop/24NovColorCheck.png",400,2000,res= 1200)
print(res_on_ggks)
dev.off()  

# new_scale <- function(new_aes) {
#   structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
# }
# 
# res_on_ggks + new_scale("fill") + geom_tile(aes(fill = status), data = final.res) 


p <- ggplot()
box.highlited.tiles <- res_on_ggks + geom_tile(data=final.res, aes(colour=status))

                        png(filename="~/Desktop/23NovHighlightedCheck.png",500,2000)
                        print(box.highlited.tiles)
                        dev.off()                         
                        
      
      # + geom_tile(data=psT_SST[[1]], aes(x=as.numeric(N2), y=as.numeric(N1), fill=mean_RMSE, colour="blue")))




#facet_wrap(~ set, ncol = 1,  drop=TRUE)






res_one <- res[periods.freq == 1]
ongoing_ggks_one <- ggplot(res_one,aes(x = period, y = State.District.ID , fill = n.coaching.sessions)) +
  geom_tile(color = "black") + coord_fixed() +
  theme(strip.text.y = element_text(angle = 0)) +
  geom_text(aes(label = n.coaching.sessions),  size = 3)  +
  scale_fill_gradient(low="grey90", high="red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  
  scale_x_discrete(limits = (rows.value)) +
  scale_y_discrete(limits = unique(res_one$State.District.ID))

res_two <- res[periods.freq == 2]
ongoing_ggks_two <- ggplot(res_two,aes(x = period, y = State.District.ID , fill = n.coaching.sessions)) +
  geom_tile(color = "black") + coord_fixed() +
  theme(strip.text.y = element_text(angle = 0)) +
  geom_text(aes(label = n.coaching.sessions),  size = 3)  +
  scale_fill_gradient(low="grey90", high="red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  
  scale_x_discrete(limits = (rows.value)) +
  scale_y_discrete(limits = unique(res_two$State.District.ID))

one <- ongoing_ggks_one + facet_grid(periods.freq ~.) 
png(filename="~/Desktop/one-heatmap.png",3000,6000)
print(ongoing_ggks_one)
dev.off()  










  
ggks <- ggplot(res,aes(x = period, y = State.District.ID , fill = n.coaching.sessions)) +
  geom_tile(color = "black") +  
  geom_text(aes(label = n.coaching.sessions),  size = 10) +  coord_fixed() +
  scale_fill_gradient(low="grey90", high="red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  
  scale_x_discrete(limits = (rows.value)) +
  scale_y_discrete(limits = (cols.value)) +
  theme_bw()   


png(filename="~/Desktop/colsreschange-10-11-21-heatmap.png",2000,4000)
print(ggks)
dev.off()


america_heatmap <- heatmap(head(summary.dt), Rowv=NA, 
                           Colv=NA, col = brewer.pal(9, "Blues"), scale="column", 
                           margins=c(2,6))




geom_text(aes(label = Duration.of.Event), color = "white", size = 3) +
  annotate("text",label="Duration.of.Event", color = "white", size = 2)


p <- ggplot(summary.dt,aes(x=period,y=State.District.ID,fill=Duration.of.Event))+
  geom_tile()


base_size <- 9
p + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) 
+scale_y_discrete(expand = c(0, 0)) + opts(legend.position = "none",axis.ticks = theme_blank(),
axis.text.x = theme_text(size = base_size * 0.8, angle = 330, hjust = 0, colour = "grey50"))

library(gplots) # heatmap.2() function
library(plotrix) # gradient.rect() function

# convert from long format to wide format
m5 <- m3 %>% spread(key="state",value=count)
m6 <- as.matrix(m5[,-1])
rownames(m6) <- m5$year

# base heatmap
png(filename="measles-base.png",height=5.5,width=8.8,res=200,units="in")
heatmap(t(m6),Rowv=NA,Colv=NA,na.rm=T,scale="none",col=terrain.colors(100),
        xlab="",ylab="",main="Incidence of Measles in the US")
dev.off()

# gplots heatmap.2
png(filename="measles-gplot.png",height=6,width=9,res=200,units="in")
par(mar=c(2,3,3,2))
gplots::heatmap.2(summary.dt,na.rm=T,dendrogram="none",Rowv=NULL,Colv="Rowv",trace="none",scale="none",offsetRow=0.3,offsetCol=0.3,
                  breaks=c(-1,0,1,10,100,500,1000,max(m4$count,na.rm=T)),colsep=which(seq(1928,2003)%%10==0),
                  margin=c(3,8),col=rev(c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#ddf1da")),
                  xlab="",ylab="",key=F,lhei=c(0.1,0.9),lwid=c(0.2,0.8))
gradient.rect(0.125,0.25,0.135,0.75,nslices=7,border=F,gradient="y",col=rev(c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#ddf1da")))
text(x=rep(0.118,7),y=seq(0.28,0.72,by=0.07),adj=1,cex=0.8,labels=c("Aug17-Feb18"  ,"Mar18-July18" ,"Aug18-Feb19", "Mar19-July19" ,"Aug19-Feb20" , "Mar20-July20" ,"Aug20-Feb21","Mar21-July21" , "Aug21-Feb22"))
text(x=0.135,y=0.82,labels="Cases per\n100,000 people",adj=1,cex=0.85)
title(main="Incidence of Measles in the US",line=1,oma=T,adj=0.21)
dev.off()


#textcol <- "grey40"
# # further modified ggplot
# ps <- ggplot(summary.dt,aes(x=period,y=State.District.ID,fill=Duration.of.Event))+
#   geom_tile(colour="white",size=0.2)+
#   guides(fill=guide_legend(title="Cases per\n100,000 people"))+
#   labs(x="",y="",title="Incidence of Measles in the US")+
#   scale_y_discrete(expand=c(0,0))+
#   scale_x_discrete(expand=c(0,0),breaks=c("Aug17-Feb18"  ,"Mar18-July18" ,"Aug18-Feb19", "Mar19-July19" ,
#                                           "Aug19-Feb20" , "Mar20-July20" ,"Aug20-Feb21","Mar21-July21" , "Aug21-Feb22"))+
#   scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#ddf1da","#ddf1db"),na.value = "grey90")+
#   #coord_fixed()+
#   theme_grey(base_size=10)+
#   theme(legend.position="right",legend.direction="vertical",
#         legend.title=element_text(colour=textcol),
#         legend.margin=margin(grid::unit(0,"cm")),
#         legend.text=element_text(colour=textcol,size=7,face="bold"),
#         legend.key.height=grid::unit(0.8,"cm"),
#         legend.key.width=grid::unit(0.2,"cm"),
#         axis.text.x=element_text(size=10,colour=textcol),
#         axis.text.y=element_text(vjust=0.2,colour=textcol),
#         axis.ticks=element_line(size=0.4),
#         plot.background=element_blank(),
#         panel.border=element_blank(),
#         plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
#         plot.title=element_text(colour=textcol,hjust=0,size=14,face="bold"))
# 
# #export figure
# ggsave(ps,filename="~/Desktop/mod3.png",height=50,width=50,units="in",dpi=200,limitsize = FALSE)


p <- ggplot(summary.dt,aes(x=period,y=State.District.ID,fill=Duration.of.Event))+
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  #remove x and y axis labels
  labs(x="",y="")+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  #define new breaks on x-axis
  scale_x_discrete(expand=c(0,0),
                   breaks=c("Aug17-Feb18"  ,"Mar18-July18" ,
                            "Aug18-Feb19", "Mar19-July19" , "Aug19-Feb20" , "Mar20-July20" ,
                            "Aug20-Feb21","Mar21-July21" , "Aug21-Feb22"))+
  #set a base size for all fonts
  theme_grey(base_size=8)+
  #theme options
  theme(
    #bold font for legend text
    legend.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.4),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank())

#save with dpi 200
png(filename="~/Desktop/working-heatmap.png",5000,10000)
print(p)
dev.off()

#ggsave(p,filename="measles-mod1.png",height=5.5,width=8.8,units="in",dpi=200)

png(filename="~/Desktop/307-11-21-heatmap.png",5000,10000)
print(p)
dev.off()

#save plot to working directory

#save plot to working directory
ggsave(p,filename="~/Desktop/basic.png")

library(ggplot2)
library(hrbrthemes)
library(plotly)

# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)

# new column: text for tooltip:
data <- data %>%
  mutate(text = paste0("x: ", x, "\n", "y: ", y, "\n", "Value: ",round(Z,2), "\n", "What else?"))

# classic ggplot, with text in aes
p <- ggplot(data, aes(X, Y, fill= Z, text=text)) + 
  geom_tile() +
  theme_ipsum()

ggplotly(p, tooltip="text")




