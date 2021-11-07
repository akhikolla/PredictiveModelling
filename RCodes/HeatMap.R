library(tidyverse)
library(readxl)
library(ggtext)
library(RColorBrewer)

summary.dt$durationrange <- ""
for(i in 1:nrow(forcheck)){
  date <-summary.dt$Duration.of.Event[i]
  if(date < 5){
    val <- "half-day"
  }else if(date > 4 & date < 8){
    val <- "full-day"
  }else{
    val <- "Two-day"
  }
  summary.dt$durationrange[i] <- val 
}
data <- data[ !(menuitem == 'coffee' | amount <= 0),] 

summary.dt <- summary.dt[!(summary.dt$period == "Mar1-July1")]
summary.dt <- summary.dt[!(summary.dt$period == "Aug207-Feb208")]

summary.dt$month <- ""
for(i in 1:nrow(summary.dt)){
  date <-summary.dt$Date.of.Event.Visit[i]
  Month <- as.numeric(sapply(strsplit(as.character(date),'/'), "[", 1))
  if(Month == 1){
    val <- "Jan"
  }else if(Month == 2){
    val <- "Feb"
  }else if(Month == 3){
    val <- "Mar"
  }else if(Month == 4){
    val <- "Apr"
  }else if(Month == 5){
    val <-"May"
  }else if(Month == 6){
    val <- "Jun"
  }else if(Month == 7){
    val <- "Jul"
  }else if(Month == 8){
    val <- "Aug"
  }else if(Month == 9){
    val <- "Sept"
  }else if(Month == 10){
    val <- "Oct"
  }else if(Month == 11){
    val <- "Nov"
  }else if(Month == 12){
    val <- "Dec"
  }else{
    val <- "None"
  }
  summary.dt$month[i] <- val 
}


summary.dt <- kt.dt#kt.dt[,(noofcoachings=.N), by= c("State.District.ID")]



mine.heatmap <- ggplot(data = kt.dt, mapping = aes(x = period,
                                                   y = Duration.of.Event,
                                                   fill = State.District.ID)) +
  geom_tile() +
  xlab(label = "Sample")



png(filename="~/Desktop/figure_heatmap1.png",2000,10000)
print(mine.heatmap)
dev.off()

mine.heatmap
summary.dt <- kt.dt#kt.dt[,(noofcoachings=.N), by= c("State.District.ID")]
id.change <- "MO-025001"
summary.dt.id <- summary.dt[summary.dt$State.District.ID==id.change]

mine.heatmap <- ggplot(data = summary.dt.id, mapping = aes(x = period,
                                                   y = Duration.of.Event,
                                                   fill = State.District.ID)) +
  geom_tile() +
  xlab(label = "Sample")

summary.dt.head <- head(summary.dt,10)

mine.heatmap <- ggplot(data = summary.dt, mapping = aes(x = period,
                                                           y = State.District.ID,
                                                           fill = State.District.ID)) +
  geom_tile() + geom_text
  xlab(label = "Sample")

png(filename="~/Desktop/figure_heatmap_check.png",5000,10000)
print(mine.heatmap)
dev.off()

# set.seed(123)                                                     # Set seed for reproducibility
# data <- matrix(rnorm(100, 0, 10), nrow = 10, ncol = 10)           # Create example data
# colnames(data) <- paste0("col", 1:10)                             # Column names
# rownames(data) <- paste0("row", 1:10)
# heatmap(data)

district.count.tracker<- kt.dt[,(noofcoachings=.N), by= c("State.District.ID")]
colnames(district.count.tracker)[2] <- "count"
ordered.district.count.tracker<-district.count.tracker[order(-rank(count), State.District.ID)]

heat.data <- matrix(0,nrow = 190, ncol = 9)   
rownames(heat.data) <-ordered.district.count.tracker$State.District.ID

colnames(heat.data) <-c("Aug17-Feb18"  ,"Mar18-July18" ,
                    "Aug18-Feb19", "Mar19-July19" , "Aug19-Feb20" , "Mar20-July20" ,
                    "Aug20-Feb21","Mar21-July21" , "Aug21-Feb22")

message(sprintf("unique districts found : %s\n",paste(unique(kt.dt$period), collapse=", ")))

# message(sprintf("unique districts found : %s\n",paste(unique(ordered.district.count.tracker$State.District.ID), collapse=", ")))

for(itr in 1:nrow(summary.dt)){
  row.value <- summary.dt[itr,]
  heat.data[row.value$State.District.ID,row.value$period] <- heat.data[row.value$State.District.ID,row.value$period] + 1
}


png(filename="~/Desktop/figure_heatmap2.png",5000,10000)
print(heatmap(heat.data, Rowv = NA, Colv = NA) )
dev.off()

data = head(summary.dt,20)

x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(20, 0, 5)

# new column: text for tooltip:
  # data <- data %>%
  #   mutate(text = paste0("x: ", x, "\n", "y: ", y, "\n", "Value: ",round(Z,2), "\n", "What else?"))
  # 
  # # classic ggplot, with text in aes
  # p <- ggplot(data, aes(period,State.District.ID , fill= durationrange, text=text)) + 
  #   geom_tile() +
  #   theme_ipsum()
  # 
  # ggplotly(p, tooltip="text")
  # # Heatmap 
  # ggplot(head(summary.dt,20), aes(period, Duration.of.Event, fill= durationrange)) + 
  #   geom_tile()

heatmap(data, Rowv = NA, Colv = NA)    


heatmap(head(heat.data,40), Rowv = NA, Colv = NA)    

mat_letters = matrix(sample(letters[1:4], 100, replace = TRUE), 10)

dist_letters = function(x, y) {
  x = strtoi(charToRaw(paste(x, collapse = "")), base = 16)
  y = strtoi(charToRaw(paste(y, collapse = "")), base = 16)
  sqrt(sum((x - y)^2))
}
Heatmap(mat_letters, name = "letters", col = structure(2:5, names = letters[1:4]),
        clustering_distance_rows = dist_letters, clustering_distance_columns = dist_letters,
        cell_fun = function(j, i, x, y, w, h, col) { # add text to each grid
          grid.text(mat_letters[i, j], x, y)
        })

write.csv(heat.data,"/Users/akhilachowdarykolla/Desktop/norowsdata.csv", row.names = FALSE)
# norows eliminated outliers
write.csv(heat.data,"/Users/akhilachowdarykolla/Desktop/nooutliersdata.csv", row.names = FALSE)




library(devtools)
install_github("jokergoo/ComplexHeatmap")
library("ComplexHeatmap")

ha = HeatmapAnnotation(foo = anno_simple(heat.data))
draw(ha, 1:10)

library(ComplexHeatmap)
library(circlize)

df = data.frame(type = c(rep("a", 5), rep("b", 5)))
ha = HeatmapAnnotation(df = df)
ha


requiredid.MO_042118  <- summary.dt[summary.dt$State.District.ID == "MO-042118",]

gg.requiredid.MO_042118 <- ggplot(data=requiredid.MO_042118, aes(x=month,y=Duration.of.Event))+ 
  geom_point() + 
  facet_wrap(~State.District.ID,nrow=6,ncol=2 ,scales="free_x") + geom_hline(yintercept=4.0,linetype="dashed", color = "blue") +
  geom_hline(yintercept=8.0,linetype="dashed", color = "red")

mlevels <- c( "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
summary.dt$month <- factor(summary.dt$month,mlevels)




