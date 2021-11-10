library(ggplot2)
library(dplyr)
library(TSstudio)
library(xts)
library(magrittr)

library(ggplot2)
theme_set(theme_minimal())
# Demo dataset


ggplot(data = required.date.id, aes(y = Date.of.Event.Visit, x = State.District.ID))+
  geom_line(color = "#00AFBB", size = 2)
# Plot a subset of the data


ggplot(required.date.id, aes(x = Date.of.Event.Visit, y = State.District.ID)) + 
  geom_line(aes(color = State.District.ID), size = 1) +
  scale_color_manual(values = required.date.id$State.District.ID ) +
  theme_minimal()

scale_color_manual(values = c("A" = "black", "B" = "red"))

coachingdatecheck <- read.csv("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/Coaching logs Fall 2017- Spring 2021/Condensed columns-Table 1.csv")#,header = TRUE,check.names=TRUE,format = "%m/%d/%Y"  )
required.timeframe.cols <- coachingdatecheck[,c(1,4,5,8)]
head(required.timeframe.cols)

required.date.id <- data.table::data.table(coachingdatecheck[,c(4,8)])

it <- head(required.date.id,8)
nt <-dcast(setDT(it), rowid(State.District.ID) ~ State.District.ID, value.var = "Date.of.Event.Visit")[, State.District.ID := NULL][]
nt

nt <- as.data.frame(nt)
for(i in 1:ncol(nt)){
  nt[,i] <- as.POSIXct(nt[,i], format =  "%m/%d/%Y")
}

nt <- as.POSIXct(nt,"%m/%d/%Y")  
ts_plot(nt)

required.date.id$Date.of.Event.Visit <- gsub("00","20",as.Date(required.date.id$Date.of.Event.Visit, format =  "%m/%d/%Y"))
head(required.date.id)

required.date.id$Date.of.Event.Visit <- as.POSIXct(required.date.id$Date.of.Event.Visit,"%m/%d/%Y")   


required.date.id <- as.xts(required.date.id)
class(required.date.id$Date.of.Event.Visit)
class(required.date.id)






required.date.id$Date.of.Event.Visit <- as.POSIXlt(required.date.id$Date.of.Event.Visit,)
# y <- as.POSIXlt(required.date.id$Date.of.Event.Visit, format = "%m/%d/%Y")
# k <- gsub("00","20",y)
# k <- gsub("-","/",k)
# z <- as.Date(k, "%m/%d/%Y")

# required.date.id$Date.of.Event.Visit  <- as.POSIXlt(gsub("00","20",required.date.id$Date.of.Event.Visit), format = "%m/%d/%Y")


oo <- xts(required.date.id[-1], required.date.id[[1]])

ts_plot(oo,
        type = "multiple")

randomData <- rnorm(100)

months.ts <- ts(randomData,start=2015,end=2020, frequency = 4)

plot(months.ts)

lines(months.ts,col=2)





