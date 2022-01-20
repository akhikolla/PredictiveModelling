
periods <- c("Aug17-Feb18"  ,"Mar18-July18" ,
             "Aug18-Feb19", "Mar19-July19" , "Aug19-Feb20" , "Mar20-July20" ,
             "Aug20-Feb21","Mar21-July21" , "Aug21-Feb22")

head(res)
mar18.july18 <- res[periods.freq == 3 & period == "Mar18-July18" ]
mar19.july19 <- res[periods.freq == 3 & period == "Mar19-July19" ]
mar20.july20 <- res[periods.freq == 3 & period == "Mar20-July20" ]
mar21.july21 <- res[periods.freq == 3 & period == "Mar21-July21" ]

mar18.july18 <- res[periods.freq == 1 & period == "Mar18-July18" ]
mar19.july19 <- res[periods.freq == 1 & period == "Mar19-July19" ]
mar20.july20 <- res[periods.freq == 1 & period == "Mar20-July20" ]
mar21.july21 <- res[periods.freq == 1 & period == "Mar21-July21" ]
freq.sums.march.july <- sum(res[periods.freq == 1]$n.coaching.sessions)


mar18.july18 <- res[periods.freq == 2 & period == "Mar18-July18" ]
mar19.july19 <- res[periods.freq == 2 & period == "Mar19-July19" ]
mar20.july20 <- res[periods.freq == 2 & period == "Mar20-July20" ]
mar21.july21 <- res[periods.freq == 2 & period == "Mar21-July21" ]

offperiodsValues <-list()
for(i in 1:8){
  mar18.july18 <- res[periods.freq == i & period == "Mar18-July18" ]
  mar19.july19 <- res[periods.freq == i & period == "Mar19-July19" ]
  mar20.july20 <- res[periods.freq == i & period == "Mar20-July20" ]
  mar21.july21 <- res[periods.freq == i & period == "Mar21-July21" ]
  sums.march.july <- nrow(mar18.july18) + nrow(mar19.july19) + nrow(mar20.july20) + nrow(mar21.july21)
  offperiodsValues[i] <- sums.march.july
}

off.periods.values.dt <- do.call(rbind,offperiodsValues)

sums.march.july <- nrow(mar18.july18) + nrow(mar19.july19) + nrow(mar20.july20) + nrow(mar21.july21)

freq.sums.march.july <- sum(res[periods.freq == 3]$n.coaching.sessions)
  
unneccessary.coaching.data <- freq.sums.march.july - sums.march.july



