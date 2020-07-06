#load("Computations")
qs <- matrix(0.0, ncol = length(dates), nrow = 3)
qsc <- qs

tab <- matrix(0.0,15,21)
nops <- rep(c(15000,10000,5000),length(countries))
for(pop in 1:length(label)){
  tab[pop,1] <- nops[pop]
  tab[pop,2] <- round(mean(resCounts[start.date:length(dates),1:nrep, 11, pop]))
  tab[pop,3] <- round(mean(resCounts[start.date:length(dates),1:nrep, 12, pop]))
  
  #30 day mortality
  tmp <- resCounts[, , 5,pop]
  
  tmpCum <- tmp
  for (t in (start.date + 1):length(dates)) {
    tmpCum[t, ] <- tmpCum[t - 1, ] + tmp[t, ]
  }
  for (t in start.date:length(dates)) {
    qs[, t] <- quantile(tmp[t,], probs = c(0.05, 0.5, 0.95))
    qsc[, t] <- quantile(tmpCum[t,], probs = c(0.05, 0.5, 0.95))
  }
  tab[pop,4] <- round(qsc[2,which(dates=="2020-12-31")])
  tab[pop,5] <- round(qsc[1,which(dates=="2020-12-31")])
  tab[pop,6] <- round(qsc[3,which(dates=="2020-12-31")])
  tab[pop,7] <- round(qsc[2,which(dates=="2021-07-01")])
  tab[pop,8] <- round(qsc[1,which(dates=="2021-07-01")])
  tab[pop,9] <- round(qsc[3,which(dates=="2021-07-01")])
  tab[pop,10] <- round(qsc[2,which(dates=="2022-07-01")])
  tab[pop,11] <- round(qsc[1,which(dates=="2022-07-01")])
  tab[pop,12] <- round(qsc[3,which(dates=="2022-07-01")])
  
  # pulmonary complications
  tmp <- resCounts[, , 10,pop]
  
  tmpCum <- tmp
  for (t in (start.date + 1):length(dates)) {
    tmpCum[t, ] <- tmpCum[t - 1, ] + tmp[t, ]
  }
  for (t in start.date:length(dates)) {
    qs[, t] <- quantile(tmp[t,], probs = c(0.05, 0.5, 0.95))
    qsc[, t] <- quantile(tmpCum[t,], probs = c(0.05, 0.5, 0.95))
  }
  tab[pop,13] <- round(qsc[2,which(dates=="2020-12-31")])
  tab[pop,14] <- round(qsc[1,which(dates=="2020-12-31")])
  tab[pop,15] <- round(qsc[3,which(dates=="2020-12-31")])
  tab[pop,16] <- round(qsc[2,which(dates=="2021-07-01")])
  tab[pop,17] <- round(qsc[1,which(dates=="2021-07-01")])
  tab[pop,18] <- round(qsc[3,which(dates=="2021-07-01")])
  tab[pop,19] <- round(qsc[2,which(dates=="2022-07-01")])
  tab[pop,20] <- round(qsc[1,which(dates=="2022-07-01")])
  tab[pop,21] <- round(qsc[3,which(dates=="2022-07-01")])
}


df <- data.frame(country.pop,tab)

colnames(df) <- c("country","# surg per 100000 person years",
                  "daily surgeries on infected",
                  "daily surgeries on infected >= 70 years",
                  "30-day mortality, 2020-07-01 -> 2020-12-31, median",
                  "30-day mortality, 2020-07-01 -> 2020-12-31, q05",
                  "30-day mortality, 2020-07-01 -> 2020-12-31, q95",
                  "30-day mortality, 2020-07-01 -> 2021-07-01, median",
                  "30-day mortality, 2020-07-01 -> 2021-07-01, q05",
                  "30-day mortality, 2020-07-01 -> 2021-07-01, q95",
                  "30-day mortality, 2020-07-01 -> 2022-07-01, median",
                  "30-day mortality, 2020-07-01 -> 2022-07-01, q05",
                  "30-day mortality, 2020-07-01 -> 2022-07-01, q95",
                  "pulmonary complications, 2020-07-01 -> 2020-12-31, median",
                  "pulmonary complications, 2020-07-01 -> 2020-12-31, q05",
                  "pulmonary complications, 2020-07-01 -> 2020-12-31, q95",
                  "pulmonary complications, 2020-07-01 -> 2021-07-01, median",
                  "pulmonary complications, 2020-07-01 -> 2021-07-01, q05",
                  "pulmonary complications, 2020-07-01 -> 2021-07-01, q95",
                  "pulmonary complications, 2020-07-01 -> 2022-07-01, median",
                  "pulmonary complications, 2020-07-01 -> 2022-07-01, q05",
                  "pulmonary complications, 2020-07-01 -> 2022-07-01, q95")


writexl::write_xlsx(df,path="dynamics_table.xlsx")
