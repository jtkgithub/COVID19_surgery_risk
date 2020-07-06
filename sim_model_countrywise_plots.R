
load("Computations")

for(pop in 1:length(label)){

qs <- matrix(0.0, ncol = length(dates), nrow = 3)
qsc <- qs

pdf(paste0("dynamics_plots_",label[pop],".pdf"),height = 14,width = 10)
par(mfrow = c(3, 2))
for (t in 1:length(dates)) {
  qs[, t] <- quantile(resInf[t,], probs = c(0.05, 0.5, 0.95))
}
print(mean(resInf[start.date:length(dates),]))

# proportion of population infected
tmpdf <-
  data.frame(t(qs[, start.date:length(dates)]), dates[start.date:length(dates)])
colnames(tmpdf) <- c("q005", "median", "q095", "time")
plot(
  tmpdf$median ~ tmpdf$time,
  type = "l",
  main = "r(t)",
  ylim = c(0, max(qs[3, ])),
  xlab = "",
  ylab = "fraction of population",
  xaxt = "n"
)
text(labels="A",x=tmpdf$time[10],y=max(qs[3, ]),cex=2)
xlabels <- strptime(tmpdf$time, format = "%Y-%m-%d")
axis.Date(1, at = xlabels[xlabels$mday == 1], format = "%b-%y")
lines(tmpdf$q005 ~ tmpdf$time, type = "l", col = "red")
lines(tmpdf$q095 ~ tmpdf$time, type = "l", col = "red")


# total surgeries on infected
tmp <- resCounts[, , 11,pop]
for (t in start.date:length(dates)) {
  qs[, t] <- quantile(tmp[t,], probs = c(0.05, 0.5, 0.95))
}

tmpdf <-
  data.frame(t(qs[, start.date:length(dates)]), dates[start.date:length(dates)])
colnames(tmpdf) <- c("q005", "median", "q095", "time")
plot(
  tmpdf$median ~ tmpdf$time,
  type = "l",
  main = "daily surgeries on asymptomatic infected",
  ylim = c(0, max(qs[3, ])),
  xlab = "",
  ylab = "daily # cases",
  xaxt = "n"
)
text(labels="B",x=tmpdf$time[10],y=max(qs[3, ]),cex=2)
xlabels <- strptime(tmpdf$time, format = "%Y-%m-%d")
axis.Date(1, at = xlabels[xlabels$mday == 1], format = "%b-%y")
lines(tmpdf$q005 ~ tmpdf$time, type = "l", col = "red")
lines(tmpdf$q095 ~ tmpdf$time, type = "l", col = "red")


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

tmpdf <-
  data.frame(t(qs[, start.date:length(dates)]), dates[start.date:length(dates)])
colnames(tmpdf) <- c("q005", "median", "q095", "time")
plot(
  tmpdf$median ~ tmpdf$time,
  type = "l",
  main = "pulmonary complications, daily",
  ylim = c(0, max(qs[3, ])),
  xlab = "",
  ylab = "daily # cases",
  xaxt = "n"
)
text(labels="C",x=tmpdf$time[10],y=max(qs[3, ]),cex=2)
xlabels <- strptime(tmpdf$time, format = "%Y-%m-%d")
axis.Date(1, at = xlabels[xlabels$mday == 1], format = "%b-%y")
lines(tmpdf$q005 ~ tmpdf$time, type = "l", col = "red")
lines(tmpdf$q095 ~ tmpdf$time, type = "l", col = "red")

#mtext(paste0("Country/reg"),side=1,line=0.3*max(qs[3, ]),at=tmpdf$time[10])



tmpdf <-
  data.frame(t(qsc[, start.date:length(dates)]), dates[start.date:length(dates)])
colnames(tmpdf) <- c("q005", "median", "q095", "time")
plot(
  tmpdf$median ~ tmpdf$time,
  type = "l",
  main = "pulmonary complications, cumulative",
  ylim = c(0, max(qsc[3, ])),
  xlab = "",
  ylab = "# cases",
  xaxt = "n"
)
text(labels="D",x=tmpdf$time[10],y=max(qsc[3, ]),cex=2)
xlabels <- strptime(tmpdf$time, format = "%Y-%m-%d")
axis.Date(1, at = xlabels[xlabels$mday == 1], format = "%b-%y")
lines(tmpdf$q005 ~ tmpdf$time, type = "l", col = "red")
lines(tmpdf$q095 ~ tmpdf$time, type = "l", col = "red")


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

tmpdf <-
  data.frame(t(qs[, start.date:length(dates)]), dates[start.date:length(dates)])
colnames(tmpdf) <- c("q005", "median", "q095", "time")
plot(
  tmpdf$median ~ tmpdf$time,
  type = "l",
  main = "30 day mortality, daily",
  ylim = c(0, max(qs[3, ])),
  xlab = "",
  ylab = "daily # cases",
  xaxt = "n"
)
text(labels="E",x=tmpdf$time[10],y=max(qs[3, ]),cex=2)
xlabels <- strptime(tmpdf$time, format = "%Y-%m-%d")
axis.Date(1, at = xlabels[xlabels$mday == 1], format = "%b-%y")
lines(tmpdf$q005 ~ tmpdf$time, type = "l", col = "red")
lines(tmpdf$q095 ~ tmpdf$time, type = "l", col = "red")

tmpdf <-
  data.frame(t(qsc[, start.date:length(dates)]), dates[start.date:length(dates)])
colnames(tmpdf) <- c("q005", "median", "q095", "time")
plot(
  tmpdf$median ~ tmpdf$time,
  type = "l",
  main = "30 day mortality, cumulative",
  ylim = c(0, max(qsc[3, ])),
  xlab = "",
  ylab = "# cases",
  xaxt = "n"
)
text(labels="F",x=tmpdf$time[10],y=max(qsc[3, ]),cex=2)
xlabels <- strptime(tmpdf$time, format = "%Y-%m-%d")
axis.Date(1, at = xlabels[xlabels$mday == 1], format = "%b-%y")
lines(tmpdf$q005 ~ tmpdf$time, type = "l", col = "red")
lines(tmpdf$q095 ~ tmpdf$time, type = "l", col = "red")





dev.off()
}
