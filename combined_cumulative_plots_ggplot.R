load("Computations")
library(ggplot2)
library(gridExtra)

qs <- matrix(0.0, ncol = length(dates), nrow = 3)
qsc <- qs

k <- 1

cols <- c(4,2,2,1,3)

df <- data.frame(time=dates[start.date:length(dates)])
dfp <- data.frame(time=dates[start.date:length(dates)])

combined.m <- 0*resCounts[, , 5,1]
combined.p <- combined.m

qtab <- matrix(0.0,6,12)



for(c in 1:length(countries)){
  pop <- (c-1)*3 + 1
  #30 day mortality
  tmp <- resCounts[, , 5,pop]
  tmpCum <- tmp
  
  
  for (t in (start.date + 1):length(dates)) {
    tmpCum[t, ] <- tmpCum[t - 1, ] + tmp[t, ]
  }
  
  qtab[c,7:9] <- quantile(tmpCum[which(dates=="2021-07-01"),],prob=c(0.05,0.5,0.95))
  qtab[c,10:12] <- quantile(tmpCum[which(dates=="2022-07-01"),],prob=c(0.05,0.5,0.95))
  
  combined.m <- combined.m + tmpCum
  
  for (t in start.date:length(dates)) {
    qs[, t] <- quantile(tmp[t,], probs = c(0.05, 0.5, 0.95))
    qsc[, t] <- quantile(tmpCum[t,], probs = c(0.05, 0.5, 0.95))
  }
  
  
  tmpdf <-
    data.frame(t(qsc[, start.date:length(dates)]))
  colnames(tmpdf) <- c(paste0(countries[c],"_q005"),
                       paste0(countries[c],"_median"),
                       paste0(countries[c],"_q095"))
  df <- cbind(df,tmpdf)
  
  # pulmonary complications
  tmp <- resCounts[, , 10,pop]
  
  tmpCum <- tmp
  for (t in (start.date + 1):length(dates)) {
    tmpCum[t, ] <- tmpCum[t - 1, ] + tmp[t, ]
  }
  
  qtab[c,1:3] <- quantile(tmpCum[which(dates=="2021-07-01"),],prob=c(0.05,0.5,0.95))
  qtab[c,4:6] <- quantile(tmpCum[which(dates=="2022-07-01"),],prob=c(0.05,0.5,0.95))
  
  
  combined.p <- combined.p + tmpCum
  
  for (t in start.date:length(dates)) {
    qs[, t] <- quantile(tmp[t,], probs = c(0.05, 0.5, 0.95))
    qsc[, t] <- quantile(tmpCum[t,], probs = c(0.05, 0.5, 0.95))
  }
  
  
  tmpdf <-
    data.frame(t(qsc[, start.date:length(dates)]))
  colnames(tmpdf) <- c(paste0(countries[c],"_q005"),
                       paste0(countries[c],"_median"),
                       paste0(countries[c],"_q095"))
  dfp <- cbind(dfp,tmpdf)

}


  
p1 <- ggplot(dfp,aes(x=time,y=AUS_median)) + 
  geom_line(lwd=1.5,color="black") +
  
   geom_line(aes(x=time,y=CAN_median),color="blue",data=dfp,lwd=1.5) +
 
   geom_line(aes(x=time,y=EU27_median),color="red",data=dfp,lwd=1.5) +
   
   geom_line(aes(x=time,y=UK_median),color="green",data=dfp,lwd=1.5) +
   geom_line(aes(x=time,y=US_median),color="cyan",data=dfp,lwd=1.5) +
    theme_bw() +
    
    geom_ribbon(aes(ymin=AUS_q005,ymax=AUS_q095),alpha=0.2,fill="black",linetype=0) +
  geom_ribbon(aes(ymin=CAN_q005,ymax=CAN_q095),alpha=0.2,fill="blue",linetype=0) +  
  geom_ribbon(aes(ymin=EU27_q005,ymax=EU27_q095),alpha=0.2,fill="red",linetype=0) +
    geom_ribbon(aes(ymin=UK_q005,ymax=UK_q095),alpha=0.2,fill="green",linetype=0) +
   geom_ribbon(aes(ymin=US_q005,ymax=US_q095),alpha=0.2,fill="cyan",linetype=0) +
  coord_cartesian(ylim=c(0,150000))  +
  scale_y_continuous(breaks=seq(0,150000,5000)) +
  ggtitle("cumulative pulmonary complications") +
  ylab("") + 
guides(color=guide_legend(override.aes=list(fill=NA)))+
  scale_color_manual(name = "",
                       breaks = countries,
                       values=c("AUS"="black",
                                "CAN"="blue",
                                "EU27"="red",
                                "UK"="green",
                                "US"="cyan")) 
#  theme(legend.position = "none")
#    guides(color=guide_legend(override.aes=list(fill=NA)))+

p2 <-  ggplot(df,aes(x=time,y=AUS_median,color="AUS")) + 
  geom_line(lwd=1.5) +
  
  geom_line(aes(x=time,y=CAN_median,color="CAN"),data=df,lwd=1.5) +
  
  geom_line(aes(x=time,y=EU27_median,color="EU27"),data=df,lwd=1.5) +
  
  geom_line(aes(x=time,y=UK_median,color="UK"),data=df,lwd=1.5) +
  geom_line(aes(x=time,y=US_median,color="US"),data=df,lwd=1.5) +
  theme_bw() +
  
  geom_ribbon(aes(ymin=AUS_q005,ymax=AUS_q095),alpha=0.2,fill="black",linetype=0) +
  geom_ribbon(aes(ymin=EU27_q005,ymax=EU27_q095),alpha=0.2,fill="red",linetype=0) +
  geom_ribbon(aes(ymin=CAN_q005,ymax=CAN_q095),alpha=0.2,fill="blue",linetype=0) +
  geom_ribbon(aes(ymin=UK_q005,ymax=UK_q095),alpha=0.2,fill="green",linetype=0) +
  geom_ribbon(aes(ymin=US_q005,ymax=US_q095),alpha=0.2,fill="cyan",linetype=0) +
  coord_cartesian(ylim=c(0,50000))  + 
  scale_y_continuous(breaks=seq(0,50000,2500)) +
  ggtitle("cumulative 30 day mortality") +
  ylab("")+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  scale_color_manual(name = "",
                     breaks = countries,
                     values=c("AUS"="black",
                              "CAN"="blue",
                              "EU27"="red",
                              "UK"="green",
                              "US"="cyan")) 



#p3 <- grid.arrange(p2,p1,ncol=2)

ggsave("cumulative_plot.pdf",plot=arrangeGrob(p1,p2,ncol=2),width=14,height = 7)
#legend(dates[start.date],45000,legend = c("AUS","EU27","UK","US"),col=c(4,2,1,3),lty=c(1,1,1,1),lwd=c(2,2,2,2))

print("mortality")
qtab[6,7:9] <- quantile(combined.m[which(dates=="2021-07-01"),],prob=c(0.05,0.5,0.95))
qtab[6,10:12] <- quantile(combined.m[which(dates=="2022-07-01"),],prob=c(0.05,0.5,0.95))
print(qtab[,7:12])
print("pulmonary")
qtab[6,1:3] <- quantile(combined.p[which(dates=="2021-07-01"),],prob=c(0.05,0.5,0.95))
qtab[6,4:6] <- quantile(combined.p[which(dates=="2022-07-01"),],prob=c(0.05,0.5,0.95))
print(qtab[,1:6])

qtab <- rbind(qtab[,1:6],qtab[,7:12])

writexl::write_xlsx(as.data.frame(qtab),path="cumulative_quantiles.xlsx")


#dev.off()

