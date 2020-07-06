rm(list=ls())
source("make_surg_rates.R")
infection.rate <- 0.01
prop.asympt <- 0.5
holmer.s16 <- c(7579,3375,2445,328)
wb.pop <- 1000*c(1210312,2655636,3022905,705417) # (https://blogs.worldbank.org/opendata/new-country-classifications-income-level-2018-2019 )

standard.pop <- 100000*c(1822+7033,8687, # notice last age bracket is 85+, and combining 0 and 1-4 classes
                         8597,
                         8474,
                         8222,
                         7928,
                         7605,
                         7145,
                         6590,
                         6038,
                         5371,
                         4547,
                         3723,
                         2955,
                         2210,
                         1515,
                         905,
                         632)
standard.pop <- sum(wb.pop)/sum(standard.pop)*standard.pop # scale to world bank population

# covidSurg probabilities
covidSD <- c(0.0,0.058,0.202,0.341,0.289,0.184)
covidSP <- c(0.291,0.442,0.540,0.560,0.572,0.469)


last.brac.rate <- (omling.data$pdr_all[18]*omling.data$nn[18] + omling.data$pdr_all[19]*omling.data$nn[19])/sum(omling.data$nn[18:19])
omling.pdr <- c(omling.data$pdr_all[1:17],last.brac.rate)




pop.wt <- wb.pop/sum(wb.pop)

tot.surg.vol <- sum(holmer.s16*pop.wt)
#print(paste0("total surgeries per 100000 per year",tot.surg.vol))

per.person.day <- tot.surg.vol/(100000*365)

total.surg.per.day <- sum(wb.pop)*per.person.day
#print(paste0("daily surgeries in the world ",total.surg.per.day))
c <- total.surg.per.day/sum(omling.pdr*standard.pop)

world.pdr <- c*omling.pdr

#print(paste0("check: ", sum(world.pdr*standard.pop)))
#print(paste0("infection rate ", infection.rate))
daily.surg.on.infected <- infection.rate*prop.asympt*total.surg.per.day
#print(paste0("daily surgeries on infected ", daily.surg.on.infected))

# age groups - similar shape to Gudbjartsson et al
covid.age.distr <- 0.1 + sin(pi*(1:19)/20)
# correct so that cohort size adjusted mean is 1
covid.age.distr <- covid.age.distr*(pop.data$female+pop.data$male)
covid.age.distr <- covid.age.distr/mean(covid.age.distr)
# account for combined last bracket
covid.age.distr <- c(covid.age.distr[1:17],mean(covid.age.distr[18:19]))


infection.rates <- c(0.01,0.005,0.001)

tab <- matrix(0.0,nrow=length(infection.rates)*5,ncol=8)

tt <- world.pdr*standard.pop
for(i in 1:length(infection.rates)){
  k <- 1
  which.part <- 1:6 # <=29
  tab[k+(i-1)*5,1] <- infection.rates[i]
  tab[k+(i-1)*5,2] <- round(sum(tt[which.part]))
  tab[k+(i-1)*5,3] <- round(sum(infection.rates[i]*covid.age.distr[which.part]*tt[which.part]))
  tab[k+(i-1)*5,4] <- round(sum(prop.asympt*infection.rates[i]*covid.age.distr[which.part]*tt[which.part]))
  tab[k+(i-1)*5,5] <- round(tab[k+(i-1)*5,4]*covidSD[k])
  tab[k+(i-1)*5,6] <- 365*tab[k+(i-1)*5,5]
  tab[k+(i-1)*5,7] <- round(tab[k+(i-1)*5,4]*covidSP[k])
  tab[k+(i-1)*5,8] <- 365*tab[k+(i-1)*5,7]
  k <- 2
  which.part <- 7:10
  tab[k+(i-1)*5,1] <- infection.rates[i]
  tab[k+(i-1)*5,2] <- round(sum(tt[which.part]))
  tab[k+(i-1)*5,3] <- round(sum(infection.rates[i]*covid.age.distr[which.part]*tt[which.part]))
  tab[k+(i-1)*5,4] <- round(sum(prop.asympt*infection.rates[i]*covid.age.distr[which.part]*tt[which.part]))
  tab[k+(i-1)*5,5] <- round(tab[k+(i-1)*5,4]*covidSD[k])
  tab[k+(i-1)*5,6] <- 365*tab[k+(i-1)*5,5]
  tab[k+(i-1)*5,7] <- round(tab[k+(i-1)*4,4]*covidSP[k])
  tab[k+(i-1)*5,8] <- 365*tab[k+(i-1)*5,7]
  k <- 3
  which.part <- 11:14
  tab[k+(i-1)*5,1] <- infection.rates[i]
  tab[k+(i-1)*5,2] <- round(sum(tt[which.part]))
  tab[k+(i-1)*5,3] <- round(sum(infection.rates[i]*covid.age.distr[which.part]*tt[which.part]))
  tab[k+(i-1)*5,4] <- round(sum(prop.asympt*infection.rates[i]*covid.age.distr[which.part]*tt[which.part]))
  tab[k+(i-1)*5,5] <- round(tab[k+(i-1)*5,4]*covidSD[k])
  tab[k+(i-1)*5,6] <- 365*tab[k+(i-1)*5,5]
  tab[k+(i-1)*5,7] <- round(tab[k+(i-1)*5,4]*covidSP[k])
  tab[k+(i-1)*5,8] <- 365*tab[k+(i-1)*5,7]
  k <- 4
  which.part <- 15:18
  tab[k+(i-1)*5,1] <- infection.rates[i]
  tab[k+(i-1)*5,2] <- round(sum(tt[which.part]))
  tab[k+(i-1)*5,3] <- round(sum(infection.rates[i]*covid.age.distr[which.part]*tt[which.part]))
  tab[k+(i-1)*5,4] <- round(sum(prop.asympt*infection.rates[i]*covid.age.distr[which.part]*tt[which.part]))
  tab[k+(i-1)*5,5] <- round(tab[k+(i-1)*5,4]*covidSD[k])
  tab[k+(i-1)*5,6] <- 365*tab[k+(i-1)*5,5]
  tab[k+(i-1)*5,7] <- round(tab[k+(i-1)*5,4]*covidSP[k])
  tab[k+(i-1)*5,8] <- 365*tab[k+(i-1)*5,7]

  tab[5*i,1] <- infection.rates[i]
  tab[5*i,2:8] <- colSums(tab[(1+(i-1)*5):(4+(i-1)*5),2:8])
}

colnames(tab) <- c("infection rate","total surgeries",
                   "surgeries with covid","surgeries with asymptomatic covid",
                   "daily 30-day mortality","yealy 30-day mortality",
                   "daily pulmonary complications","yearly pulmonary complications")

rownames(tab) <- rep(c("WORLD <=29 years","WORLD 30-49 years",
                       "WORLD 50-69 years", "WORLD >= 70 years",
                       "WORLD population"),
                     length(infection.rates))

#print(tab)
#writexl::write_xlsx(as.data.frame(tab,rep(c("WORLD <=29 years","WORLD 30-49 years",
#                                            "WORLD 50-69 years", "WORLD >= 70 years",
#                                            "WORLD population"),
#                                          length(infection.rates))),path="world.xlsx")


