rm(list = ls())
# define the SEIR-model
source("seir_model.R")
# country information and surgeries rates
source("make_surg_rates.R")

countries <- c("AUS", "CAN", "EU27", "UK", "US")
start.date <- which(dates == "2020-07-01")
checkpoints <- c(
  which(dates == "2020-12-31"),
  which(dates == "2021-07-01"),
  which(dates == "2022-07-01")
)

# make scaling factors for rates
country.pop <- rep("", length(countries) * 3)
scal <- rep(0.0, length(countries) * 3)
label <- rep("", length(countries) * 3)
popRateArr.f <- matrix(0.0, length(countries) * 3, 19)
popRateArr.m <- matrix(0.0, length(countries) * 3, 19)
k <- 1
for (c in 1:length(countries)) {
  p.f <- pops[, paste0(countries[c], "_F")]
  p.m <- pops[, paste0(countries[c], "_M")]
  p.t <- p.f + p.m
  covid.age.distr <- 0.1 + sin(pi * (1:19) / 20)
  covid.age.distr <-
    covid.age.distr * sum(p.t) / sum(covid.age.distr * p.t)
  r.f <- omling.data$pdr_f * p.f
  r.m <- omling.data$pdr_m * p.m
  r.t <- r.f + r.m
  ry <- sum(r.t) * 100000 * 365 / (sum(p.f) + sum(p.m))
  popRateArr.f[k,] <- covid.age.distr * r.f * (15000 / ry)
  popRateArr.m[k,] <- covid.age.distr * r.m * (15000 / ry)
  label[k] <- paste0(countries[c], "_15000")
  country.pop[k] <- countries[c]
  scal[k] <- (15000 / ry)
  k <- k + 1
  popRateArr.f[k,] <- covid.age.distr * r.f * (10000 / ry)
  popRateArr.m[k,] <- covid.age.distr * r.m * (10000 / ry)
  label[k] <- paste0(countries[c], "_10000")
  country.pop[k] <- countries[c]
  scal[k] <- (10000 / ry)
  k <- k + 1
  popRateArr.f[k,] <- covid.age.distr * r.f * (5000 / ry)
  popRateArr.m[k,] <- covid.age.distr * r.m * (5000 / ry)
  label[k] <- paste0(countries[c], "_5000")
  country.pop[k] <- countries[c]
  scal[k] <- (5000 / ry)
  k <- k + 1
}



# covidSurg probabilities
covidSD <- c(0.0, 0.058, 0.202, 0.341, 0.289, 0.184)
covidSP <- c(0.291, 0.442, 0.540, 0.560, 0.572, 0.469)

# number of Monte Carlo replica
nrep <- 1000


# output arrays 
resInf <- matrix(0.0, nrow = length(dates), ncol = nrep)
resCounts <-
  array(0.0, dim = c(length(dates), nrep, 12, length(label)))
set.seed(1)
# Main Monte Carlo replication loop
for (i in 1:nrep) {
  # simulate effect of social distancing
  soc_dist_Rfac <- runif(n = 1,
                         min = 0.44 / 2.2,
                         max = 0.82 / 2.2)
  # season modulation amplitude
  season_amplitude <- 0.4
  # simulate upper threshold for installing social distancing
  upper_thresh <- runif(n = 1, min = 35.0, max = 70.0)
  # threshold for removing social distancing
  lower_thresh <- upper_thresh / 7.0
  # simulate asymptomatic rate
  gam <- runif(n = 1, min = 0.45, max = 0.55)
  # simulate trajectory for the above parameters
  out <- seir_sim(
    soc_dist_Rfac = soc_dist_Rfac,
    season_amplitude = season_amplitude,
    upper_thresh = upper_thresh,
    lower_thresh = lower_thresh
  )
  # potentially asymptomatic infected
  resInf[, i] <- (out$ode$IR + out$ode$E)
  
  # simulate actual surgeries etc
  for (pop in 1:length(label)) {
    for (t in start.date:length(dates)) {
      surg.f <-
        rpois(19, resInf[t, i] * popRateArr.f[pop,])
      surg.m <-
        rpois(19, resInf[t, i] * popRateArr.m[pop,])
      surg.asyp.f <- rbinom(19, size = surg.f, prob = gam)
      surg.asyp.m <- rbinom(19, size = surg.m, prob = gam)
      surg.asyp.t <- surg.asyp.f + surg.asyp.m
      resCounts[t, i, 11, pop] <-
        sum(surg.asyp.t) # total surg on infected
      resCounts[t, i, 12, pop] <-
        sum(surg.asyp.t[15:19]) # total surg on infected
      resCounts[t, i, 1, pop]  <-
        rbinom(1, size = sum(surg.asyp.t[1:6]), prob = covidSD[1])
      resCounts[t, i, 2, pop]  <-
        rbinom(1, size = sum(surg.asyp.t[7:10]), prob = covidSD[2])
      resCounts[t, i, 3, pop]  <-
        rbinom(1, size = sum(surg.asyp.t[11:14]), prob = covidSD[3])
      resCounts[t, i, 4, pop]  <-
        rbinom(1, size = sum(surg.asyp.t[15:19]), prob = covidSD[4])
      resCounts[t, i, 5, pop]  <- sum(resCounts[t, i, 1:4, pop])
      resCounts[t, i, 6, pop]  <-
        rbinom(1, size = sum(surg.asyp.t[1:6]), prob = covidSP[1])
      resCounts[t, i, 7, pop]  <-
        rbinom(1, size = sum(surg.asyp.t[7:10]), prob = covidSP[2])
      resCounts[t, i, 8, pop]  <-
        rbinom(1, size = sum(surg.asyp.t[11:14]), prob = covidSP[3])
      resCounts[t, i, 9, pop]  <-
        rbinom(1, size = sum(surg.asyp.t[15:19]), prob = covidSP[4])
      resCounts[t, i, 10, pop] <- sum(resCounts[t, i, 6:9, pop])
    }
  }
  
}
save.image("Computations")
<<<<<<< HEAD


=======
>>>>>>> 09747b3... improved comments
