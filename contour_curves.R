rm(list=ls())
library(ggplot2)
library(directlabels)
library(gridExtra)
library(metR)
library(isoband)
# load countrywise information and surgery rates
source("make_surg_rates.R")
# load world information 
source("world.R")

gam <- 0.5 # fixed asymptomatic rate
pdr_f <- omling.data$pdr_f
pdr_m <- omling.data$pdr_m

# covidSurg probabilities
covidSD <- c(0.0, 0.058, 0.202, 0.341, 0.289, 0.184)
covidSP <- c(0.291, 0.442, 0.540, 0.560, 0.572, 0.469)
# plotting grids
n.I <- 100
n.S <- 101

r.I <- seq(from = 0.001,
           to = 0.015,
           length.out = n.I)
r.S <- seq(from = 5000.0,
           to = 20000.0,
           length.out = n.S)
r.SW <- seq(from = 2000.0,
            to = 5000.0,
            length.out = n.S)
countries <- c("AUS", "CAN", "EU27", "UK", "US")
# table of betas
tab <- matrix(0.0, nrow = length(countries) + 1, 3)

for (c in 1:length(countries)) {
  # population counts
  p.f <- pops[, paste0(countries[c], "_F")]
  p.m <- pops[, paste0(countries[c], "_M")]
  p.t <- p.f + p.m
  # covid loading functions
  covid.age.distr <- 0.1 + sin(pi * (1:19) / 20)
  covid.age.distr <- covid.age.distr*sum(p.t)/sum(covid.age.distr*p.t)
 
  
  # # surgeries per 100000 person-years
  fy <- sum(pdr_f * p.f + pdr_m * p.m) / sum(p.t) * 100000 * 365
  
  # u-s in appendix
  raw.f <- (gam / fy) * covid.age.distr * p.f * pdr_f
  raw.m <- (gam / fy) * covid.age.distr * p.m * pdr_m
  raw.t <- raw.f + raw.m
  
  # over 70 with covid, per year
  tab[c, 1] <- sum(raw.t[15:19]) * 365
  
  
  # pulmonary complications
  tab[c, 2] <- 365 * (
    sum(raw.t[1:6]) * covidSP[1] +
      sum(raw.t[7:10]) * covidSP[2] +
      sum(raw.t[11:14]) * covidSP[3] +
      sum(raw.t[15:19]) * covidSP[4]
  )
  
  
  # 30-day mortality
  tab[c, 3] <- 365 * (
    sum(raw.t[1:6]) * covidSD[1] +
      sum(raw.t[7:10]) * covidSD[2] +
      sum(raw.t[11:14]) * covidSD[3] +
      sum(raw.t[15:19]) * covidSD[4]
  )
  
  
  
  print(fy)
}

# world is somewhat different

last.brac.rate <-
  (omling.data$pdr_all[18] * omling.data$nn[18] + omling.data$pdr_all[19] *
     omling.data$nn[19]) / sum(omling.data$nn[18:19])
omling.pdr <- c(omling.data$pdr_all[1:17], last.brac.rate)


covid.age.distr <- 0.1 + sin(pi * (1:19) / 20)
covid.age.distr <- covid.age.distr[1:18]
covid.age.distr <- covid.age.distr * sum(standard.pop)/sum(covid.age.distr*standard.pop)


fy <- sum(standard.pop * omling.pdr) / sum(standard.pop) * 100000 * 365

raw.t <- (gam / fy) * covid.age.distr * standard.pop * omling.pdr

c <- length(countries) + 1
# over 70 with covid, per year
tab[c, 1] <- sum(raw.t[15:18]) * 365



# pulmonary complications
tab[c, 2] <- 365 * (
  sum(raw.t[1:6]) * covidSP[1] +
    sum(raw.t[7:10]) * covidSP[2] +
    sum(raw.t[11:14]) * covidSP[3] +
    sum(raw.t[15:18]) * covidSP[4]
)

# 30-day mortality
tab[c, 3] <- 365 * (
  sum(raw.t[1:6]) * covidSD[1] +
    sum(raw.t[7:10]) * covidSD[2] +
    sum(raw.t[11:14]) * covidSD[3] +
    sum(raw.t[15:18]) * covidSD[4]
)


# table of betas
print(tab)
tab.df <- data.frame(tab)
rownames(tab.df) <- c(countries,"WORLD")
writexl::write_xlsx(tab.df,path="ts.xlsx")

# make the plots
if (T) {
  z <- vector(mode = "numeric", length = length(r.I) * length(r.S))
  r.Iv <- z
  r.Sv <- z
  r.SWv <- z
  zW <- z
    for (i in 1:length(r.I)) {
      r.Iv[((i - 1) * n.S + 1):(i * n.S)] <- r.I[i]
      r.Sv[((i - 1) * n.S + 1):(i * n.S)] <- r.S
      r.SWv[((i - 1) * n.S + 1):(i * n.S)] <- r.SW
      z[((i - 1) * n.S + 1):(i * n.S)] <- r.I[i] * r.S
      zW[((i - 1) * n.S + 1):(i * n.S)] <- r.I[i] * r.SW
    }
  plots <- list()
  props <- c("Yearly surgeries on Covid-19\ninfected >= 70 years", 
             "Yearly pulmonary complications \nafter surgery on infected",
             "Yearly 30 day mortality after \nsurgery on infected")
  regions <- c(countries, "WORLD")
  for (c in 1:length(regions)) {
    for (prop in 1:dim(tab)[2]) {
      if (identical(regions[c], "WORLD")) {
        df <- data.frame(I = r.Iv,
                         S = r.SWv,
                         z = zW * tab[c, prop])
      } else {
        df <- data.frame(I = r.Iv,
                         S = r.Sv,
                         z = z * tab[c, prop])
      }
      p <- ggplot(df, aes(I, S, z = z)) +
        geom_contour() +
        geom_text_contour(size = 2,stroke=0.2) +
        theme_bw() +
        theme(plot.margin = unit(-0.0 * c(1, 1, 1, 1), "cm"),
              panel.spacing = unit(-1, "cm"),
              plot.title = element_text(size = 7, face = "bold"),
              axis.text = element_text(size = 7, face = "bold"),
              axis.title = element_text(size = 7, face = "bold")) +
        ggtitle(paste0(regions[c], " ", props[prop]))
          
      if (c == 3 || c == 6) {
        p <- p + xlab("Infection rate in population r")
      } else {
        p <- p + xlab("")
      }
      if (c == 1 || c == 4) {
        p <- p + ggtitle(props[prop])
      } else {
        p <- p + ggtitle("")
      }
      if (prop == 1) {
        p <- p + ylab(paste0(regions[c], "\n Surgery rate S (#  per 100K person-years)"))
      } else {
        p <- p + ylab("")
      }
      
      plots <- c(plots, list(p))
    }
    
    if(c==3){
      ggsave(
        "contour_plots_1.pdf",
 #       "contour_plots_1.tiff",
        plot = do.call(arrangeGrob,
                       c(gList = plots, ncol = 3)),
        width = 7,
        height = 7
      )
      plots <- list()
    }
    
    if(c==6){
      ggsave(
        "contour_plots_2.pdf",
        plot = do.call(arrangeGrob,
                       c(gList = plots, ncol = 3)),
        width = 7,
        height = 7
      )
    }
    
  }
  
 
}