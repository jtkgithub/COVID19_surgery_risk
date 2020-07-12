



# data from Omling et al, Table 1
labs <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
"35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74",
"75-79","80-84","85-89",">=90")

# in-patient
inp <- matrix(c(2020 , 4030,
         1350 , 2710, 
         1500 , 2930, 
         2010 , 3480,
         2680 , 4560, 
         4020 , 6930, 
         4810 , 8450, 
         3830 , 6890, 
         3080 , 5640, 
         3350 , 6190,
         4080 , 7600,
         5170 , 9610, 
         6790 , 12510,
         8590 , 15690,
         10790 , 19300,
         13030,22830,
         14330,24380, 
         14840,24820, 
         13670,22940),19,2,byrow = T)
# out-patient
outp <- matrix(c(4230,4100,3990,5810,6900,7900,8830,
                 9670,9680,10680,12600,14840,18120,
                 22860,27980,32240,32500,28440,18990),19,1)

# only used for sanity checks
nn <- c(550606,
        510683, 
        512505, 
        606991, 
        613358, 
        581776, 
        589876, 
        625583, 
        655637, 
        632247, 
        586228, 
        583810, 
        606237, 
        526007, 
        388598, 
        308947, 
        247218, 
        163935, 
        83810)



# gross per (person,day) rates
pdrate.min <- (inp[,1]+outp)/(365*100000)
pdrate.max <- (inp[,2]+outp)/(365*100000)

# data from fig 2, measured (using preview on mac) from article-pdf : 
# the numbers are: total length of bar, lenght of female bar
f<-matrix(c(38.3,15.76,
  31.01,13.68,
  31.2,13.68,
  43.89,21.37,
  53.29,31.98,
  69.45,49.03,
  81.43,60.05,
  77.25,53.92,
  71.58,44.4,
  78.26,46.35,
  93.61,52.98,
  113.78,59.72,
  142.65,70.28,
  179.91,85.68,
  221.11,103.8,
  258.52,119.09,
  270.19,121.21,
  257.77,111.38,
  211.53,87.43),19,2,byrow = T)

# "reproduce" fig 2 in Omling (check)
#plot(-(f[,1]-f[,2]),19:1,xlim=c(-170,170))
#points(f[,2],19:1)
#lines(c(0,0),c(20,0))

# swedish demography data from 
# http://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__BE__BE0101__BE0101A/BefolkningR1860/table/tableViewLayout1/
library(readxl)
tmp <- as.data.frame(read_xlsx("swedish_population.xlsx",sheet="Ark1"))
tmp2 <- matrix(0.0,nrow=21,ncol=2)
# average over populations 2006-2013
for(ii in 1:21){
  tmp2[ii,1] <- mean(as.numeric(tmp[(2*ii-1),2:9]))
  tmp2[ii,2] <- mean(as.numeric(tmp[2*ii,2:9]))
}
# combine last cohorts to match Omling data
tmp2[19,] <- tmp2[19,]+tmp2[20,]+tmp2[21,]
tmp2 <- tmp2[1:19,]
# check
#print(round(tmp2[,1]+tmp2[,2])-nn) # i.e. exactly the same data used by Omling et al

# proportion of females in population
f.pop.prop <- tmp2[1:19,2]/(tmp2[1:19,1]+tmp2[1:19,2])

# female proportion of those having surgery
f.prop <- f[,2]/f[,1]

# take average of most conservative and most liberal data from Omling
surg.rate.overall <- 0.5*(pdrate.min+pdrate.max)

# specific rates taking into account discrepancies in overall surgery intesity across sexes,
# these are found as the solutions to (within each age group)
# N_f*l_f + N_m*l_m = l_t*(N_f+N_m)
# l_f*N_f/(l_f*N_f + l_m*N_m) = w_f
# where l_t is the total rate of surgery (surg.rate.overall), and w_f is the proportion of these surgeries done to females
# solutions :
# l_f = w_f*l_t/(N_f/(N_f+N_m))
# l_m = (1-w_f)*l_t/(N_m/(N_f+N_m))
# N_f/(N_f+N_m) taken from population data in f.pop.prop

surg.rate.f <- f.prop*surg.rate.overall/f.pop.prop
surg.rate.m <- (1.0-f.prop)*surg.rate.overall/(1.0-f.pop.prop)



omling.data <- as.data.frame(cbind(inp,outp,pdrate.min,pdrate.max,f.prop,
                                   f.pop.prop,surg.rate.f,surg.rate.m,surg.rate.overall,nn))
rownames(omling.data) <- labs
colnames(omling.data) <- c("in_min","in_max","out","pdr_min","pdr_max","fprop","f_pop_prop",
                           "pdr_f","pdr_m","pdr_all","nn")


to.appendix <- data.frame(omling.data$pdr_all,
                          omling.data$fprop,
                          omling.data$f_pop_prop,
                          omling.data$pdr_f,
                          omling.data$pdr_m)
rownames(to.appendix) <- labs
writexl::write_xlsx(to.appendix,path="omling_appendix.xlsx")


raw <- matrix(0.0,19,10)



# Canada

# note data downloaded from Canada 2016 census
#https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/page_Download-Telecharger.cfm?Lang=E&Tab=1&Geo1=PR&Code1=01&Geo2=PR&Code2=01&SearchText=Canada&SearchType=Begins&SearchPR=01&B1=All&TABID=1&type=0

dta <- as.data.frame(read.csv("CensusProfile2016-ProfilRecensement2016-20200602125535.csv"))

dta <- dta[dta$Topic=="Age characteristics",]
cha <- as.character(dta$Characteristics)
for(i in 0:18){
  str <- paste0(i*5," to ",(i+1)*5-1," years")
  ii <- which(grepl(str,cha))
  raw[(i+1),1] <- as.numeric(dta[ii,"Female"])
  raw[(i+1),2] <- as.numeric(dta[ii,"Male"])
  
}

raw[19,1] <- raw[19,1] + as.numeric(dta[which(grepl("95 to 99 years",cha)),"Female"])
raw[19,2] <- raw[19,2] + as.numeric(dta[which(grepl("95 to 99 years",cha)),"Male"])
raw[19,1] <- raw[19,1] + as.numeric(dta[which(grepl("100 years and over",cha)),"Female"])
raw[19,2] <- raw[19,2] + as.numeric(dta[which(grepl("100 years and over",cha)),"Male"])



# UK 
#UK population (2011 census)
# data taken from table 1 of 
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/2011censuspopulationandhouseholdestimatesfortheunitedkingdom
#
raw[,4] <- c(2002000,1800000,1879000,2041000,2164000,2145000,2060000,2082000,2283000,2293000,2029000,1785000,1869000,
             1464000,1163000,904000,615000,324000,127000)
raw[,3] <- c(1912000,1717000,1790000,1956000,2133000,2162000,2066000,2112000,2341000,2350000,2066000,1829000,1939000,
             1555000,1300000,1102000,883000,594000,349000)


# USA
# https://www.census.gov/prod/cen2010/briefs/c2010br-03.pdf

raw[,6] <- c(10319427,10389638,10579862,11303666,11014176,10635591,9996500,10042022,10393977,
             11209085,10933274,9523648,8077500,5852547,4243972,3182388,2294374,1273867,424387+82263+9162)
raw[,5] <- c(9881935,9959019,10097332,10736677,10571823,10466258,9965599,10137620,10496987,
             11499506,11364851,10141157,8740424,6582716,5034194,4135407,3448953,2346592,1023979+288981+44202)



# EU2020 (without UK)
# https://appsso.eurostat.ec.europa.eu/nui/submitViewTableAction.do
dta <- read.csv("demo_pjan_1_Data.csv")
f <- dta[dta$SEX=="Females",]
m <- dta[dta$SEX=="Males",]
k <- 2
for(i in 1:19){
  for(j in 1:5){
    raw[i,7] <- raw[i,7] + as.numeric(gsub(",","",f[k,"Value"],fixed=TRUE))
    raw[i,8] <- raw[i,8] + as.numeric(gsub(",","",m[k,"Value"],fixed=TRUE))
    k <- k+1
  }
}
for (j in 97:102){
  raw[19,7] <- raw[19,7] + as.numeric(gsub(",","",f[j,"Value"],fixed=TRUE))
  raw[19,8] <- raw[19,8] + as.numeric(gsub(",","",m[j,"Value"],fixed=TRUE))
}

# Australia
 # see "table_australia.xlsx" for details (downloaded from https://guest.censusdata.abs.gov.au/webapi/jsf/tableView/tableView.xhtml )

raw[,10] <- c(752138,771055,717635,727677,795423,824077,839822,773139,777703,773171,748951,709119,632353,581236,431321,307444,204024,123499,46831+9245+778)
raw[,9] <-c(712637,731590,679557,693918,771369,840526,864030,788545,805556,808284,774601,745215,667045,607762,456390,345216,256527,185459,93572+24672+2788)



colnames(raw) <- c("CAN_F","CAN_M","UK_F","UK_M","US_F","US_M","EU27_F","EU27_M","AUS_F","AUS_M")
rownames(raw) <- labs

pops <- as.data.frame(raw)


writexl::write_xlsx(pops,path="pops_appendix.xlsx")


# make a smaller data frame based on the EURO-population (see simulation_model_3.R)

f.pop <- pops$EU27_F
m.pop <- pops$EU27_M

N <- sum(f.pop)+sum(m.pop)
f.pop <- f.pop*(1.0e7/N)
m.pop <- m.pop*(1.0e7/N)

pop.data <- as.data.frame(cbind(f.pop,m.pop))
rownames(pop.data)<-labs
colnames(pop.data)<-c("female","male")





