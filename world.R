# standard population and absolute size for world

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

