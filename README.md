# COVID19_surgery_risk
This repository contains R code for a modelling study of the risks related to doing surgery on asymptomatic COVID19 patients.

The code is developed by Tore Selland Kleppe (tore.kleppe@uis.no).

The work is documented in the manuscript "Estimating risk of admitting patients with presymptomatic SARS-CoV-2 to surgery during the 
COVID-19 pandemic and perioperative pulmonary complications and mortality" and the supplementary material for the manuscript.

The code in the file "sim_model.R" runs the stochastic dynamic model, and makes an R image file used for plots 3 and 4. Note, this takes a while, and generates a rather large image file.

The code in the file "contour_curves.R" generate plot 1 and 2 in the manuscript.

The code in the file "sim_model_contrywise_plots.R" generate plot 3 in the manuscript and similar plots for other countries and surgery rates.

The code in the file "combined_cumulative_plots_ggplot.R" generate plot 4 in the manuscript.

