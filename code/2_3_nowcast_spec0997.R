## Functions to perform the steps can be found in 99_daily_analysis_fun.R
source("./analysis_fun_stanmodel_mc.R")
# Load required packages
library(tidyverse)
library(data.table)
library(surveillance)
library(broom)
library(readr)
library(lubridate)
library(knitr)
library(rstan)
options(mc.cores = 3)
library(nleqslv)
set.seed(523523)
data_date = ymd("2020-09-21")

load(paste0("../results/nowcast_bav/imp_dat_adj_0997_synth.RData"))

nc_res = estimate_nowcasts(imputed_data = imputed_dat_adj_997, 
                           data_date = data_date, 
                           save_results = FALSE, safePredictLag = 2)

nc_smry = nc_res$nc_smry
save(nc_smry, file = "../results/nowcast_bav/nowcast_997_synth.RData")

rt_997 = estimate_Rt(nc_res, data_date = data_date)
save(rt_997, file = "../results/nowcast_bav/rt_0997_synth.RData")
quit(save = "no")