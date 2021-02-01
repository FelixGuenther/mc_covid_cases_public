# Load required packages
library(tidyverse)
library(data.table)
library(lubridate)
library(RColorBrewer)
library(mgcv)
library(ggpubr)
library(mgcViz)
library(surveillance)
library(broom)
library(readr)
library(rstan)
library(nleqslv)

col_vec = brewer.pal(3, "Dark2")
## Read Edit Data
# Bavaria
# Daily PCR-test data
load("../data/200921_test_bav.RData") 
dat_test_bav = dat_test_bav %>% filter(date>=ymd("2020-03-16"))
# Person-specific case data
dat_cases_ind_bav = read_tsv("../data/2020_09_21_bav_synth.csv")
# Derive daily case data
dat_cases_bav = dat_cases_ind_bav %>%
  group_by(date=rep_date_local) %>% summarise(n_cases = n()) %>%
  select(date, n_cases)
case_test_dat_bav = dat_test_bav %>% rename(n_pcr=Gesamtzahl, n_pcr_pos=Positive) %>%
  left_join(dat_cases_bav) %>% arrange(date) %>% mutate(t=1:n())

# Germany
load("../data/201001_cases_tests_germany.RData")
case_test_dat_ger = case_test_dat_ger %>% mutate(date = as.Date(paste(2020, KW, 3, sep="-"), "%Y-%U-%u")-7) %>%
  mutate(n_pcr_100k = n_pcr/83000000*100000/7,
         n_pcr_pos_100k = n_pcr_pos/83000000*100000/7,
         n_cases_100k = n_cases/83000000*100000/7)
# Adjust Bavarian case data by population size
case_test_dat_bav = case_test_dat_bav %>%
  mutate(n_pcr_100k = n_pcr/13080000*100000,
         n_pcr_pos_100k = n_pcr_pos/13080000*100000,
         n_cases_100k = n_cases/13080000*100000)

# Plot number cases and number of tests
plot_dat_fig_1 = rbind(case_test_dat_ger %>% 
                         mutate(region="Germany") %>% 
                         select(date, 
                                n_pcr_100k, 
                                n_pcr_pos_100k, 
                                n_cases_100k, region),
                       case_test_dat_bav %>% 
                         mutate(region="Bavaria") %>% 
                         select(date, 
                                n_pcr_100k, 
                                n_pcr_pos_100k, 
                                n_cases_100k, region)) 
fig_1_1 = ggplot(plot_dat_fig_1 %>% 
                   mutate(region = factor(region, 
                                          levels = c("Germany", "Bavaria"),
                                          labels = c("Germany", "Bavaria")))) + 
  geom_line(aes(date, n_pcr_100k, lty = region)) +
  theme_bw() +
  xlab("Date") +
  ylab("Number tests\nper 100k") +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  scale_x_date(date_breaks = "1 month")


fig_1_2 = ggplot(plot_dat_fig_1 %>% 
                   select(date, n_pcr_pos_100k, n_cases_100k, region) %>%
                   pivot_longer(cols = c("n_pcr_pos_100k", "n_cases_100k")) %>%
                   mutate(name = factor(name, levels = c("n_pcr_pos_100k", "n_cases_100k"),
                                        labels = c("Positive tests",
                                                   "Reported cases")),
                          region = factor(region, levels = c("Germany", "Bavaria")))) + 
  geom_line(aes(date, value, col = name, lty = region)) +
  theme_bw() +
  xlab("Date") +
  ylab("Number\nper 100k") +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  scale_x_date(date_breaks = "1 month")

fig_1 = ggarrange(fig_1_1,
                  fig_1_2, nrow = 2, labels = "AUTO", common.legend = F, 
                  legend = "bottom")

ggsave(fig_1, filename = "../results/figures/fig_1_test_case_num_synth.pdf", width = 8, height = 5, dpi = 500)

# Numbers
# Number PCR Tests Germany/Bavaria
sum(case_test_dat_ger$n_pcr)
sum(case_test_dat_bav$n_pcr)
# per 100
sum(case_test_dat_ger$n_pcr)/83000000*100
sum(case_test_dat_bav$n_pcr)/13080000*100

# Cases/pos Test Bavaria
sum(case_test_dat_bav$n_cases)
sum(case_test_dat_bav$n_pcr_pos)

# Cases/pos Test Ger
sum(case_test_dat_ger$n_cases)
sum(case_test_dat_ger$n_pcr_pos)

# Estimate test model
mod_ger = est_pos_test_model(case_test_dat_ger, max_lag = 2, date_start = ymd("2020-04-29"))
summary(mod_ger[[1]])
mod_bav = est_pos_test_model(case_test_dat_bav, max_lag = 7, date_start = ymd("2020-05-01"))
summary(mod_bav[[1]])
# Figure two, predicted number of examined persons and reported number of PCR tests
plot_dat_fig_2 = rbind(mod_ger[[2]] %>% select(date, n_pcr, n_exam_pers) %>%
                         pivot_longer(cols = c("n_pcr", "n_exam_pers")) %>%
                         mutate(name = factor(name, levels = c("n_pcr", "n_exam_pers"),
                                              labels = c("PCR tests\nreported by labs", 
                                                         "Examined persons\nmodel derived"))) %>%
                         mutate(region="Germany",
                                value = value/83000000/7*100000),
                       mod_bav[[2]] %>% select(date, n_pcr, n_exam_pers) %>%
                         pivot_longer(cols = c("n_pcr", "n_exam_pers")) %>%
                         mutate(name = factor(name, levels = c("n_pcr", "n_exam_pers"),
                                              labels = c("PCR tests\nreported by labs", 
                                                         "Examined persons\nmodel derived"))) %>%
                         mutate(region="Bavaria",
                       value = value/13080000*100000)) %>%
  mutate(region = factor(region, levels = c("Germany", "Bavaria")))

fig_2 = ggplot(plot_dat_fig_2) + 
  geom_line(aes(date, value, col = name, lty = region)) + theme_bw() +
  xlab("Date") + 
  ylab("Number\nper 100k") +
  facet_grid(rows = "region") +
  scale_linetype_discrete(guide=FALSE) + 
  theme(legend.position = "bottom", legend.title = element_blank())
ggsave(fig_2, filename = "../results/figures/fig_2_test_exam_num_synth.pdf", width = 8, height = 5, dpi = 500)

# Supplemental Figure 1 - Effects of model
viz_ger = getViz(mod_ger[[1]])
ger_beta_0 = plot(sm(viz_ger, 1)) + l_fitLine() + l_ciLine() + 
  scale_x_continuous(breaks=seq(1,21, by=4), labels = paste0("CW",seq(18,38, by=4))) +
  ylim(-.2,1.2) + ylab("Time-varying linear effect Lag-0") + xlab("Time")
ger_beta_1 = plot(sm(viz_ger, 2)) + l_fitLine() + l_ciLine() +
  scale_x_continuous(breaks=seq(1,21, by=4), labels = paste0("CW",seq(18,38, by=4))) +
  ylim(-.2,1.2) + ylab("Time-varying linear effect Lag-1") + xlab("Time")
plots_ger = gridPrint(ger_beta_0, ger_beta_1, left = "Germany", ncol=3)

viz_bav = getViz(mod_bav[[1]])
bav_beta_0 = plot(sm(viz_bav, 1)) + l_fitLine() + l_ciLine() + 
  scale_x_continuous(breaks=seq(1,143, by=28), labels = format(seq(ymd("2020-05-01"), ymd("2020-09-20"), by = 28), '%m-%d')) +
  ylim(-.2,1.2) + ylab("Time-varying linear effect Lag-0") + xlab("Time")
bav_beta_1 = plot(sm(viz_bav, 2)) + l_fitLine() + l_ciLine() + 
  scale_x_continuous(breaks=seq(1,143, by=28), labels = format(seq(ymd("2020-05-01"), ymd("2020-09-20"), by = 28), '%m-%d')) +
  ylim(-.2,1.2) + ylab("Time-varying linear effect Lag-1") + xlab("Time")
bav_beta_6 = plot(sm(viz_bav, 3)) + l_fitLine() + l_ciLine() + 
  scale_x_continuous(breaks=seq(1,143, by=28), labels = format(seq(ymd("2020-05-01"), ymd("2020-09-20"), by = 28), '%m-%d')) +
  ylim(-.2,1.2) + ylab("Time-varying linear effect Lag-6") + xlab("Time")

plots_bav = gridPrint(bav_beta_0, bav_beta_1, bav_beta_6, ncol=3, left="Bavaria")
supp_fig_1 = gridPrint(plots_ger, plots_bav)
ggsave(supp_fig_1, filename = "../results/figures/supp_fig_1_synth.pdf", width = 12, height = 6)

# Adjust case numbers
adj_case_ger = expand_grid(date=mod_ger[[2]]$date, sens=c(1,.9,.7), spec=c(1,.999,.997,.995)) %>%
  left_join(mod_ger[[2]] %>% dplyr::select(date, n_cases, n_exam_pers))

adj_case_ger = adj_case_ger %>% 
  mutate(n_cases_adj = adjust_n_case(n_cases, n_exam_pers, sens = sens, spec=spec)) %>%
  mutate(sens = factor(sens, levels = c("1","0.9", "0.7"),
                       labels = paste0("Sens: ", c("1","0.9", "0.7"))),
         spec = factor(spec, levels = c("1", "0.999", "0.997", "0.995"),
                       labels = paste0("Spec: ", c("1", "0.999", "0.997", "0.995"))))

adj_case_bav = expand_grid(date=mod_bav[[2]]$date, sens=c(1,.9,.7), spec=c(1,.999,.997,.995)) %>%
  left_join(mod_bav[[2]] %>% dplyr::select(date, n_cases, n_exam_pers))

adj_case_bav = adj_case_bav %>% 
  mutate(n_cases_adj = adjust_n_case(n_cases, n_exam_pers, sens = sens, spec=spec)) %>%
  mutate(sens = factor(sens, levels = c("1","0.9", "0.7"),
                       labels = paste0("Sens: ", c("1","0.9", "0.7"))),
         spec = factor(spec, levels = c("1", "0.999", "0.997", "0.995"),
                       labels = paste0("Spec: ", c("1", "0.999", "0.997", "0.995"))))

plot_dat_fig_3 = rbind(adj_case_ger %>% mutate(n_cases_adj = n_cases_adj/81000000*100000/7,
                                               region="Germany"),
                       adj_case_bav %>% mutate(n_cases_adj = n_cases_adj/13080000*100000,
                                               region="Bavaria")) %>%
  mutate(region=factor(region, levels = c("Germany", "Bavaria")))

fig_3 = ggplot(plot_dat_fig_3 %>% filter(sens!= "Sens: 1",
                                         spec!= "Spec: 1") %>%
                 mutate(type=NA)) + 
  geom_line(aes(date, n_cases_adj, col = spec), lwd=1, alpha=.75) +
  geom_line(aes(date, n_cases_adj, linetype = type, size=type), 
            data = plot_dat_fig_3 %>% filter(sens== "Sens: 1",
                                             spec== "Spec: 1") %>%
              select(date, n_cases_adj, region) %>%
              mutate(type="Unadjusted"), alpha=1) +
  facet_grid(cols = vars(sens), rows = vars(region), scales = "free_y") +
  xlab("Date") +
  ylab("Number per 100k") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  scale_linetype_manual(values=c("Unadjusted"=1)) +
  scale_size_manual(values=c("Unadjusted"=.5)) +
  guides(col = guide_legend(order=1),
         linetype = guide_legend(order=2),
         size=FALSE)

ggsave(fig_3, filename = "../results/figures/fig_3_adj_case_numbers_2_synth.pdf", width = 8, height = 5, dpi = 500)

# Figure 4
plot_dat_fig_4 = plot_dat_fig_3 %>% filter(sens== "Sens: 0.7", spec== "Spec: 1") %>%
  select(date, region, n_cases = n_cases_adj) %>%
  right_join(plot_dat_fig_3 %>% filter(sens== "Sens: 0.7") %>%
               select(date, region, n_cases_adj, spec),
             by = c("date", "region")) %>% 
  mutate(frac_fp=1-(n_cases_adj/n_cases)) %>%
  filter(spec!="Spec: 1")

fig_4 = ggplot(plot_dat_fig_4) + 
  geom_line(aes(date, frac_fp, col = spec), lwd=0.3) +
  geom_smooth(aes(date, frac_fp, col=spec), se = FALSE) +
  facet_grid(rows=vars(region)) +
  xlab("Date") + ylab("Fraction") + theme_bw() +
  theme(legend.title = element_blank(), legend.pos = "bottom")

ggsave(fig_4, filename = "../results/figures/fig_4_frac_reported_adjusted_synth.pdf", width = 8, 
       height = 5, dpi = 500)

# Summary Bavarian data upper bound false-positives
adj_case_bav %>% filter(spec=="Spec: 0.995",
                        sens=="Sens: 1",
                        date<ymd("2020-08-01"),
                        date>=ymd("2020-06-01")) %>% 
  summarise(min_date = min(date),
            max_date = max(date),
            n=n(),
            abs_zero = sum(n_cases_adj==0),
            frac_zero = mean(n_cases_adj==0))
# Number of positive cases
case_test_dat_bav %>% filter(date>=ymd("2020-06-01"), date < ymd("2020-08-01")) %>% summarise(sum(n_pcr_pos)/sum(n_pcr))

# Prepare data for misclassification adjusted nowcasting
source("./daily_analysis_fun_stanmodel_mc.R")
set.seed(523523)
data_date = ymd("2020-09-21")
# Read IfSG dataset
dat_edit = read_edit_data("../data/2020_09_21_8Uhr_IfSG_Daten.csv", data_date = data_date)
# Perform imputation of missing disease onsets
imputation = perform_imputation(dat_edit, type = "week_weekday_age")
imputed_data = imputation[[1]]

imputed_dat_adj_999 = adjust_case_data(imputed_data = imputed_data,
                                       adj_case_bav %>% 
                                         filter(sens== "Sens: 1" &  spec== "Spec: 0.999") %>% 
                                         select(date, n_cases_adj))

imputed_dat_adj_997 = adjust_case_data(imputed_data = imputed_data,
                                       adj_case_bav %>% 
                                         filter(sens== "Sens: 1" &  spec== "Spec: 0.997") %>% 
                                         select(date, n_cases_adj))

imputed_dat_adj_995 = adjust_case_data(imputed_data = imputed_data,
                                       adj_case_bav %>% 
                                         filter(sens== "Sens: 1" &  spec== "Spec: 0.995") %>% 
                                         select(date, n_cases_adj))

save(imputed_dat_adj_999, file = "../results/nowcast_bav/imp_dat_adj_0999.RData")
save(imputed_dat_adj_997, file = "../results/nowcast_bav/imp_dat_adj_0997.RData")
save(imputed_dat_adj_995, file = "../results/nowcast_bav/imp_dat_adj_0995.RData")
save(imputed_data, file = "../results/nowcast_bav/imp_dat.RData")

load("../results/nowcast_bav/nowcast.RData")
nc_res_1 = nc_res
load("../results/nowcast_bav/nowcast_999.RData")
nc_res_999 = nc_res
load("../results/nowcast_bav/nowcast_997.RData")
nc_res_997 = nc_res
load("../results/nowcast_bav/nowcast_995.RData")
nc_res_995 = nc_res

colvec = RColorBrewer::brewer.pal(n = 6, "Set2")
# Plot nowcast results for different assumptions wrt specificity, sensitivity=1
fig_5_1 = rbind(nc_res_1$nc_smry$ntInf %>% mutate(spec= "Spec: 1"),
                nc_res_999$nc_smry$ntInf %>% mutate(spec= "Spec: 0.999"),
                nc_res_997$nc_smry$ntInf %>% mutate(spec= "Spec: 0.997"),
                nc_res_995$nc_smry$ntInf %>% mutate(spec= "Spec: 0.995")) %>%
  mutate(spec=factor(spec, levels = c("Spec: 1", "Spec: 0.999", "Spec: 0.997", "Spec: 0.995"),
                     labels = c("Unadjusted", "Spec: 0.999", "Spec: 0.997", "Spec: 0.995"))) %>%
  ggplot() +
  geom_col(aes(date, observed), data = nc_res_1$nc_smry$ntInf, fill = "lightgrey") +
  geom_line(aes(date, med, col = spec)) +
  geom_ribbon(aes(date, ymin = q025, ymax=q975, fill =spec, col = spec), alpha = 0.2, lwd = .2) +
  theme_bw() + ylab("Number") + xlab("Date") +
  scale_color_manual(values = c("Unadjusted"=1,
                                "Spec: 0.999"=2,
                                "Spec: 0.997"=3,
                                "Spec: 0.995"=4)) +
  scale_fill_manual(values = c("Unadjusted"=1,
                               "Spec: 0.999"=2,
                               "Spec: 0.997"=3,
                               "Spec: 0.995"=4)) +
  theme(legend.title = element_blank(), legend.position = c(.3, .9), 
        legend.direction = "horizontal", legend.background = element_rect(fill="transparent")) +
  ggtitle(label = "Sens.: 1") +
  coord_cartesian(xlim = c(ymd("2020-05-01"), ymd("2020-09-18")), ylim = c(0, 650))

# Plot nowcast results for different assumptions with respect to sensitivity (perform upscaling),
# specificity = 0.997
fig_5_2 = rbind(nc_res_1$nc_smry$ntInf %>% mutate(spec = "Unadjusted"),
                nc_res_997$nc_smry$ntInf %>% mutate(spec = "Sens: 1"),
                nc_res_997$nc_smry$ntInf %>% 
                  mutate(med=med*0.997/(0.9+0.997-1),
                         q025=q025*0.997/(0.9+0.997-1),
                         q975=q975*0.997/(0.9+0.997-1),
                         spec= "Sens: 0.9"),
                nc_res_997$nc_smry$ntInf %>% 
                  mutate(med=med*0.997/(0.7+0.997-1),
                         q025=q025*0.997/(0.7+0.997-1),
                         q975=q975*0.997/(0.7+0.997-1),
                         spec= "Sens: 0.7")) %>%
  mutate(spec = factor(spec, levels = c("Unadjusted",
                                        "Sens: 1",
                                        "Sens: 0.9",
                                        "Sens: 0.7")),
         date=ymd(date)) %>%
  ggplot() +
  geom_col(aes(date, observed), data = nc_res_1$nc_smry$ntInf, fill = "lightgrey") +
  geom_line(aes(date, med, col = spec)) +
  geom_ribbon(aes(date, ymin = q025, ymax=q975, fill =spec, col = spec), alpha = 0.2, lwd = .2) +
  theme_bw() + ylab("Number") + xlab("Date") +
  scale_color_manual(values = c("Unadjusted"=1,
                                "Sens: 1"=3,
                                "Sens: 0.9"=5,
                                "Sens: 0.7"=6)) +
  scale_fill_manual(values = c("Unadjusted"=1,
                                "Sens: 1"=3,
                                "Sens: 0.9"=5,
                                "Sens: 0.7"=6)) +
  theme(legend.title = element_blank(), legend.position = c(.264, .9), 
        legend.direction = "horizontal",legend.background = element_rect(fill="transparent")) +
  ggtitle("Spec.: 0.997") +
  coord_cartesian(xlim = c(ymd("2020-05-01"), ymd("2020-09-18")), ylim = c(0, 650))
  

# rt_1 = estimate_Rt(nc_res_1, data_date = data_date)
# rt_999 = estimate_Rt(nc_res_999, data_date = data_date)
# rt_997 = estimate_Rt(nc_res_997, data_date = data_date)
# rt_995 = estimate_Rt(nc_res_995, data_date = data_date)
# 
# save(rt_1, file = "../results/nowcast_bav/rt_1.RData")
# save(rt_999, file = "../results/nowcast_bav/rt_0999.RData")
# save(rt_997, file = "../results/nowcast_bav/rt_0997.RData")
# save(rt_995, file = "../results/nowcast_bav/rt_0995.RData")
# Load and Plot estimated R(t) based on different assumptions wrt to specificity
load("../results/nowcast_bav/rt_1.RData")
load("../results/nowcast_bav/rt_0999.RData")
load("../results/nowcast_bav/rt_0997.RData")
load("../results/nowcast_bav/rt_0995.RData")

fig_5_3 = rbind(rt_1 %>% mutate(spec= "Spec: 1"),
      rt_999 %>% mutate(spec="Spec: 0.999"),
      rt_997 %>% mutate(spec="Spec: 0.997"),
      rt_995 %>% mutate(spec="Spec: 0.995")) %>%
  mutate(spec=factor(spec, levels = c("Spec: 1", "Spec: 0.999", "Spec: 0.997", "Spec: 0.995"),
                     labels = c("Unadjusted", "Spec: 0.999", "Spec: 0.997", "Spec: 0.995"))) %>%
  dplyr::filter(Date>=lubridate::ymd("2020-05-10")) %>%
  ggplot() +
  geom_line(aes(Date, Rt, col = spec)) +
  geom_ribbon(aes(Date, ymin=Rt_lower, ymax=Rt_upper, fill = spec), alpha = .1, lwd = 0) +
  theme_bw() +
  geom_hline(aes(yintercept=1), lty=2) +
  theme(legend.title = element_blank(), legend.position = c(.3, .9), 
        legend.direction = "horizontal", 
        legend.background = element_rect(fill="transparent")) +
  scale_color_manual(values = c("Unadjusted"="black", 
                                "Spec: 0.999" = 2, 
                                "Spec: 0.997" = 3, 
                                "Spec: 0.995" = 4)) +
  scale_fill_manual(values = c("Unadjusted"="black", 
                                "Spec: 0.999" = 2, 
                                 "Spec: 0.997" = 3, 
                                "Spec: 0.995" = 4)) +
  ylab("\nR(t)") +
  coord_cartesian(xlim = c(ymd("2020-05-01"), ymd("2020-09-18"))) +
  ggtitle("Sens. constant")
# Combine plots
fig_5 = ggarrange(fig_5_1, fig_5_2, fig_5_3, nrow = 3, labels = "AUTO", align = "hv")
ggsave(fig_5, file = "../results/figures/fig_5_nc.pdf", width = 8, height = 10, dpi = 500)
