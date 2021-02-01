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
# Load helper functions
source("./functions.R")

col_vec = brewer.pal(3, "Dark2")
## Read Edit Data
# Bavaria
# Daily PCR-test data
load("../data/200921_test_bav.RData") 
dat_test_bav = dat_test_bav %>% filter(date>=ymd("2020-03-16"))
# Person-specific case data
dat_cases_ind_bav = read_tsv("../data/2020_09_21_bav_synth.csv")
# Derive daily case numbers
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

# Adjust case numbersfor differnt assumptions on Sensitivity and Specificity
# Prepare dataset for adjustment
adj_case_ger = expand_grid(date=mod_ger[[2]]$date, sens=c(1,.9,.7), spec=c(1,.999,.997,.995)) %>%
  left_join(mod_ger[[2]] %>% dplyr::select(date, n_cases, n_exam_pers))

adj_case_bav = expand_grid(date=mod_bav[[2]]$date, sens=c(1,.9,.7), spec=c(1,.999,.997,.995)) %>%
  left_join(mod_bav[[2]] %>% dplyr::select(date, n_cases, n_exam_pers))

# Use helper-function to derive misclassification-adjusted case counts
adj_case_ger = adj_case_ger %>% 
  mutate(n_cases_adj = adjust_n_case(n_cases, n_exam_pers, sens = sens, spec=spec)) %>%
  mutate(sens = factor(sens, levels = c("1","0.9", "0.7"),
                       labels = paste0("Sens: ", c("1","0.9", "0.7"))),
         spec = factor(spec, levels = c("1", "0.999", "0.997", "0.995"),
                       labels = paste0("Spec: ", c("1", "0.999", "0.997", "0.995"))))
adj_case_bav = adj_case_bav %>% 
  mutate(n_cases_adj = adjust_n_case(n_cases, n_exam_pers, sens = sens, spec=spec)) %>%
  mutate(sens = factor(sens, levels = c("1","0.9", "0.7"),
                       labels = paste0("Sens: ", c("1","0.9", "0.7"))),
         spec = factor(spec, levels = c("1", "0.999", "0.997", "0.995"),
                       labels = paste0("Spec: ", c("1", "0.999", "0.997", "0.995"))))

# Plot results
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

# Figure 4 - plot fraction of false-positives of all reported cases for different values of specificity
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
# (Numbers do not match numbers in original data due to artificial reporting dates)
adj_case_bav %>% filter(spec=="Spec: 0.995",
                        sens=="Sens: 1",
                        date<ymd("2020-08-01"),
                        date>=ymd("2020-06-01")) %>% 
  summarise(min_date = min(date),
            max_date = max(date),
            n=n(),
            abs_zero = sum(n_cases_adj==0),
            frac_zero = mean(n_cases_adj==0))
# Fraction of positive PCR-tests reported by labs in June/July
case_test_dat_bav %>% filter(date>=ymd("2020-06-01"), date < ymd("2020-08-01")) %>% summarise(sum(n_pcr_pos)/sum(n_pcr))

# Prepare data for misclassification adjusted nowcasting
source("analysis_fun_stanmodel_mc.R")
# Perform imputation of missing disease onsets based on Weibull-GAMLSS
imputation = perform_imputation(dat_cases_ind_bav, type = "week_weekday_age")
imputed_data = imputation[[1]]

# Adjust case numbers for false-positive cases assuming spec < 1
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

save(imputed_dat_adj_999, file = "../results/nowcast_bav/imp_dat_adj_0999_synth.RData")
save(imputed_dat_adj_997, file = "../results/nowcast_bav/imp_dat_adj_0997_synth.RData")
save(imputed_dat_adj_995, file = "../results/nowcast_bav/imp_dat_adj_0995_synth.RData")
save(imputed_data, file = "../results/nowcast_bav/imp_dat_synth.RData")