library(tidyverse)
library(ggpubr)
# Results of synthetic data 
load("../results/nowcast_bav/nowcast_synth.RData")
nc_res_1 = nc_smry
load("../results/nowcast_bav/nowcast_999_synth.RData")
nc_res_999 = nc_smry
load("../results/nowcast_bav/nowcast_997_synth.RData")
nc_res_997 = nc_smry
load("../results/nowcast_bav/nowcast_995_synth.RData")
nc_res_995 = nc_smry

colvec = RColorBrewer::brewer.pal(n = 6, "Set2")
# Plot nowcast results for different assumptions wrt specificity, sensitivity=1
fig_5_1 = rbind(nc_res_1$ntInf %>% mutate(spec= "Spec: 1"),
                nc_res_999$ntInf %>% mutate(spec= "Spec: 0.999"),
                nc_res_997$ntInf %>% mutate(spec= "Spec: 0.997"),
                nc_res_995$ntInf %>% mutate(spec= "Spec: 0.995")) %>%
  mutate(spec=factor(spec, levels = c("Spec: 1", "Spec: 0.999", "Spec: 0.997", "Spec: 0.995"),
                     labels = c("Unadjusted", "Spec: 0.999", "Spec: 0.997", "Spec: 0.995"))) %>%
  ggplot() +
  geom_col(aes(date, observed), data = nc_res_1$ntInf, fill = "lightgrey") +
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
fig_5_2 = rbind(nc_res_1$ntInf %>% mutate(spec = "Unadjusted"),
                nc_res_997$ntInf %>% mutate(spec = "Sens: 1"),
                nc_res_997$ntInf %>% 
                  mutate(med=med*0.997/(0.9+0.997-1),
                         q025=q025*0.997/(0.9+0.997-1),
                         q975=q975*0.997/(0.9+0.997-1),
                         spec= "Sens: 0.9"),
                nc_res_997$ntInf %>% 
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
  geom_col(aes(date, observed), data = nc_res_1$ntInf, fill = "lightgrey") +
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

# Load Estimated R(t)
load("../results/nowcast_bav/rt_1_synth.RData")
load("../results/nowcast_bav/rt_0999_synth.RData")
load("../results/nowcast_bav/rt_0997_synth.RData")
load("../results/nowcast_bav/rt_0995_synth.RData")

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
ggsave(fig_5, file = "../results/figures/fig_5_nc_synth.pdf", width = 8, height = 10, dpi = 500)


# Results of original data (based on saved intermediate results)
load("../results/nowcast_bav/nowcast.RData")
nc_res_1 = nc_smry
load("../results/nowcast_bav/nowcast_999.RData")
nc_res_999 = nc_smry
load("../results/nowcast_bav/nowcast_997.RData")
nc_res_997 = nc_smry
load("../results/nowcast_bav/nowcast_995.RData")
nc_res_995 = nc_smry

colvec = RColorBrewer::brewer.pal(n = 6, "Set2")
# Plot nowcast results for different assumptions wrt specificity, sensitivity=1
fig_5_1 = rbind(nc_res_1$ntInf %>% mutate(spec= "Spec: 1"),
                nc_res_999$ntInf %>% mutate(spec= "Spec: 0.999"),
                nc_res_997$ntInf %>% mutate(spec= "Spec: 0.997"),
                nc_res_995$ntInf %>% mutate(spec= "Spec: 0.995")) %>%
  mutate(spec=factor(spec, levels = c("Spec: 1", "Spec: 0.999", "Spec: 0.997", "Spec: 0.995"),
                     labels = c("Unadjusted", "Spec: 0.999", "Spec: 0.997", "Spec: 0.995"))) %>%
  ggplot() +
  geom_col(aes(date, observed), data = nc_res_1$ntInf, fill = "lightgrey") +
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
fig_5_2 = rbind(nc_res_1$ntInf %>% mutate(spec = "Unadjusted"),
                nc_res_997$ntInf %>% mutate(spec = "Sens: 1"),
                nc_res_997$ntInf %>% 
                  mutate(med=med*0.997/(0.9+0.997-1),
                         q025=q025*0.997/(0.9+0.997-1),
                         q975=q975*0.997/(0.9+0.997-1),
                         spec= "Sens: 0.9"),
                nc_res_997$ntInf %>% 
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
  geom_col(aes(date, observed), data = nc_res_1$ntInf, fill = "lightgrey") +
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

# Load Estimated R(t)
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
