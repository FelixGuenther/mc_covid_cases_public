library(tidyverse)
dat = read_tsv("../data/nowcast_curr/nowcasting_results_2021-01-21.csv") %>%
  mutate(spec="Spec: 1", sens= "Sens: 1") %>%
  rbind(read_tsv("../data/nowcast_curr/nowcasting_results_spec_0999_2021-01-21.csv") %>%
          mutate(spec="Spec: 0.999", sens="Sens: 1")) %>%
  rbind(read_tsv("../data/nowcast_curr/nowcasting_results_spec_0997_2021-01-21.csv") %>%
          mutate(spec="Spec: 0.997", sens="Sens: 1")) %>%
  rbind(read_tsv("../data/nowcast_curr/nowcasting_results_spec_0995_2021-01-21.csv") %>%
          mutate(spec="Spec: 0.995", sens="Sens: 1")) %>% as_tibble()

dat = dat %>% 
  rbind(dat %>% filter(spec=="Spec: 0.999", sens=="Sens: 1") %>% 
          mutate(nowcast_med = (nowcast_med*0.999)/(0.9+0.999-1),
                 nowcast_lwr = (nowcast_lwr*0.999)/(0.9+0.999-1),
                 nowcast_upr = (nowcast_upr*0.999)/(0.9+0.999-1),
                 sens="Sens: 0.9")) %>%
  rbind(dat %>% filter(spec=="Spec: 0.999", sens=="Sens: 1") %>% 
          mutate(nowcast_med = (nowcast_med*0.999)/(0.7+0.999-1),
                 nowcast_lwr = (nowcast_lwr*0.999)/(0.7+0.999-1),
                 nowcast_upr = (nowcast_upr*0.999)/(0.7+0.999-1),
                 sens="Sens: 0.7")) %>%
  rbind(dat %>% filter(spec=="Spec: 0.997", sens=="Sens: 1") %>% 
          mutate(nowcast_med = (nowcast_med*0.997)/(0.9+0.997-1),
                 nowcast_lwr = (nowcast_lwr*0.997)/(0.9+0.997-1),
                 nowcast_upr = (nowcast_upr*0.997)/(0.9+0.997-1),
                 sens="Sens: 0.9")) %>%
  rbind(dat %>% filter(spec=="Spec: 0.997", sens=="Sens: 1") %>% 
          mutate(nowcast_med = (nowcast_med*0.997)/(0.7+0.997-1),
                 nowcast_lwr = (nowcast_lwr*0.997)/(0.7+0.997-1),
                 nowcast_upr = (nowcast_upr*0.997)/(0.7+0.997-1),
                 sens="Sens: 0.7")) %>%
  rbind(dat %>% filter(spec=="Spec: 0.995", sens=="Sens: 1") %>% 
          mutate(nowcast_med = (nowcast_med*0.995)/(0.9+0.995-1),
                 nowcast_lwr = (nowcast_lwr*0.995)/(0.9+0.995-1),
                 nowcast_upr = (nowcast_upr*0.995)/(0.9+0.995-1),
                 sens="Sens: 0.9")) %>%
  rbind(dat %>% filter(spec=="Spec: 0.995", sens=="Sens: 1") %>% 
          mutate(nowcast_med = (nowcast_med*0.995)/(0.7+0.995-1),
                 nowcast_lwr = (nowcast_lwr*0.995)/(0.7+0.995-1),
                 nowcast_upr = (nowcast_upr*0.995)/(0.7+0.995-1),
                 sens="Sens: 0.7")) %>%
  mutate(sens=factor(sens, levels = c("Sens: 1", "Sens: 0.9", "Sens: 0.7")),
         spec=factor(spec, levels = c("Spec: 1", "Spec: 0.999", "Spec: 0.997", "Spec: 0.995")))


supp_fig_2 = ggplot(dat %>% filter(date>=lubridate::ymd("2020-08-01"),
                                   sens == "Sens: 0.9" & spec %in% c("Spec: 0.999", "Spec: 0.997", "Spec: 0.995") | 
                                     sens == "Sens: 0.7" & spec %in% c("Spec: 0.999", "Spec: 0.997", "Spec: 0.995")) %>%
                      rbind(dat %>% 
                              filter(date>=lubridate::ymd("2020-08-01"),
                                     sens == "Sens: 1", spec=="Spec: 1") %>%
                              mutate(sens = "Sens: 0.9", spec = "Unadjusted")) %>% 
                      rbind(dat %>% 
                              filter(date>=lubridate::ymd("2020-08-01"), 
                                     sens == "Sens: 1", spec=="Spec: 1") %>%
                              mutate(sens = "Sens: 0.7", spec = "Unadjusted"))) +
  geom_col(aes(date, reported), data = dat %>%
             filter(sens== "Sens: 1",
                    spec=="Spec: 1",
                    date >= lubridate::ymd("2020-08-01")) %>%
             select(date, reported), fill = "lightgrey") +
  geom_line(aes(date, nowcast_med, col = spec)) +
  geom_ribbon(aes(date, ymin=nowcast_lwr, ymax=nowcast_upr, fill = spec), alpha = 0.2, lwd = .2) +
  facet_wrap(.~sens, nrow=3, ) + 
  theme_light() +
  theme(legend.position = "bottom") +
  ylab("Number onsets") + xlab("Date") +
  guides(col=guide_legend(title="")) +
  scale_color_manual(values = c("Unadjusted"=1,
                                "Spec: 0.999"=3,
                                "Spec: 0.997"=5,
                                "Spec: 0.995"=6)) +
  scale_fill_manual(values = c("Unadjusted"=1,
                               "Spec: 0.999"=3,
                               "Spec: 0.997"=5,
                               "Spec: 0.995"=6)) +
  scale_linetype_manual(values = c("Unadjusted"=2,
                                   "Spec: 0.999"=1,
                                   "Spec: 0.997"=1,
                                   "Spec: 0.995"=1), guide=FALSE) +
  guides(fill="none")
ggsave(supp_fig_2, filename = "../results/figures/supp_fig_2.png", width = 7, height = 6)


