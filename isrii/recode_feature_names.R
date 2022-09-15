vi_xgb %>% 
  pull(Variable)

vi_final <- vi_xgb %>% 
  mutate(Variable = str_remove_all(Variable, "l0."),
         Variable = str_replace_all(Variable, "p48.rratecount.count.lapse", "lapse.48.count_raw"),
         Variable = str_replace_all(Variable, "p24.dratecount.count.lapse", "lapse.24.count_chng"),
         Variable = str_replace_all(Variable, "p168.dratecount.count.lapse", "lapse.168.count_chng"),
         Variable = str_replace_all(Variable, "p48.dratecount.count.lapse", "lapse.48.count_chng"),
         Variable = str_replace_all(Variable, "p48.rratecount.count.lapse", "lapse.48.count_raw"),
         Variable = str_replace_all(Variable, "p24.rratecount.count.lapse", "lapse.24.count_raw"),
         Variable = str_replace_all(Variable, "p12.dratecount.count.lapse", "lapse.12.count_chng"),
         Variable = str_replace_all(Variable, "p72.rratecount.count.lapse", "lapse.72.count_raw"),
         Variable = str_replace_all(Variable, "p168.rratecount.count.lapse", "lapse.168.count_raw"),
         Variable = str_replace_all(Variable, "p0", "0"),
         Variable = str_replace_all(Variable, "p12", "12"),
         Variable = str_replace_all(Variable, "p24", "24"),
         Variable = str_replace_all(Variable, "p48", "48"),
         Variable = str_replace_all(Variable, "p72", "72"),
         Variable = str_replace_all(Variable, "p168", "168"),
         Variable = str_replace_all(Variable, "dmax_response", "max_chng"),
         Variable = str_replace_all(Variable, "rmax_response", "max_raw"),
         Variable = str_replace_all(Variable, "dmin_response", "min_chng"),
         Variable = str_replace_all(Variable, "rmin_response", "min_raw"),
         Variable = str_replace_all(Variable, "rmedian_response", "median_raw"),
         Variable = str_replace_all(Variable, "dmedian_response", "median_chng"),
         Variable = str_replace_all(Variable, "drecent_response", "chng"),
         Variable = str_replace_all(Variable, "rrecent_response", "raw"),
         Variable = str_replace_all(Variable, "ema_2", "crave"),
         Variable = str_replace_all(Variable, "ema_3", "risky_situation"),
         Variable = str_replace_all(Variable, "ema_4", "stressful_event"),
         Variable = str_replace_all(Variable, "ema_5", "pleasant_event"),
         Variable = str_replace_all(Variable, "ema_6", "affect_valence"),
         Variable = str_replace_all(Variable, "ema_7", "affect_arousal"),
         Variable = str_replace_all(Variable, "ema_8", "risky_situation_future"),
         Variable = str_replace_all(Variable, "ema_9", "stressful_event_future"),
         Variable = str_replace_all(Variable, "ema_10", "drink_future"))


vi_final %>%
  vip(num_features = 40, geom = "point") +
  theme(axis.text = element_text(size = rel(1.50)), 
        axis.title = element_text(size = rel(1.75)))

ggsave(file.choose(), width = 9, height = 7.5, units = "in", device = "png",  dpi = 100)
