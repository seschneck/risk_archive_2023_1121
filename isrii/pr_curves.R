# pr_1week <- pr_data
# pr_1day <- pr_data
# pr_1hour <- pr_data



pr_1week <- pr_1week %>% 
  mutate(model = "1week")

pr_1day <- pr_1day %>% 
  mutate(model = "1day")


pr_1hour <- pr_1hour %>% 
  mutate(model = "1hour")


pr_data <- pr_1hour %>% 
  rbind(pr_1day) %>% 
  rbind(pr_1week)


model_colors <- c("1week" = "purple", "1day"="blue", "1hour"="orange")

plot_theme = theme(
  axis.text = element_text(size = rel(1.50)), 
  axis.title = element_text(size = rel(1.75)))


# week only
pr_data %>% 
  filter(model == "1week") %>% 
  ggplot(aes(x = recall, y = precision, color = model)) +
  geom_path(size = 1.25) +
#  geom_hline(lty = 3, yintercept = mean(preds_best$truth == "lapse")) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Sensitivity (Recall)",
       y = "Positive Predictive Value (Precision)") +
  scale_color_manual(values = model_colors)  +
  plot_theme


ggsave(file.choose(), width = 6.5, height = 6.5, units = "in", device = "png",  dpi = 100)


pr_data %>% 
  filter(model == "1week" | model == "1day") %>% 
  ggplot(aes(x = recall, y = precision, color = model)) +
  geom_path(size = 1.25) +
  # geom_abline(lty = 3) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Sensitivity (Recall)",
       y = "Positive Predictive Value (Precision)") +
  # scale_x_continuous(breaks = seq(0,1,.20),
  #                    labels = sprintf("%0.2f", seq(0,1,.20))) +
  scale_color_manual(values = model_colors) +
  plot_theme


ggsave(file.choose(), width = 6.5, height = 6.5, units = "in", device = "png",  dpi = 100)


pr_data %>% 
  ggplot(aes(x = recall, y = precision, color = model)) +
  geom_path(size = 1.25) +
  # geom_abline(lty = 3) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Sensitivity (Recall)",
       y = "Positive Predictive Value (Precision)") +
  # scale_x_continuous(breaks = seq(0,1,.20),
  #                    labels = sprintf("%0.2f", seq(0,1,.20))) +
  scale_color_manual(values = model_colors) +
  plot_theme


ggsave(file.choose(), width = 6.5, height = 6.5, units = "in", device = "png",  dpi = 100)



# get threshold and sensitivity for minimum PPV
pr_data %>% 
  filter(precision > .750) %>% 
  arrange(desc(recall)) %>% 
  group_by(model) %>% 
  slice(1)

